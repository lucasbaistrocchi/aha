# ==============================================================================
# app.R -- Rugby Union Athlete Management System
# Run locally: shiny::runApp("rugby-ams")
#
# Environment variables (see DEPLOYMENT.md):
#   APP_PASSWORD              shared password for the login gate
#   GS4_SERVICE_ACCOUNT_JSON  service-account key (path or JSON text)
#   AVAILABILITY_SHEET_ID     sheet the availability board writes to
#   GPS_SHEET_ID / WELLNESS_SHEET_ID / TESTING_SHEET_ID  (optional overrides)
#   CATAPULT_API_TOKEN        optional; switches GPS to the OpenField API
# ==============================================================================

# ------------------------------------------------------------------------------
# Locate the app directory before anything else.
# A hosted deploy can start the R process at the REPOSITORY ROOT rather than
# the folder holding app.R (e.g. when the repo keeps everything inside a
# rugby-ams/ subfolder). If that happens, "R/" and "data/" resolve to nothing:
# no packages load, no modules exist, and the first error you see is a
# baffling `could not find function "page_fluid"`. So: find R/global.R, move
# there, and fail loudly with a useful message if it genuinely isn't present.
# ------------------------------------------------------------------------------
if (!file.exists(file.path("R", "global.R"))) {
  subdirs <- list.dirs(".", recursive = FALSE)
  hit <- subdirs[file.exists(file.path(subdirs, "R", "global.R"))]
  if (length(hit) >= 1) {
    setwd(hit[1])   # also makes data/ paths (roster, workbook) resolve
    message("app.R: switched working directory to ", normalizePath(getwd()))
  } else {
    # List what IS here -- a missing R/ folder is almost always an upload
    # that dropped nested directories, and seeing the actual contents
    # settles it immediately instead of guessing from the logs.
    here <- sort(list.files(".", all.files = TRUE, no.. = TRUE))
    stop("Cannot find R/global.R from '", normalizePath(getwd()), "'.\n",
         "Contents of that directory: ",
         if (length(here)) paste(here, collapse = ", ") else "(empty)", "\n",
         "Expected app.R alongside an R/ folder (14 .R files) and a data/ ",
         "folder. If R/ is absent, the upload dropped nested directories -- ",
         "re-upload the R and data folders to the repository.")
  }
}

# Source global.R FIRST -- it attaches every package. The rest of R/ runs
# top-level code (constant tables built with tribble(), etc.) that needs
# those packages already loaded. Sourcing the folder alphabetically would
# put data_sources.R ahead of global.R and fail on a clean machine.
source(file.path("R", "global.R"))
rest <- setdiff(list.files("R", full.names = TRUE, pattern = "\\.R$"),
                file.path("R", "global.R"))
for (f in rest) source(f)

app_password <- function() Sys.getenv("APP_PASSWORD", "")

# ------------------------------------------------------------------------------
# Main interface. navset_bar (not page_navbar) so it can be rendered *after*
# login -- keeping athlete medical data off the page until authenticated.
# ------------------------------------------------------------------------------
app_body <- function() {
  navset_bar(
    title = span(icon("shield-halved"), " Life University Rugby"),
    # fillable = FALSE is essential: with fill behaviour on, every card and
    # plot is compressed to share one viewport height instead of taking the
    # height it asks for, and the page collapses into an unreadable strip.
    fillable = FALSE,
    nav_panel("Home", icon = icon("house"), mod_home_ui("home")),
    nav_panel("Availability", icon = icon("user-check"),
              mod_availability_ui("availability")),
    nav_panel("Weekly Load", icon = icon("gauge-high"),
              mod_weekly_load_ui("weekly")),
    nav_panel("Longitudinal", icon = icon("chart-line"),
              mod_longitudinal_ui("longitudinal")),
    nav_panel("Speed Vaccine", icon = icon("bolt"),
              mod_speed_vaccine_ui("vaccine")),
    nav_panel("Positional", icon = icon("users"),
              mod_positional_ui("positional")),
    nav_panel("Match Minutes", icon = icon("stopwatch"),
              mod_match_minutes_ui("minutes")),
    nav_panel("Match Day", icon = icon("trophy"),
              mod_match_day_ui("matchday")),
    nav_panel("Wellness", icon = icon("bed"), mod_wellness_ui("wellness")),
    nav_panel("Testing", icon = icon("dumbbell"), mod_testing_ui("testing")),
    nav_panel("Individual Report", icon = icon("id-card"),
              mod_individual_ui("individual")),
    nav_spacer(),
    nav_item(uiOutput("data_status"))
  )
}

login_screen <- function(msg = NULL) {
  div(
    style = "max-width:400px;margin:12vh auto;",
    card(
      card_header("Life University Rugby — AMS"),
      p(class = "text-muted small",
        "This dashboard contains athlete medical and performance data.
         Enter the staff password to continue."),
      passwordInput("pw", NULL, placeholder = "Password"),
      actionButton("login", "Sign in", class = "btn-primary w-100"),
      if (!is.null(msg))
        p(class = "small mt-2 mb-0",
          style = paste0("color:", AMS_COLORS$red), msg)
    )
  )
}

# page_fluid (NOT page_fillable): content should flow down the page and
# scroll, not be squeezed to fit the window.
ui <- page_fluid(
  theme = ams_theme,
  tags$head(tags$title("Life University Rugby AMS")),
  uiOutput("shell")
)

server <- function(input, output, session) {

  # No password configured -> app is open. That's convenient locally and
  # dangerous when hosted, so an unmissable banner says so.
  authed <- reactiveVal(!nzchar(app_password()))
  login_msg <- reactiveVal(NULL)

  observeEvent(input$login, {
    if (identical(input$pw, app_password())) {
      authed(TRUE)
      login_msg(NULL)
    } else {
      login_msg("Incorrect password.")
    }
  })

  output$shell <- renderUI({
    if (!authed()) return(login_screen(login_msg()))
    tagList(
      if (!nzchar(app_password()))
        div(style = paste0("background:", AMS_COLORS$red,
                           ";color:white;padding:5px 12px;font-weight:600;",
                           "font-size:0.82rem;"),
            "No APP_PASSWORD set — this deployment is unprotected."),
      app_body()
    )
  })

  # Everything below is initialised ONCE, after login: modules populate their
  # dropdowns on startup, so they must not run before their UI exists.
  #
  # NB: a plain observeEvent(authed(), ..., once = TRUE) is a trap here. It
  # fires on the initial value too -- FALSE whenever a password is set -- and
  # `once` then destroys the observer before login ever happens, leaving every
  # output blank. An explicit latch is unambiguous.
  modules_started <- reactiveVal(FALSE)

  observe({
    req(authed(), !modules_started())
    modules_started(TRUE)

    # Single load at startup; refreshes every 15 min for live deployments.
    # Wrapped so a bad sheet read surfaces as a notification rather than
    # taking the R process down -- module observers call this, and an error
    # inside an observer disconnects every user.
    load_failed <- reactiveVal(NULL)
    app_data <- reactive({
      invalidateLater(15 * 60 * 1000, session)
      tryCatch({
        d <- load_all_data(weeks = 10)
        load_failed(NULL)
        d
      }, error = function(e) {
        load_failed(conditionMessage(e))
        # Demo data keeps the app usable and obviously-not-live (the navbar
        # badges read DEMO) instead of a blank disconnected screen.
        roster <- generate_dummy_roster()
        list(roster = roster,
             gps = derive_vmax(generate_dummy_gps(roster, 10)),
             wellness = generate_dummy_wellness(roster, 10),
             testing = tibble(athlete_name = character(),
                              test_date = as_date(character()),
                              metric = character(), value = numeric()),
             testing_metrics = character(),
             has_hmld = TRUE, has_load = TRUE,
             live = c(gps = FALSE, wellness = FALSE, testing = FALSE))
      })
    })

    observeEvent(load_failed(), {
      req(load_failed())
      showNotification(
        paste("Could not load live data — showing demo data instead.",
              "Detail:", load_failed()),
        type = "error", duration = NULL)
    })

    # Shared derived reactives -- computed once, consumed by several modules.
    wellness_scored <- reactive(compute_wellness_scores(app_data()$wellness))
    vaccine_status  <- reactive(compute_speed_vaccine(app_data()$gps))

    # Weekly module returns the selected pre-season week; the Home briefing
    # uses it so both screens report against the same forecast.
    pre_week <- mod_weekly_load_server("weekly", app_data)

    mod_home_server("home", app_data, wellness_scored, vaccine_status,
                    pre_week)
    mod_availability_server("availability", app_data,
                            wellness_scored, vaccine_status)
    mod_longitudinal_server("longitudinal", app_data)
    mod_speed_vaccine_server("vaccine", vaccine_status, app_data)
    mod_positional_server("positional", app_data)
    mod_match_minutes_server("minutes", app_data)
    mod_match_day_server("matchday", app_data)
    mod_wellness_server("wellness", app_data, wellness_scored)
    mod_testing_server("testing", app_data)
    mod_individual_server("individual", app_data, wellness_scored,
                          vaccine_status)

    output$data_status <- renderUI({
      live <- app_data()$live
      lbl <- function(ok, name) span(
        class = "badge me-1",
        style = paste0("background:",
                       if (isTRUE(ok)) AMS_COLORS$green else AMS_COLORS$grey),
        paste(name, if (isTRUE(ok)) "LIVE" else "DEMO"))
      div(class = "d-flex align-items-center px-2",
          lbl(live["gps"], "GPS"), lbl(live["wellness"], "Wellness"),
          lbl(live["testing"], "Testing"))
    })
  })
}

shinyApp(ui, server)
