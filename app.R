ial: with fill behaviour on, every card and
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
    app_data <- reactive({
      invalidateLater(15 * 60 * 1000, session)
      load_all_data(weeks = 10)
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
    mod_speed_vaccine_server("vaccine", vaccine_status)
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
