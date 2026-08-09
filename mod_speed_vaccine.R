# ==============================================================================
# mod_speed_vaccine.R -- Tab 4: Speed Vaccine
#
# Logic: sprinting >=90% of individual Vmax is the hamstring's tetanus shot.
# Regular near-max exposure conditions the tissue for the eccentric demands
# of late-swing; long gaps between doses are when hamstrings get "caught out"
# in matches. Cadence target: at least one exposure every 5-7 days.
# ==============================================================================

mod_speed_vaccine_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(title = "Green (≤5 days)", value = textOutput(ns("n_green")),
                showcase = icon("circle-check"), theme = "success"),
      value_box(title = "Yellow (6-7 days)", value = textOutput(ns("n_yellow")),
                showcase = icon("triangle-exclamation"), theme = "warning"),
      value_box(title = "Red (>7 days)", value = textOutput(ns("n_red")),
                showcase = icon("circle-exclamation"), theme = "danger")
    ),
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Exposure status — days since ≥90% Vmax"),
        reactableOutput(ns("vaccine_table"))
      ),
      card(
        card_header("Needs speed exposure TODAY"),
        p(class = "text-muted small",
          "Prescribe during extended warm-up or post-session: 2-3 x 30-40 m
           build-to-fly sprints, full recovery (1 min per 10 m). Never chase
           Vmax under fatigue — that recreates the injury mechanism the
           vaccine is meant to prevent."),
        uiOutput(ns("needs_list"))
      )
    )
  )
}

mod_speed_vaccine_server <- function(id, vaccine) {
  moduleServer(id, function(input, output, session) {

    output$n_green  <- renderText(as.character(sum(vaccine()$status == "Green")))
    output$n_yellow <- renderText(as.character(sum(vaccine()$status == "Yellow")))
    output$n_red    <- renderText(as.character(sum(vaccine()$status == "Red")))

    output$vaccine_table <- renderReactable({
      tbl <- vaccine() |>
        transmute(
          Athlete = athlete_name, Group = position_group,
          `Vmax (m/s)` = vmax,
          `Last ≥90% exposure` = format(last_exposure, "%b %d"),
          `Days since` = days_since,
          `Best % (window)` = round(best_recent_pct, 1),
          `Doses (28d)` = exposures_28d,
          Status = as.character(status)
        )

      reactable(
        tbl, compact = TRUE, striped = TRUE, defaultPageSize = 20,
        columns = list(
          Status = colDef(cell = function(value) {
            col <- switch(value, Green = AMS_COLORS$green,
                          Yellow = AMS_COLORS$yellow, AMS_COLORS$red)
            status_badge(col, value)
          }),
          `Days since` = colDef(cell = function(value) {
            if (is.na(value)) "never" else as.character(value)
          })
        ),
        theme = ams_react_theme
      )
    })

    output$needs_list <- renderUI({
      need <- vaccine() |> filter(status %in% c("Red", "Yellow"))
      if (nrow(need) == 0) return(p("Squad fully vaccinated — maintain cadence."))
      tags$ul(
        lapply(seq_len(nrow(need)), function(i) {
          r <- need[i, ]
          tags$li(
            strong(r$athlete_name), " — ",
            if (is.na(r$days_since)) "no exposure in window"
            else sprintf("%d days since last dose", r$days_since),
            sprintf(" (target: ≥%.1f m/s)",
                    r$vmax * THRESHOLDS$vmax_pct)
          )
        })
      )
    })
  })
}
