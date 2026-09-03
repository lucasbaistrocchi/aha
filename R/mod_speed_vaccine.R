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
    card(
      card_header("Days since last ≥90% Vmax exposure"),
      p(class = "text-muted small mb-1",
        "Bars use the same traffic light as the table: green ≤5 days,
         gold 6-7, red >7 or never. Athletes with no exposure in the window
         are plotted at the top of the red band."),
      uiOutput(ns("vaccine_plot_ui"))
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

    # --- Colour-coded bars, same categorisation as the table ----------------
    output$vaccine_plot_ui <- renderUI({
      n <- tryCatch(nrow(vaccine()), error = function(e) 0)
      div(style = "max-height:620px;overflow-y:auto",
          plotlyOutput(session$ns("vaccine_plot"),
                       height = bar_chart_height(n, px_per_row = 20)))
    })

    output$vaccine_plot <- renderPlotly({
      v <- vaccine()
      validate(need(nrow(v) > 0, "No GPS data loaded."))

      # "Never exposed" has no numeric days-since; plot it just past the
      # worst real value so it reads as the most overdue, not as a gap.
      max_days <- suppressWarnings(max(v$days_since, na.rm = TRUE))
      if (!is.finite(max_days)) max_days <- THRESHOLDS$vaccine_yellow + 1
      never_at <- max_days + 2

      d <- v |>
        mutate(
          plot_days = if_else(is.na(days_since), never_at,
                              as.numeric(days_since)),
          bar_col = case_when(
            status == "Green"  ~ AMS_COLORS$green,
            status == "Yellow" ~ AMS_COLORS$gold,
            TRUE               ~ AMS_COLORS$red),
          lbl = if_else(is.na(days_since), "never",
                        paste0(days_since, "d"))
        ) |>
        arrange(plot_days)   # most recently exposed at the bottom

      plot_ly(d, x = ~plot_days,
              y = ~factor(athlete_name, levels = athlete_name),
              type = "bar", orientation = "h",
              marker = list(color = ~bar_col),
              text = ~lbl, textposition = "outside", cliponaxis = FALSE,
              hovertemplate = paste0("%{y}<br>%{text} since last dose",
                                     "<extra></extra>")) |>
        layout(
          xaxis = list(title = "Days since exposure",
                       range = c(0, never_at * 1.18), automargin = TRUE),
          yaxis = list(title = "", tickfont = list(size = 10),
                       automargin = TRUE),
          bargap = 0.25,
          showlegend = FALSE,
          # Boundaries of the traffic light, drawn where the categories change.
          shapes = list(
            list(type = "line", x0 = THRESHOLDS$vaccine_green + 0.5,
                 x1 = THRESHOLDS$vaccine_green + 0.5,
                 y0 = -0.5, y1 = nrow(d) - 0.5,
                 line = list(dash = "dot", color = AMS_COLORS$gold,
                             width = 1.5)),
            list(type = "line", x0 = THRESHOLDS$vaccine_yellow + 0.5,
                 x1 = THRESHOLDS$vaccine_yellow + 0.5,
                 y0 = -0.5, y1 = nrow(d) - 0.5,
                 line = list(dash = "dot", color = AMS_COLORS$red,
                             width = 1.5)))
        ) |>
        ams_plotly_layout("Speed vaccine — dotted lines mark 5 and 7 days",
                          margin_l = 150)
    })

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
