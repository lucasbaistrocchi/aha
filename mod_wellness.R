# ==============================================================================
# mod_wellness.R -- Tab 8: Full Wellness Dashboard
#
# Live Form metrics: Sleep Quantity (h), Sleep Quality (1-10), Energy (1-10),
# Soreness (1-10, higher = worse), Stress (1-10, higher = worse), plus
# ache/pain reports. Composite readiness inverts the negative items; z-scores
# are within-athlete over a rolling 21-day window.
# ==============================================================================

mod_wellness_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 9),
      card(
        card_header("Controls"),
        selectInput(ns("athlete"), "Athlete trend", choices = NULL),
        dateInput(ns("day"), "Squad snapshot date", value = Sys.Date()),
        helpText("Z-scores compare each athlete against their OWN prior
                  21-day window — deviation from personal normal, not from
                  the squad. Soreness & stress are 'higher = worse'; cell
                  colors account for direction.")
      ),
      card(
        card_header("Squad snapshot — daily wellness + readiness"),
        reactableOutput(ns("squad_table"))
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Individual readiness trend (21-day)"),
        plotlyOutput(ns("trend_plot"), height = "340px")
      ),
      card(
        card_header("Yesterday's PlayerLoad vs Today's Readiness"),
        plotlyOutput(ns("load_response_plot"), height = "340px")
      )
    )
  )
}

mod_wellness_server <- function(id, data, wellness_scored) {
  moduleServer(id, function(input, output, session) {

    observeEvent(data(), {
      updateSelectInput(session, "athlete",
                        choices = sort(unique(data()$wellness$athlete_name)))
      updateDateInput(session, "day", value = max(data()$wellness$date),
                      min = min(data()$wellness$date),
                      max = max(data()$wellness$date))
    })

    output$squad_table <- renderReactable({
      req(input$day)
      snap <- wellness_scored() |>
        filter(date == input$day) |>
        transmute(Athlete = athlete_name,
                  `Sleep (h)` = sleep_quantity,
                  Quality = sleep_quality, Energy = energy,
                  Soreness = soreness, Stress = stress,
                  Reported = if_else(injury_flag, aches, "—"),
                  `Readiness %` = round(readiness),
                  `Z (21d)` = round(readiness_z, 2)) |>
        arrange(`Z (21d)`)

      validate(need(nrow(snap) > 0, "No submissions for this date."))

      # Direction-aware dark-mode washes: lime = good, gold = middling,
      # red = concern.
      wash <- list(good = "rgba(170,255,0,0.18)",
                   mid  = "rgba(212,175,55,0.25)",
                   bad  = "rgba(229,72,77,0.30)")
      pos_col <- colDef(style = function(value) {   # higher = better
        col <- if (is.na(value)) NULL
               else if (value >= 7) wash$good
               else if (value >= 4) wash$mid else wash$bad
        list(background = col)
      }, width = 84)
      neg_col <- colDef(style = function(value) {   # higher = worse
        col <- if (is.na(value)) NULL
               else if (value <= 3) wash$good
               else if (value <= 6) wash$mid else wash$bad
        list(background = col)
      }, width = 84)
      hrs_col <- colDef(style = function(value) {
        col <- if (is.na(value)) NULL
               else if (value >= 8) wash$good
               else if (value >= 6.5) wash$mid else wash$bad
        list(background = col)
      }, width = 90)

      reactable(
        snap, compact = TRUE, defaultPageSize = 30,
        columns = list(
          `Sleep (h)` = hrs_col,
          Quality = pos_col, Energy = pos_col,
          Soreness = neg_col, Stress = neg_col,
          Reported = colDef(style = function(value) {
            if (!is.null(value) && value != "—")
              list(color = AMS_COLORS$red, fontWeight = 700)
          }, minWidth = 120),
          `Z (21d)` = colDef(style = function(value) {
            list(fontWeight = 700,
                 color = if (!is.na(value) && value < THRESHOLDS$wellness_z_flag)
                   AMS_COLORS$red else NULL)
          })
        ),
        theme = ams_react_theme
      )
    })

    output$trend_plot <- renderPlotly({
      req(input$athlete)
      d <- wellness_scored() |>
        filter(athlete_name == input$athlete,
               date > max(date) - 21)
      validate(need(nrow(d) > 0, "No data for this athlete."))

      plot_ly(d, x = ~date) |>
        add_lines(y = ~readiness, name = "Readiness %",
                  line = list(color = AMS_COLORS$primary, width = 3)) |>
        add_markers(y = ~readiness, showlegend = FALSE,
                    marker = list(
                      size = 9,
                      color = ~if_else(z_flag, AMS_COLORS$red,
                                       AMS_COLORS$primary))) |>
        layout(yaxis = list(title = "Readiness %", range = c(0, 100),
                            automargin = TRUE),
               xaxis = list(title = "", automargin = TRUE)) |>
        ams_plotly_layout(paste("Readiness —", input$athlete,
                                "(red dots = z < -1.5)"),
                          hovermode = "x unified")
    })

    # Dose-response: high load yesterday + low readiness today (bottom-right
    # quadrant) = poor tolerance -> candidates for modification. Note: only
    # athletes present in BOTH the GPS roster and the Form appear with a
    # non-zero x value.
    output$load_response_plot <- renderPlotly({
      req(input$day)
      # PlayerLoad when the source provides it, distance otherwise.
      load_col <- if (isTRUE(data()$has_load)) "player_load" else "distance"
      load_lbl <- if (load_col == "player_load")
        "Yesterday's PlayerLoad (AU)" else "Yesterday's Distance (m)"
      yesterday_load <- data()$gps |>
        filter(date == input$day - 1) |>
        group_by(athlete_id) |>
        summarise(pl_yday = sum(.data[[load_col]], na.rm = TRUE),
                  .groups = "drop")

      d <- wellness_scored() |>
        filter(date == input$day) |>
        left_join(yesterday_load, by = "athlete_id") |>
        mutate(pl_yday = coalesce(pl_yday, 0))

      validate(need(nrow(d) > 0, "No submissions for this date."))

      plot_ly(d, x = ~pl_yday, y = ~readiness, type = "scatter",
              mode = "markers",
              text = ~athlete_name, hoverinfo = "text+x+y",
              marker = list(
                size = 11,
                color = ~if_else(z_flag, AMS_COLORS$red, AMS_COLORS$primary),
                opacity = 0.85)) |>
        layout(
          xaxis = list(title = load_lbl, automargin = TRUE),
          yaxis = list(title = "Today's Readiness %", range = c(0, 100),
                       automargin = TRUE),
          shapes = list(list(
            type = "line", x0 = 0, x1 = max(d$pl_yday) * 1.05 + 1,
            y0 = 70, y1 = 70,
            line = list(dash = "dot", color = AMS_COLORS$grey)))
        ) |>
        ams_plotly_layout("Load tolerance map (bottom-right = watch closely)")
    })
  })
}
