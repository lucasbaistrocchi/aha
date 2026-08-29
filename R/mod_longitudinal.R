# ==============================================================================
# mod_longitudinal.R -- Tab 3: Longitudinal Tracker & Load Progression
# EWMA ACWR trends + week-over-week jump flags (see utils_metrics.R for math).
# ==============================================================================

mod_longitudinal_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 9),
      card(
        card_header("Controls"),
        selectInput(ns("athlete"), "Athlete", choices = NULL),
        selectInput(ns("load_metric"), "Load metric", choices = NULL),
        checkboxInput(ns("preseason"),
                      sprintf("Pre-season mode (flag >%d%% WoW jumps)",
                              round(THRESHOLDS$wow_jump_pct * 100)),
                      value = TRUE),
        helpText("ACWR shaded band = 0.8-1.5 'sweet spot'. Treat excursions
                  as conversation starters, not verdicts — context (travel,
                  academics, injury history) always outranks the ratio.")
      ),
      card(
        card_header("EWMA Acute:Chronic Workload"),
        plotlyOutput(ns("acwr_plot"), height = "420px")
      )
    ),
    card(
      card_header(uiOutput(ns("weekly_header"))),
      p(class = "text-muted small mb-1",
        "Monday-Sunday weeks, all sessions (training + match day).
         Change is measured against the immediately preceding week only —
         a dash means the previous week has no data, so no honest
         comparison exists."),
      reactableOutput(ns("athlete_weekly"))
    ),
    card(
      card_header("Week-over-week load progression (squad)"),
      reactableOutput(ns("wow_table"))
    )
  )
}

mod_longitudinal_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    observeEvent(data(), {
      updateSelectInput(session, "athlete",
                        choices = sort(unique(data()$gps$athlete_name)))
      # Only offer load metrics the current GPS source actually reports.
      metrics <- c("PlayerLoad" = "player_load", "Distance (m)" = "distance",
                   "HSR (m)" = "hsr_distance", "HMLD (m)" = "hmld")
      metrics <- metrics[vapply(metrics,
                                \(m) any(!is.na(data()$gps[[m]])),
                                logical(1))]
      updateSelectInput(session, "load_metric", choices = metrics)
    })

    acwr_data <- reactive({
      req(input$load_metric)
      compute_acwr(data()$gps, load_col = input$load_metric)
    })

    output$acwr_plot <- renderPlotly({
      req(input$athlete)
      d <- acwr_data() |> filter(athlete_name == input$athlete)

      plot_ly(d, x = ~date) |>
        # Sweet-spot band 0.8-1.5
        add_ribbons(ymin = THRESHOLDS$acwr_low, ymax = THRESHOLDS$acwr_high,
                    fillcolor = "rgba(46,139,87,0.10)",
                    line = list(width = 0), name = "0.8-1.5 band",
                    hoverinfo = "skip") |>
        add_bars(y = ~daily_load, name = "Daily load", yaxis = "y2",
                 marker = list(color = "rgba(138,147,165,0.45)")) |>
        add_lines(y = ~acwr, name = "EWMA ACWR",
                  line = list(color = AMS_COLORS$gold, width = 3)) |>
        layout(
          yaxis  = list(title = "ACWR", range = c(0, 2.2), overlaying = NULL,
                        automargin = TRUE),
          yaxis2 = list(title = "Daily load", side = "right",
                        overlaying = "y", showgrid = FALSE,
                        automargin = TRUE),
          # Below the plot: y = 1.12 sat on top of the title.
          legend = list(orientation = "h", y = -0.16, yanchor = "top", x = 0),
          xaxis  = list(title = "", automargin = TRUE)
        ) |>
        ams_plotly_layout(paste("ACWR —", input$athlete),
                          hovermode = "x unified", margin_b = 90,
                          margin_l = 64)
    })

    # --- Weekly breakdown for the selected athlete ---------------------------
    output$weekly_header <- renderUI({
      span(paste0("Weekly breakdown",
                  if (!is.null(input$athlete) && nzchar(input$athlete))
                    paste0(" — ", input$athlete) else ""))
    })

    output$athlete_weekly <- renderReactable({
      req(input$athlete)
      w <- compute_athlete_weekly(data()$gps, input$athlete)
      validate(need(nrow(w) > 0, "No sessions recorded for this athlete."))

      tbl <- w |>
        arrange(desc(week)) |>
        transmute(
          Week = format(week, "%b %d"),
          Sessions = sessions,
          `TD (m)` = round(td),          `TD Δ` = d_td,
          `HSR (m)` = round(hsr),        `HSR Δ` = d_hsr,
          `A+D` = round(ad),             `A+D Δ` = d_ad,
          `HMLD (m)` = round(hmld),      `HMLD Δ` = d_hmld
        )
      if (!isTRUE(data()$has_hmld))
        tbl <- select(tbl, -`HMLD (m)`, -`HMLD Δ`)

      # Spikes above the pre-season threshold read red; sharp drops of the
      # same size read gold (de-training / missed week is also worth seeing).
      lim <- THRESHOLDS$wow_jump_pct * 100
      delta_col <- colDef(
        cell = function(value) {
          if (is.na(value)) "—" else sprintf("%+.0f%%", value)
        },
        style = function(value) {
          if (is.na(value)) return(list(color = AMS_COLORS$grey))
          col <- if (value > lim) AMS_COLORS$red
                 else if (value < -lim) AMS_COLORS$gold
                 else AMS_COLORS$primary
          list(color = col, fontWeight = 700)
        },
        width = 84)

      cols <- list(
        Week = colDef(width = 88, style = list(fontWeight = 700)),
        Sessions = colDef(width = 84),
        `TD Δ` = delta_col, `HSR Δ` = delta_col,
        `A+D Δ` = delta_col, `HMLD Δ` = delta_col
      )
      reactable(
        tbl, compact = TRUE, striped = TRUE, defaultPageSize = 12,
        defaultColDef = colDef(format = colFormat(separators = TRUE)),
        columns = cols[names(cols) %in% names(tbl)],
        theme = ams_react_theme
      )
    })

    output$wow_table <- renderReactable({
      req(input$load_metric)
      preseason <- input$preseason   # reactive dep captured before cell fns
      wow <- compute_wow_change(data()$gps, load_col = input$load_metric) |>
        filter(week == max(week) | week == max(week) - 7) |>
        filter(week == max(week)) |>
        mutate(wow_pct = round(100 * wow_pct, 1)) |>
        arrange(desc(wow_pct)) |>
        select(Athlete = athlete_name, Group = position_group,
               `Weekly load` = weekly_load, `WoW %` = wow_pct,
               Flag = wow_flag)

      reactable(
        wow, compact = TRUE, striped = TRUE, defaultPageSize = 20,
        columns = list(
          `Weekly load` = colDef(format = colFormat(separators = TRUE)),
          `WoW %` = colDef(cell = function(value) {
            if (is.na(value)) "—" else sprintf("%+.1f%%", value)
          }),
          Flag = colDef(cell = function(value) {
            if (isTRUE(value) && isTRUE(preseason))
              status_badge(AMS_COLORS$red,
                           sprintf(">%d%% jump",
                                   round(THRESHOLDS$wow_jump_pct * 100)))
            else status_badge(AMS_COLORS$green, "OK")
          })
        ),
        theme = ams_react_theme
      )
    })
  })
}
