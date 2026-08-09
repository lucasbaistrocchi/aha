# ==============================================================================
# mod_weekly_load.R -- Tab 2: Weekly Load vs Pre-Season Forecast
#
# No manual targets: forecasted weekly load per position group comes straight
# from the coaches' master database workbook --
#   forecast = Target Match Load x pre-season week multiplier.
# One gauge per position group (distance progress vs forecast), plus a full
# metric breakdown table and per-athlete detail.
# ==============================================================================

mod_weekly_load_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Pre-season forecast"),
        selectInput(ns("pre_week"), "Pre-season week",
                    choices = setNames(
                      seq_along(WEEK_MULTIPLIERS),
                      sprintf("Week %d  (%s, x%.1f)",
                              seq_along(WEEK_MULTIPLIERS),
                              format(PRESEASON_START +
                                       (seq_along(WEEK_MULTIPLIERS) - 1) * 7,
                                     "%b %d"),
                              WEEK_MULTIPLIERS))),
        uiOutput(ns("week_context")),
        p(class = "text-muted small",
          "Weeks run Monday-Sunday from the pre-season start
           (10 Aug 2026). Forecasted weekly load per athlete = Target Match
           Load x the week multiplier, from the master database workbook
           (data/2026-2027 LIFE U GPS Master Database.xlsx). Update the
           workbook and restart to change targets."),
        card_header("Forecast per athlete (this week)"),
        reactableOutput(ns("forecast_table"))
      ),
      tagList(
        card(
          card_header("Progress vs forecast — by position group"),
          uiOutput(ns("gauge_ui"))
        ),
        card(
          card_header("Group breakdown — accumulated vs forecast"),
          reactableOutput(ns("group_table"))
        )
      )
    ),
    card(
      card_header("Athlete detail — % of cohort forecast"),
      reactableOutput(ns("athlete_table"))
    )
  )
}

mod_weekly_load_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Default to the pre-season week that contains today (Mon-Sun weeks from
    # PRESEASON_START); clamped to the 6 programmed weeks.
    observeEvent(TRUE, once = TRUE, {
      wk <- as.integer(floor(as.numeric(
        floor_date(Sys.Date(), "week", week_start = 1) -
          PRESEASON_START) / 7)) + 1
      wk <- max(1, min(wk, length(WEEK_MULTIPLIERS)))
      updateSelectInput(session, "pre_week", selected = wk)
    })

    week_no <- reactive(as.integer(input$pre_week %||% 1))
    mult    <- reactive(WEEK_MULTIPLIERS[week_no()])

    output$week_context <- renderUI({
      wk_start <- PRESEASON_START + (week_no() - 1) * 7
      cur      <- floor_date(Sys.Date(), "week", week_start = 1)
      p(class = "small mb-2",
        style = paste0("color:", AMS_COLORS$primary, ";font-weight:600"),
        sprintf("%s - %s%s", format(wk_start, "%a %b %d"),
                format(wk_start + 6, "%a %b %d"),
                if (identical(as.Date(cur), as.Date(wk_start)))
                  "  (current week)" else ""))
    })

    group_progress <- reactive(
      compute_group_progress(data()$gps, week_no()))

    # Cohorts actually present in the current data.
    groups_in_data <- reactive({
      gp <- group_progress()
      as.character(gp$position_group)
    })

    output$forecast_table <- renderReactable({
      fc <- MATCH_BENCHMARKS |>
        filter(position_group %in% groups_in_data()) |>
        transmute(
          Cohort = position_group,
          `TD (m)` = round(bm_distance * mult()),
          `HMLD (m)` = round(bm_hmld * mult()),
          `HSR (m)` = round(bm_hsr * mult()),
          `A+D (n)` = round(bm_ad * mult())
        )
      if (!isTRUE(data()$has_hmld)) fc <- select(fc, -`HMLD (m)`)
      reactable(fc, compact = TRUE, defaultPageSize = 8,
                defaultColDef = colDef(format = colFormat(separators = TRUE)),
                theme = ams_react_theme)
    })

    # --- One gauge per position group (distance vs forecast) -----------------
    output$gauge_ui <- renderUI({
      n <- length(groups_in_data())
      rows <- ceiling(max(1, n) / 4)
      # Extra per-row height keeps gauge titles clear of the row above.
      plotlyOutput(ns("gauge_plot"), height = paste0(rows * 250 + 20, "px"))
    })

    output$gauge_plot <- renderPlotly({
      gp <- group_progress()
      n <- nrow(gp)
      validate(need(n > 0, "No GPS data loaded."))
      ncols <- min(n, 4)
      nrows <- ceiling(n / ncols)

      p <- plot_ly()
      for (i in seq_len(n)) {
        p <- p |> add_trace(
          type = "indicator", mode = "gauge+number",
          value = round(min(150, gp$pct_distance[i])),
          number = list(suffix = "%", font = list(color = AMS_COLORS$ink)),
          title = list(text = as.character(gp$position_group[i]),
                       font = list(size = 14, color = AMS_COLORS$ink)),
          domain = list(row = (i - 1) %/% ncols, column = (i - 1) %% ncols),
          gauge = list(
            axis = list(range = list(0, 150), tickcolor = AMS_COLORS$grey),
            bar = list(color = AMS_COLORS$primary),
            bgcolor = "#1A1A1A",
            borderwidth = 0,
            steps = list(
              list(range = c(0, 80),    color = "rgba(212,175,55,0.20)"),
              list(range = c(80, 110),  color = "rgba(170,255,0,0.15)"),
              list(range = c(110, 150), color = "rgba(229,72,77,0.25)")
            )
          )
        )
      }
      p |> layout(grid = list(rows = nrows, columns = ncols),
                  margin = list(t = 40, b = 10),
                  font = list(family = "Rajdhani", color = AMS_COLORS$ink),
                  paper_bgcolor = "rgba(0,0,0,0)") |>
        config(displaylogo = FALSE)
    })

    output$group_table <- renderReactable({
      gp <- group_progress() |>
        transmute(
          Cohort = as.character(position_group),
          Athletes = n_athletes,
          `TD %` = pct_distance,   `TD rem (m)` = round(rem_distance),
          `HSR %` = pct_hsr,       `HSR rem (m)` = round(rem_hsr),
          `A+D %` = pct_ad,        `A+D rem (n)` = round(rem_ad),
          `HMLD %` = pct_hmld,     `HMLD rem (m)` = round(rem_hmld)
        )
      if (!isTRUE(data()$has_hmld)) gp <- select(gp, -starts_with("HMLD"))

      pct_col <- colDef(
        cell = function(value) sprintf("%.0f%%", value),
        style = function(value) {
          col <- if (is.na(value) || value < 70) AMS_COLORS$red
                 else if (value < 90) AMS_COLORS$gold
                 else AMS_COLORS$primary
          list(color = col, fontWeight = 700)
        }, width = 80)

      cols <- list(`TD %` = pct_col, `HSR %` = pct_col,
                   `A+D %` = pct_col, `HMLD %` = pct_col)
      reactable(
        gp, compact = TRUE, striped = TRUE, defaultPageSize = 8,
        defaultColDef = colDef(format = colFormat(separators = TRUE)),
        columns = cols[names(cols) %in% names(gp)],
        theme = ams_react_theme
      )
    })

    output$athlete_table <- renderReactable({
      win <- preseason_week_window(week_no())
      wd <- data()$gps |>
        filter(date >= win[1], date <= win[2]) |>
        group_by(athlete_id, athlete_name, position_group) |>
        summarise(distance = sum(distance, na.rm = TRUE),
                  hsr = sum(hsr_distance, na.rm = TRUE),
                  ad = sum(accels, na.rm = TRUE) + sum(decels, na.rm = TRUE),
                  hmld = sum(hmld, na.rm = TRUE),
                  .groups = "drop") |>
        left_join(MATCH_BENCHMARKS, by = "position_group") |>
        mutate(
          pct_td   = 100 * distance / (bm_distance * mult()),
          pct_hsr  = 100 * hsr / (bm_hsr * mult()),
          pct_ad   = 100 * ad / (bm_ad * mult()),
          pct_hmld = 100 * hmld / (bm_hmld * mult())
        ) |>
        arrange(pct_td) |>
        transmute(Athlete = athlete_name, Cohort = position_group,
                  `TD done (m)` = round(distance),
                  `TD %` = pct_td, `HSR %` = pct_hsr,
                  `A+D %` = pct_ad, `HMLD %` = pct_hmld)
      if (!isTRUE(data()$has_hmld)) wd <- select(wd, -`HMLD %`)
      validate(need(nrow(wd) > 0,
                    "No sessions recorded in this pre-season week yet."))

      pct_col <- colDef(
        cell = function(value) sprintf("%.0f%%", value),
        style = function(value) {
          col <- if (is.na(value) || value < 70) AMS_COLORS$red
                 else if (value < 90) AMS_COLORS$gold
                 else AMS_COLORS$primary
          list(color = col, fontWeight = 700)
        }, width = 84)

      cols <- list(`TD %` = pct_col, `HSR %` = pct_col,
                   `A+D %` = pct_col, `HMLD %` = pct_col)
      reactable(
        wd, compact = TRUE, striped = TRUE, defaultPageSize = 25,
        defaultColDef = colDef(format = colFormat(separators = TRUE)),
        columns = cols[names(cols) %in% names(wd)],
        theme = ams_react_theme
      )
    })

    week_no   # return reactive for cross-module use (Home briefing)
  })
}
