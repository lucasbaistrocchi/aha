# ==============================================================================
# mod_match_day.R -- Tab 7: Match Day Performance
#
# Two comparison frames, both minute-adjusted so a 25-min cameo is judged on
# intensity, not raw volume:
#   1. Athlete vs POSITION GROUP PEERS in the same match ("did he do his
#      positional job relative to the others doing the same job today?")
#   2. Athlete vs 80-min MATCH BENCHMARK ("was his per-minute output at
#      full-game standard?")
# Peer comparison catches team-wide context (slow game = everyone low);
# benchmark comparison catches standards drift (whole unit under-performing).
# ==============================================================================

mod_match_day_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 9),
      card(
        card_header("Match"),
        selectInput(ns("match_date"), "Match date", choices = NULL),
        selectInput(ns("athlete"), "Athlete", choices = NULL),
        helpText("All comparisons are per-minute (minute-adjusted), so
                  substitutes are judged on intensity of involvement, not
                  raw totals.")
      ),
      card(
        card_header("Squad — game output vs positional 80-min benchmark"),
        # Grouped bars for a 30+ man squad need horizontal room; scroll
        # rather than squeeze four series into a few pixels per athlete.
        uiOutput(ns("vs_bench_ui"))
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Individual — vs position group & match benchmark"),
        plotlyOutput(ns("indiv_plot"), height = "340px")
      ),
      card(
        card_header("Position group detail (per-minute rates)"),
        reactableOutput(ns("group_table"))
      )
    )
  )
}

mod_match_day_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    matches <- reactive(data()$gps |> filter(session_type == "match"))

    observeEvent(data(), {
      mm <- matches() |> distinct(date, opponent) |> arrange(desc(date))
      labs <- ifelse(is.na(mm$opponent), as.character(mm$date),
                     paste0(format(mm$date, "%b %d"), " · ", mm$opponent))
      updateSelectInput(session, "match_date",
                        choices = setNames(as.character(mm$date), labs))
      updateSelectInput(session, "athlete",
                        choices = sort(unique(matches()$athlete_name)))
    })

    match_day <- reactive({
      req(input$match_date)
      matches() |>
        filter(date == as_date(input$match_date), match_minutes > 0) |>
        mutate(ad = accels + coalesce(decels, 0))
    })

    # Benchmark-comparable metrics (benchmarks = Target Match Load workbook;
    # A+D = combined accel + decel efforts, matching the workbook's target).
    metric_defs <- reactive({
      defs <- tribble(
        ~key,           ~label,  ~bm_col,
        "distance",     "Dist",  "bm_distance",
        "hsr_distance", "HSR",   "bm_hsr",
        "hmld",         "HMLD",  "bm_hmld",
        "ad",           "A+D",   "bm_ad"
      )
      if (!isTRUE(data()$has_hmld)) defs <- filter(defs, key != "hmld")
      defs
    })

    # ---- Squad overview: everyone vs their minute-adjusted MDB --------------
    output$vs_bench_ui <- renderUI({
      n <- tryCatch(nrow(match_day()), error = function(e) 0)
      div(style = "overflow-x:auto",
          plotlyOutput(session$ns("vs_bench_plot"), height = "440px",
                       width = paste0(max(680, n * 46), "px")))
    })

    output$vs_bench_plot <- renderPlotly({
      d <- match_day() |>
        left_join(MATCH_BENCHMARKS, by = "position_group") |>
        mutate(
          min_frac = match_minutes / THRESHOLDS$match_full_min,
          pct_dist = 100 * distance / (bm_distance * min_frac),
          pct_hsr  = 100 * hsr_distance / (bm_hsr * min_frac),
          pct_hmld = 100 * hmld / (bm_hmld * min_frac),
          pct_ad   = 100 * ad / (bm_ad * min_frac)
        )
      validate(need(nrow(d) > 0, "No athletes recorded in this match."))

      p <- plot_ly(d, x = ~reorder(athlete_name, pct_dist)) |>
        add_bars(y = ~pct_dist, name = "Distance",
                 marker = list(color = AMS_COLORS$primary)) |>
        add_bars(y = ~pct_hsr, name = "HSR",
                 marker = list(color = AMS_COLORS$gold))
      if (isTRUE(data()$has_hmld))
        p <- p |> add_bars(y = ~pct_hmld, name = "HMLD",
                           marker = list(color = "#5A8F00"))
      p |>
        add_bars(y = ~pct_ad, name = "A+D",
                 marker = list(color = AMS_COLORS$grey)) |>
        layout(barmode = "group",
               # automargin + generous bottom room: names like
               # "Leo Keesler-Venables" get clipped otherwise.
               xaxis = list(title = "", tickangle = -45, automargin = TRUE,
                            tickfont = list(size = 10)),
               yaxis = list(title = "% of minute-adjusted MDB",
                            automargin = TRUE),
               legend = list(orientation = "h", y = -0.32, yanchor = "top",
                             x = 0),
               shapes = list(list(
                 type = "line", x0 = -0.5, x1 = nrow(d) - 0.5,
                 y0 = 100, y1 = 100,
                 line = list(dash = "dash", color = AMS_COLORS$red)))) |>
        ams_plotly_layout("Match output vs benchmark (100% = full MDB intensity)",
                          margin_b = 150)
    })

    # ---- Individual: per-minute rates vs group mean and vs MDB --------------
    output$indiv_plot <- renderPlotly({
      req(input$athlete)
      d  <- match_day()
      me <- d |> filter(athlete_name == input$athlete)
      validate(need(nrow(me) == 1, "Athlete did not feature in this match."))

      grp <- d |> filter(position_group == me$position_group)
      bm  <- MATCH_BENCHMARKS |>
        filter(position_group == me$position_group)
      validate(need(nrow(bm) == 1,
                    "No benchmark defined for this position group."))

      comp <- purrr::pmap_dfr(metric_defs(), function(key, label, bm_col) {
        my_rate  <- me[[key]] / me$match_minutes
        grp_rate <- mean(grp[[key]] / grp$match_minutes, na.rm = TRUE)
        bm_rate  <- bm[[bm_col]] / THRESHOLDS$match_full_min
        tibble(metric = label,
               vs_group = 100 * my_rate / grp_rate,
               vs_mdb   = 100 * my_rate / bm_rate)
      }) |>
        mutate(metric = factor(metric, levels = metric_defs()$label))

      plot_ly(comp, x = ~metric) |>
        add_bars(y = ~vs_group, name = "vs Position Group avg",
                 marker = list(color = AMS_COLORS$primary)) |>
        add_bars(y = ~vs_mdb, name = "vs 80-min Benchmark",
                 marker = list(color = AMS_COLORS$gold)) |>
        layout(barmode = "group",
               xaxis = list(title = "", automargin = TRUE),
               yaxis = list(title = "% (per-minute rates)",
                            automargin = TRUE),
               # Below the plot: at y = 1.15 it collided with the title.
               legend = list(orientation = "h", y = -0.14, yanchor = "top",
                             x = 0),
               shapes = list(list(
                 type = "line", x0 = -0.5,
                 x1 = nrow(comp) - 0.5, y0 = 100, y1 = 100,
                 line = list(dash = "dash", color = AMS_COLORS$red)))) |>
        ams_plotly_layout(sprintf("%s — %d min · %s",
                                  input$athlete, round(me$match_minutes),
                                  me$position_group),
                          margin_b = 84)
    })

    # ---- Group detail table: per-minute rates, selected athlete highlighted -
    output$group_table <- renderReactable({
      req(input$athlete)
      d  <- match_day()
      me <- d |> filter(athlete_name == input$athlete)
      validate(need(nrow(me) == 1, "Athlete did not feature in this match."))

      tbl <- d |>
        filter(position_group == me$position_group) |>
        transmute(
          Athlete = athlete_name,
          Min = round(match_minutes),
          `m/min` = round(distance / match_minutes, 1),
          `HSR/min` = round(hsr_distance / match_minutes, 2),
          `HMLD/min` = round(hmld / match_minutes, 1),
          `A+D/min` = round(ad / match_minutes, 2),
          `PL/min` = round(player_load / match_minutes, 1),
          `Imp/min` = round(contacts / match_minutes, 2)
        ) |>
        arrange(desc(`m/min`))
      if (!isTRUE(data()$has_hmld)) tbl <- select(tbl, -`HMLD/min`)
      if (!isTRUE(data()$has_load)) tbl <- select(tbl, -`PL/min`)
      if (all(is.na(tbl$`Imp/min`))) tbl <- select(tbl, -`Imp/min`)

      reactable(
        tbl, compact = TRUE, defaultPageSize = 15,
        rowStyle = function(index) {
          if (tbl$Athlete[index] == input$athlete)
            list(background = "rgba(170,255,0,0.12)", fontWeight = 700,
                 borderLeft = paste("3px solid", AMS_COLORS$primary))
        },
        theme = ams_react_theme
      )
    })
  })
}
