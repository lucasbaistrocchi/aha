# ==============================================================================
# mod_testing.R -- Tab: Performance Testing
#
# Squad-wide view of one test at a time: ranked athlete bars (coloured by
# cohort), cohort comparison, and a percentile table. Direction-aware --
# for metrics where lower is better (body fat, time-to-takeoff, time to peak
# force) the ranking and percentiles invert so "good" is always the top.
#
# Date dropdown carries every testing session in the sheet plus an
# "All-time best" option (per-athlete best, not a single session).
# ==============================================================================

mod_testing_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 9),
      card(
        card_header(
          div(class = paste("d-flex justify-content-between",
                            "align-items-center gap-2 flex-wrap"),
              span("Select test"),
              downloadButton(ns("export_pdf"), "PDF", class = "btn-sm"))
        ),
        selectInput(ns("metric"), "Test", choices = NULL),
        selectInput(ns("test_date"), "Testing date", choices = NULL),
        selectInput(ns("cohort"), "Cohort filter",
                    choices = "All cohorts"),
        uiOutput(ns("metric_note")),
        uiOutput(ns("squad_stats"))
      ),
      card(
        card_header("Squad ranking"),
        # Height scales with squad size -- a fixed box would compress 78
        # athletes into unreadable, overlapping labels.
        uiOutput(ns("rank_plot_ui"))
      )
    ),
    layout_columns(
      col_widths = c(5, 7),
      card(
        card_header("Position group comparison"),
        plotlyOutput(ns("cohort_plot"), height = "330px")
      ),
      card(
        card_header("Detail — value, squad percentile, cohort percentile"),
        reactableOutput(ns("detail_table"))
      )
    )
  )
}

mod_testing_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    testing <- reactive(data()$testing)

    # Cohort/position from the roster (testing sheet has its own Position
    # column but the roster is authoritative -- see apply_roster_override).
    roster_map <- reactive({
      data()$roster |> distinct(athlete_name, position_group)
    })

    observeEvent(data(), {
      tst <- testing()
      req(nrow(tst) > 0)

      # Dropdown lists every test column present in the sheet -- including
      # newly added ones with no results logged yet (flagged, not hidden),
      # grouped by test family for quick scanning.
      cols <- data()$testing_metrics %||% unique(tst$metric)
      with_data <- unique(tst$metric)
      avail <- TEST_METRICS |>
        filter(metric %in% cols) |>
        mutate(lbl = if_else(metric %in% with_data, metric,
                             paste0(metric, "  (no data yet)")))
      grouped <- lapply(split(avail, factor(avail$group,
                                            levels = unique(avail$group))),
                        function(g) setNames(g$metric, g$lbl))
      updateSelectInput(session, "metric", choices = grouped,
                        selected = if ("Jump Height (m)" %in% with_data)
                          "Jump Height (m)" else with_data[1])

      # Values are ISO strings (stable keys); labels are human-readable.
      dates <- sort(unique(tst$test_date), decreasing = TRUE, na.last = TRUE)
      keys  <- ifelse(is.na(dates), "__undated__", as.character(dates))
      updateSelectInput(session, "test_date",
                        choices = c(setNames(keys, test_date_label(dates)),
                                    "All-time best" = "__best__"),
                        selected = keys[1])

      updateSelectInput(session, "cohort",
                        choices = c("All cohorts",
                                    sort(unique(roster_map()$position_group))))
    })

    meta <- reactive({
      req(input$metric)
      m <- TEST_METRICS |> filter(metric == input$metric)
      if (nrow(m) == 0)
        m <- tibble(metric = input$metric, group = "", unit = "",
                    higher_better = TRUE)
      m[1, ]
    })

    # Values for the selected metric + date (or per-athlete best all-time),
    # joined to cohort and scored into percentiles.
    scored <- reactive({
      req(input$metric, input$test_date)
      tst <- testing() |> filter(metric == input$metric)
      validate(need(nrow(tst) > 0, paste0(
        "No results logged for ", input$metric,
        " yet — the column exists in the sheet but is empty.")))
      hb <- meta()$higher_better

      d <- if (identical(input$test_date, "__best__")) {
        tst |>
          group_by(athlete_name) |>
          slice(if (hb) which.max(value) else which.min(value)) |>
          ungroup()
      } else if (identical(input$test_date, "__undated__")) {
        tst |> filter(is.na(test_date))
      } else {
        tst |> filter(!is.na(test_date),
                      as.character(test_date) == input$test_date)
      }
      validate(need(nrow(d) > 0, "No results recorded on this date."))

      d <- d |>
        left_join(roster_map(), by = "athlete_name") |>
        mutate(position_group = coalesce(position_group, "Unassigned"))

      # Percentile: share of squad this athlete beats (direction-aware).
      pct_rank <- function(x, higher_better) {
        r <- rank(x, na.last = "keep", ties.method = "average")
        p <- 100 * (r - 0.5) / sum(!is.na(x))
        if (higher_better) p else 100 - p
      }

      d |>
        mutate(squad_pct = pct_rank(value, hb)) |>
        group_by(position_group) |>
        mutate(cohort_pct = if (n() > 1) pct_rank(value, hb) else NA_real_,
               cohort_mean = mean(value, na.rm = TRUE)) |>
        ungroup()
    })

    filtered <- reactive({
      s <- scored()
      if (!identical(input$cohort, "All cohorts"))
        s <- s |> filter(position_group == input$cohort)
      validate(need(nrow(s) > 0, "No athletes in this cohort for this test."))
      s
    })

    output$metric_note <- renderUI({
      m <- meta()
      p(class = "small text-muted mb-1",
        sprintf("%s%s — %s is better.",
                m$group, if (nzchar(m$unit)) paste0(" · ", m$unit) else "",
                if (m$higher_better) "higher" else "lower"))
    })

    output$squad_stats <- renderUI({
      s <- scored()
      m <- meta()
      fmt <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")
      best <- if (m$higher_better) s[which.max(s$value), ]
              else s[which.min(s$value), ]
      tagList(
        hr(),
        p(class = "mb-1", strong("Squad n = "), nrow(s)),
        p(class = "mb-1", strong("Mean: "), fmt(mean(s$value)),
          "  ·  ", strong("SD: "), fmt(sd(s$value))),
        p(class = "mb-0",
          strong("Best: ", style = paste0("color:", AMS_COLORS$primary)),
          sprintf("%s (%s)", best$athlete_name, fmt(best$value)))
      )
    })

    # --- Ranked athlete bars, coloured by cohort ------------------------------
    output$rank_plot_ui <- renderUI({
      n <- tryCatch(nrow(filtered()), error = function(e) 0)
      div(style = "max-height:640px;overflow-y:auto",
          plotlyOutput(session$ns("rank_plot"),
                       height = bar_chart_height(n)))
    })

    output$rank_plot <- renderPlotly({
      d <- filtered() |> arrange(value)
      m <- meta()
      # Ascending y-order puts the best performer at the top for
      # higher-is-better metrics; flip for lower-is-better.
      if (!m$higher_better) d <- d |> arrange(desc(value))

      pal <- setNames(
        colorRampPalette(c(AMS_COLORS$primary, AMS_COLORS$gold,
                           "#5A8F00"))(length(unique(d$position_group))),
        sort(unique(d$position_group)))

      plot_ly(d, x = ~value, y = ~factor(athlete_name, levels = athlete_name),
              type = "bar", orientation = "h",
              color = ~position_group, colors = pal,
              hovertemplate = paste0("%{y}: %{x:.2f} ", m$unit,
                                     "<extra></extra>")) |>
        layout(
          xaxis = list(title = paste0(m$metric,
                                      if (nzchar(m$unit))
                                        paste0(" (", m$unit, ")") else "")),
          yaxis = list(title = "", tickfont = list(size = 10),
                       automargin = TRUE),
          # Legend below the plot; anchored so it never rides over the bars.
          legend = list(orientation = "h", y = -0.02, yanchor = "top",
                        x = 0, font = list(size = 10)),
          bargap = 0.25,
          shapes = list(list(
            type = "line", x0 = mean(d$value), x1 = mean(d$value),
            y0 = -0.5, y1 = nrow(d) - 0.5,
            line = list(dash = "dash", color = AMS_COLORS$red, width = 1.5)))
        ) |>
        ams_plotly_layout(paste0(m$metric, " — squad ranking (dashed = mean)"),
                          margin_b = 78, margin_l = 150)
    })

    # --- Cohort comparison: mean + individual points -------------------------
    output$cohort_plot <- renderPlotly({
      s <- scored()
      m <- meta()
      plot_ly() |>
        add_bars(data = s |> group_by(position_group) |>
                   summarise(mean_val = mean(value), .groups = "drop"),
                 x = ~position_group, y = ~mean_val, name = "Cohort mean",
                 marker = list(color = "rgba(170,255,0,0.30)",
                               line = list(color = AMS_COLORS$primary,
                                           width = 1.5))) |>
        add_markers(data = s, x = ~position_group, y = ~value,
                    name = "Athletes", text = ~athlete_name,
                    hovertemplate = "%{text}: %{y:.2f}<extra></extra>",
                    marker = list(color = AMS_COLORS$gold, size = 8,
                                  opacity = 0.75)) |>
        layout(xaxis = list(title = "", tickangle = -25, automargin = TRUE),
               yaxis = list(title = m$unit, automargin = TRUE),
               showlegend = FALSE) |>
        ams_plotly_layout(paste(m$metric, "by position group"),
                          margin_b = 86)
    })

    # --- PDF export: ranked results for the selected test -------------------
    output$export_pdf <- downloadHandler(
      filename = function()
        paste0("testing-",
               gsub("[^A-Za-z0-9]+", "-", input$metric %||% "test"),
               "-", Sys.Date(), ".pdf"),
      content = function(file) {
        m <- meta()
        d <- filtered() |>
          arrange(if (m$higher_better) desc(value) else value)
        s <- scored()

        date_lbl <- if (identical(input$test_date, "__best__"))
          "All-time best" else {
            dd <- suppressWarnings(as_date(input$test_date))
            if (is.na(dd)) "Undated" else test_date_label(dd)
          }

        cols <- list(
          list(label = "#",           x = 0.00, align = "left"),
          list(label = "ATHLETE",     x = 0.05, align = "left"),
          list(label = "COHORT",      x = 0.38, align = "left"),
          list(label = "VALUE",       x = 0.68, align = "right"),
          list(label = "VS COHORT",   x = 0.82, align = "right"),
          list(label = "SQUAD %ILE",  x = 0.99, align = "right"))

        rows <- lapply(seq_len(nrow(d)), function(i) c(
          as.character(i),
          d$athlete_name[i],
          as.character(d$position_group[i]),
          formatC(d$value[i], format = "f", digits = 2, big.mark = ","),
          sprintf("%+.2f", d$value[i] - d$cohort_mean[i]),
          sprintf("%.0f", d$squad_pct[i])))

        # Percentile column keeps the on-screen colour bands.
        colour_fn <- function(i, j) {
          if (j != 6) return("#222222")
          p <- d$squad_pct[i]
          if (p >= 75) "#1E8449" else if (p >= 40) "#B7950B" else "#C0392B"
        }

        grDevices::pdf(file, width = 8.5, height = 11)
        on.exit(grDevices::dev.off(), add = TRUE)
        pdf_table(
          title = paste0("Performance Testing — ", m$metric),
          subtitle = sprintf(
            "%s | %s | %s is better | squad n=%d, mean %.2f, SD %.2f",
            date_lbl,
            if (identical(input$cohort, "All cohorts")) "All cohorts"
              else input$cohort,
            if (m$higher_better) "higher" else "lower",
            nrow(s), mean(s$value), sd(s$value)),
          cols = cols, rows = rows, colour_fn = colour_fn)
      })

    output$detail_table <- renderReactable({
      m <- meta()
      tbl <- filtered() |>
        arrange(if (m$higher_better) desc(value) else value) |>
        transmute(
          Rank = row_number(),
          Athlete = athlete_name,
          Cohort = position_group,
          Value = round(value, 2),
          `vs cohort mean` = round(value - cohort_mean, 2),
          `Squad %ile` = round(squad_pct),
          `Cohort %ile` = round(cohort_pct)
        )

      pct_col <- colDef(
        cell = function(value) if (is.na(value)) "—" else paste0(value, "%"),
        style = function(value) {
          if (is.na(value)) return(NULL)
          col <- if (value >= 75) AMS_COLORS$primary
                 else if (value >= 40) AMS_COLORS$gold else AMS_COLORS$red
          list(color = col, fontWeight = 700)
        }, width = 100)

      reactable(
        tbl, compact = TRUE, striped = TRUE, defaultPageSize = 15,
        columns = list(
          Rank = colDef(width = 56),
          Athlete = colDef(style = list(fontWeight = 600)),
          `Squad %ile` = pct_col, `Cohort %ile` = pct_col,
          `vs cohort mean` = colDef(style = function(value) {
            if (is.na(value)) return(NULL)
            good <- if (m$higher_better) value > 0 else value < 0
            list(color = if (good) AMS_COLORS$primary else AMS_COLORS$grey)
          })
        ),
        theme = ams_react_theme
      )
    })
  })
}
