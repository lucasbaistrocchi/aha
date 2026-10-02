# ==============================================================================
# mod_individual.R -- Tab: Individual Report
#
# One-page athlete summary pulling all three data streams:
#   * GPS      -- season totals, per-match rates vs cohort benchmark
#   * Testing  -- percentile profile vs squad and cohort
#   * Match    -- appearance log, minutes, exposure
#   * Wellness -- current readiness + 21-day trend
# PDF export uses the base pdf() device (no pandoc/Chrome dependency).
# ==============================================================================

mod_individual_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(12),
      card(
        class = "p-2",
        div(class = paste("d-flex justify-content-between align-items-center",
                          "gap-3 flex-wrap"),
            div(style = "min-width:240px;margin-bottom:0",
                selectInput(ns("athlete"), NULL, choices = NULL,
                            width = "240px")),
            div(class = "d-flex align-items-center gap-2 flex-wrap",
                uiOutput(ns("header_chips")),
                downloadButton(ns("export_pdf"), "Export PDF",
                               class = "btn-sm")))
      )
    ),
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(title = "Matches Played", value = textOutput(ns("vb_apps")),
                theme = "primary", class = "vb-compact"),
      value_box(title = "Total Minutes", value = textOutput(ns("vb_mins")),
                theme = "secondary", class = "vb-compact"),
      value_box(title = "Top Speed (m/s) — best logged",
                value = textOutput(ns("vb_vmax")),
                theme = "success", class = "vb-compact")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Match output vs cohort benchmark (per-minute)"),
        plotlyOutput(ns("bench_plot"), height = "300px"),
        reactableOutput(ns("bench_table"))
      ),
      card(
        card_header(uiOutput(ns("test_header"))),
        plotlyOutput(ns("test_plot"), height = "300px")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header(
          div(class = paste("d-flex justify-content-between",
                            "align-items-center gap-2 flex-wrap"),
              span("Athlete vs cohort profile"),
              div(style = "width:180px;margin-bottom:0",
                  selectInput(ns("spider_mode"), NULL,
                              choices = c("Testing profile" = "testing",
                                          "Match output" = "match"),
                              width = "170px")))
        ),
        plotlyOutput(ns("spider_plot"), height = "380px"),
        p(class = "small text-muted mb-0 px-2",
          "100 = cohort average. Axes are direction-adjusted, so further
           from centre is always better — including timed and body-fat
           measures where lower raw scores are the good ones.")
      ),
      card(
        card_header("Readiness — last 21 days"),
        plotlyOutput(ns("wellness_plot"), height = "260px"),
        uiOutput(ns("wellness_note"))
      )
    ),
    card(
      card_header("Match log"),
      reactableOutput(ns("match_table"))
    )
  )
}

mod_individual_server <- function(id, data, wellness_scored, vaccine) {
  moduleServer(id, function(input, output, session) {

    observeEvent(data(), {
      updateSelectInput(session, "athlete",
                        choices = sort(unique(data()$gps$athlete_name)))
    })

    ath_gps <- reactive({
      req(input$athlete)
      data()$gps |> filter(athlete_name == input$athlete)
    })

    ath_matches <- reactive(ath_gps() |> filter(session_type == "match"))

    ath_info <- reactive({
      g <- ath_gps()
      req(nrow(g) > 0)
      # vmax is already the best logged session speed (see derive_vmax);
      # tested is the roster's combine figure, kept only for comparison.
      vt <- if ("vmax_tested" %in% names(g))
        suppressWarnings(max(g$vmax_tested, na.rm = TRUE)) else NA_real_
      list(name = input$athlete,
           cohort = g$position_group[1],
           vmax = suppressWarnings(max(g$vmax, na.rm = TRUE)),
           vmax_tested = if (is.finite(vt)) vt else NA_real_,
           best_date = {
             ok <- is.finite(g$max_vel) & g$max_vel > 0
             if (any(ok)) g$date[ok][which.max(g$max_vel[ok])] else NA
           })
    })

    # --- Header ---------------------------------------------------------------
    output$header_chips <- renderUI({
      info <- ath_info()
      v <- vaccine() |> filter(athlete_name == input$athlete)
      chip <- function(bg, txt, dark = TRUE)
        span(class = "stat-chip",
             style = paste0("background:", bg, ";color:",
                            if (dark) "#0A0A0A" else "white"), txt)
      tagList(
        chip(AMS_COLORS$primary, info$cohort),
        if (is.finite(info$vmax) && !is.na(info$best_date))
          chip(AMS_COLORS$grey,
               paste("PB", format(info$best_date, "%b %d")), FALSE),
        if (is.finite(info$vmax_tested))
          chip(AMS_COLORS$grey,
               sprintf("tested %.2f", info$vmax_tested), FALSE),
        if (nrow(v) == 1) {
          st <- as.character(v$status)
          chip(switch(st, Green = AMS_COLORS$green,
                      Yellow = AMS_COLORS$gold, AMS_COLORS$red),
               paste("Speed vaccine:", st), st != "Red")
        }
      )
    })

    output$vb_apps <- renderText(as.character(nrow(ath_matches())))
    output$vb_mins <- renderText({
      m <- ath_matches()
      format(round(sum(m$match_minutes, na.rm = TRUE)), big.mark = ",")
    })
    output$vb_vmax <- renderText({
      v <- ath_info()$vmax
      if (is.finite(v)) sprintf("%.2f", v) else "—"
    })

    # --- Match output vs cohort benchmark (per-minute rates) ------------------
    bench_data <- reactive({
      m <- ath_matches()
      req(nrow(m) > 0)
      bm <- MATCH_BENCHMARKS |>
        filter(position_group == ath_info()$cohort)
      req(nrow(bm) == 1)

      tot_min <- sum(m$match_minutes, na.rm = TRUE)
      req(tot_min > 0)
      rate <- function(x) sum(x, na.rm = TRUE) / tot_min

      defs <- tibble(
        metric = c("Distance", "HSR", "HMLD", "A+D"),
        mine   = c(rate(m$distance), rate(m$hsr_distance), rate(m$hmld),
                   rate(m$accels + coalesce(m$decels, 0))),
        # The cohort's full-match target, and the per-minute rate it implies.
        bench_80 = c(bm$bm_distance, bm$bm_hsr, bm$bm_hmld, bm$bm_ad),
        bench  = c(bm$bm_distance, bm$bm_hsr, bm$bm_hmld, bm$bm_ad) /
                   THRESHOLDS$match_full_min
      ) |>
        mutate(pct = 100 * mine / bench,
               # What this athlete's own minutes entitle them to.
               expected = bench * tot_min,
               actual = mine * tot_min) |>
        filter(is.finite(pct))
      defs
    })

    # Spells out the benchmark instead of leaving it implicit in a percentage.
    output$bench_table <- renderReactable({
      d <- bench_data()
      validate(need(nrow(d) > 0, "No match data for this athlete."))
      tot <- sum(ath_matches()$match_minutes, na.rm = TRUE)

      tbl <- d |>
        transmute(
          Metric = metric,
          `Your /min` = round(mine, 2),
          `Bench /min` = round(bench, 2),
          `80-min bench` = round(bench_80),
          `Your total` = round(actual),
          Expected = round(expected),
          `%` = round(pct)
        )
      # Name the column after the athlete's actual minutes so the comparison
      # is unambiguous.
      names(tbl)[names(tbl) == "Expected"] <-
        sprintf("Expected (%d min)", round(tot))

      reactable(
        tbl, compact = TRUE, defaultPageSize = 5,
        defaultColDef = colDef(format = colFormat(separators = TRUE)),
        columns = list(
          Metric = colDef(style = list(fontWeight = 600), width = 90),
          `%` = colDef(width = 70, cell = function(value) paste0(value, "%"),
                       style = function(value) {
                         col <- if (value >= 100) AMS_COLORS$primary
                                else if (value >= 80) NULL else AMS_COLORS$gold
                         list(color = col, fontWeight = 700)
                       })
        ),
        theme = ams_react_theme
      )
    })

    output$bench_plot <- renderPlotly({
      d <- bench_data()
      validate(need(nrow(d) > 0, "No match data for this athlete."))
      plot_ly(d, x = ~metric, y = ~pct, type = "bar",
              marker = list(color = ~ifelse(pct >= 100, AMS_COLORS$primary,
                                            AMS_COLORS$gold)),
              text = ~sprintf("%.0f%%", pct), textposition = "outside",
              cliponaxis = FALSE,
              hovertemplate = "%{x}: %{y:.0f}% of benchmark<extra></extra>") |>
        layout(xaxis = list(title = "", automargin = TRUE),
               # Headroom so the "outside" value labels aren't clipped.
               yaxis = list(title = "% of cohort 80-min benchmark",
                            automargin = TRUE,
                            range = c(0, max(120, max(d$pct) * 1.18))),
               shapes = list(list(
                 type = "line", x0 = -0.5, x1 = nrow(d) - 0.5,
                 y0 = 100, y1 = 100,
                 line = list(dash = "dash", color = AMS_COLORS$red)))) |>
        ams_plotly_layout("Per-minute match output vs benchmark")
    })

    # --- Testing percentile profile ------------------------------------------
    test_data <- reactive({
      tst <- data()$testing
      req(nrow(tst) > 0)
      rm <- data()$roster |> distinct(athlete_name, position_group)

      # Latest session per athlete-metric (real Date ordering; NA dates last).
      latest <- tst |>
        group_by(athlete_name, metric) |>
        arrange(test_date, .by_group = TRUE) |>
        slice_tail(n = 1) |>
        ungroup() |>
        left_join(TEST_METRICS, by = "metric") |>
        left_join(rm, by = "athlete_name")

      # Rank WITHIN the athlete's own positional cohort: a prop's squat
      # against the squad says more about his position than his strength.
      my_cohort <- ath_info()$cohort

      latest |>
        # Fixed, comparable panel of tests -- not whatever happens to rank
        # highest for this athlete.
        filter(metric %in% REPORT_METRICS$metric,
               !is.na(position_group), position_group == my_cohort) |>
        group_by(metric) |>
        mutate(
          n_ok = sum(!is.na(value)),
          r = rank(value, na.last = "keep", ties.method = "average"),
          pct = 100 * (r - 0.5) / n_ok,
          pct = if_else(coalesce(higher_better, TRUE), pct, 100 - pct)
        ) |>
        ungroup() |>
        # A percentile drawn from one or two tested cohort-mates is noise.
        filter(athlete_name == input$athlete, !is.na(pct), n_ok >= 3) |>
        left_join(REPORT_METRICS, by = "metric")
    })

    output$test_header <- renderUI({
      span(paste0("Testing profile — ",
                  tryCatch(ath_info()$cohort, error = function(e) "cohort"),
                  " percentile"))
    })

    output$test_plot <- renderPlotly({
      d <- test_data()
      validate(need(nrow(d) > 0, paste(
        "No cohort percentiles available for this athlete — either no",
        "testing results, or fewer than 3 tested athletes in their",
        "position group for these tests.")))
      n_cohort <- max(d$n_ok, na.rm = TRUE)
      test_day <- suppressWarnings(max(d$test_date, na.rm = TRUE))
      # Fixed display order; reversed so the first listed test sits on top.
      ord <- rev(REPORT_METRICS$label[REPORT_METRICS$label %in% d$label])
      d <- d |> mutate(label = factor(label, levels = ord)) |> arrange(label)

      plot_ly(d, x = ~pct, y = ~label,
              type = "bar", orientation = "h",
              marker = list(color = ~case_when(
                pct >= 75 ~ AMS_COLORS$primary,
                pct >= 40 ~ AMS_COLORS$gold,
                TRUE      ~ AMS_COLORS$red)),
              # Significant-digit aware: "%.0f" turned a 0.34 m jump into
              # "0". Jump height is now read in cm, but small-valued tests
              # still need decimals.
              text = ~fmt_metric_value(value), textposition = "outside",
              cliponaxis = FALSE,
              hovertemplate = paste0("%{y}<br>value %{text}",
                                     "<br>%{x:.0f}th pct<extra></extra>")) |>
        layout(xaxis = list(title = sprintf("%s percentile (n=%d)",
                                            ath_info()$cohort, n_cohort),
                            range = c(0, 122), automargin = TRUE),
               yaxis = list(title = "", tickfont = list(size = 10),
                            automargin = TRUE),
               shapes = list(list(
                 type = "line", x0 = 50, x1 = 50, y0 = -0.5,
                 y1 = nrow(d) - 0.5,
                 line = list(dash = "dot", color = AMS_COLORS$grey)))) |>
        ams_plotly_layout(sprintf(
          "Testing percentiles vs %s%s (dotted = cohort median)",
          ath_info()$cohort,
          if (is.finite(test_day))
            paste0(" — ", test_date_label(test_day)) else ""),
          margin_l = 130)
    })

    # --- Spider: athlete vs positional cohort --------------------------------
    # Axes are indexed to the cohort mean (100 = cohort average) and
    # DIRECTION-ADJUSTED so bigger is always better:
    #   higher-is-better  -> 100 * value / cohort_mean
    #   lower-is-better   -> 100 * cohort_mean / value   (a faster 10 m or a
    #                        lower body fat pushes the polygon outward)
    # Indexing to the cohort mean is what lets tests with wildly different
    # units share one set of axes.
    spider_data <- reactive({
      req(input$athlete)
      cohort <- ath_info()$cohort
      mode <- input$spider_mode %||% "testing"

      idx <- function(value, cmean, higher_better) {
        ok <- is.finite(value) & is.finite(cmean) & cmean != 0 & value != 0
        out <- rep(NA_real_, length(value))
        out[ok & higher_better]  <- 100 * value[ok & higher_better] /
                                      cmean[ok & higher_better]
        out[ok & !higher_better] <- 100 * cmean[ok & !higher_better] /
                                      value[ok & !higher_better]
        out
      }

      if (mode == "testing") {
        tst <- data()$testing
        req(nrow(tst) > 0)
        rm <- data()$roster |> distinct(athlete_name, position_group)

        latest <- tst |>
          group_by(athlete_name, metric) |>
          arrange(test_date, .by_group = TRUE) |>
          slice_tail(n = 1) |>
          ungroup() |>
          inner_join(REPORT_METRICS, by = "metric") |>
          left_join(TEST_METRICS |> select(metric, higher_better),
                    by = "metric") |>
          left_join(rm, by = "athlete_name") |>
          filter(position_group == cohort)

        req(nrow(latest) > 0)
        summ <- latest |>
          group_by(metric, label, higher_better) |>
          summarise(cmean = mean(value, na.rm = TRUE),
                    n_cohort = sum(!is.na(value)), .groups = "drop")

        mine <- latest |>
          filter(athlete_name == input$athlete) |>
          select(metric, value)

        summ |>
          inner_join(mine, by = "metric") |>
          filter(n_cohort >= 2) |>
          mutate(score = idx(value, cmean, coalesce(higher_better, TRUE))) |>
          filter(is.finite(score)) |>
          mutate(.ord = match(label, REPORT_METRICS$label)) |>
          arrange(.ord) |>
          select(label, value, cmean, score)

      } else {
        g <- data()$gps |>
          filter(session_type == "match", position_group == cohort,
                 match_minutes > 0)
        req(nrow(g) > 0)

        rates <- g |>
          mutate(ad = accels + coalesce(decels, 0)) |>
          group_by(athlete_name) |>
          summarise(
            mins = sum(match_minutes, na.rm = TRUE),
            Distance = sum(distance, na.rm = TRUE) / mins,
            HSR      = sum(hsr_distance, na.rm = TRUE) / mins,
            HMLD     = sum(hmld, na.rm = TRUE) / mins,
            `A+D`    = sum(ad, na.rm = TRUE) / mins,
            `Max vel` = suppressWarnings(max(max_vel, na.rm = TRUE)),
            .groups = "drop") |>
          select(-mins)
        if (!isTRUE(data()$has_hmld)) rates <- select(rates, -HMLD)

        long <- rates |>
          pivot_longer(-athlete_name, names_to = "label",
                       values_to = "value") |>
          filter(is.finite(value))

        summ <- long |>
          group_by(label) |>
          summarise(cmean = mean(value, na.rm = TRUE),
                    n_cohort = n(), .groups = "drop")

        summ |>
          inner_join(long |> filter(athlete_name == input$athlete) |>
                       select(label, value), by = "label") |>
          filter(n_cohort >= 2) |>
          # All match-output metrics are higher-is-better.
          mutate(score = idx(value, cmean, rep(TRUE, n()))) |>
          filter(is.finite(score)) |>
          select(label, value, cmean, score)
      }
    })

    output$spider_plot <- renderPlotly({
      d <- spider_data()
      validate(need(nrow(d) >= 3,
        "Need at least 3 comparable measures with 2+ cohort-mates to draw a
         profile. Try the other view, or check the athlete has results
         logged."))

      # Close the polygon by repeating the first point.
      close_loop <- function(v) c(v, v[1])
      axes <- close_loop(d$label)
      rmax <- max(140, ceiling(max(d$score, na.rm = TRUE) / 10) * 10)

      plot_ly(type = "scatterpolar", mode = "lines") |>
        add_trace(
          r = close_loop(rep(100, nrow(d))), theta = axes,
          name = paste(ath_info()$cohort, "average"),
          fill = "toself",
          fillcolor = "rgba(212,175,55,0.12)",
          line = list(color = AMS_COLORS$gold, width = 2, dash = "dot"),
          hovertemplate = "%{theta}: cohort average<extra></extra>") |>
        add_trace(
          r = close_loop(d$score), theta = axes,
          name = input$athlete,
          fill = "toself",
          fillcolor = "rgba(170,255,0,0.22)",
          line = list(color = AMS_COLORS$primary, width = 3),
          text = close_loop(sprintf("%.2f (cohort %.2f)", d$value, d$cmean)),
          hovertemplate = "%{theta}: %{r:.0f}% of cohort<br>%{text}<extra></extra>") |>
        layout(
          polar = list(
            bgcolor = "rgba(0,0,0,0)",
            radialaxis = list(visible = TRUE, range = c(0, rmax),
                              gridcolor = AMS_COLORS$grid,
                              tickfont = list(size = 9,
                                              color = AMS_COLORS$grey),
                              angle = 90, tickangle = 90),
            angularaxis = list(gridcolor = AMS_COLORS$grid,
                               linecolor = AMS_COLORS$grid,
                               tickfont = list(size = 10,
                                               color = AMS_COLORS$ink))
          ),
          showlegend = TRUE,
          legend = list(orientation = "h", y = -0.12),
          paper_bgcolor = "rgba(0,0,0,0)",
          font = list(family = "Rajdhani", color = AMS_COLORS$ink),
          margin = list(t = 40, b = 40, l = 60, r = 60)
        ) |>
        config(displaylogo = FALSE)
    })

    # --- Match log ------------------------------------------------------------
    output$match_table <- renderReactable({
      m <- ath_matches()
      validate(need(nrow(m) > 0, "No matches recorded."))
      tbl <- m |>
        arrange(desc(date)) |>
        transmute(
          Date = format(date, "%b %d"),
          Opponent = coalesce(opponent, "—"),
          Min = round(match_minutes),
          `Dist (m)` = round(distance),
          `m/min` = round(distance / match_minutes, 1),
          `HSR (m)` = round(hsr_distance),
          `HMLD (m)` = round(hmld),
          `A+D` = accels + coalesce(decels, 0),
          `Max vel` = max_vel
        )
      if (!isTRUE(data()$has_hmld)) tbl <- select(tbl, -`HMLD (m)`)
      reactable(tbl, compact = TRUE, striped = TRUE, defaultPageSize = 10,
                defaultColDef = colDef(format = colFormat(separators = TRUE)),
                theme = ams_react_theme)
    })

    # --- Wellness -------------------------------------------------------------
    ath_wellness <- reactive({
      wellness_scored() |>
        filter(athlete_name == input$athlete, date > max(date) - 21)
    })

    output$wellness_plot <- renderPlotly({
      d <- ath_wellness()
      validate(need(nrow(d) > 0, "No wellness submissions for this athlete."))
      plot_ly(d, x = ~date, y = ~readiness, type = "scatter", mode = "lines+markers",
              line = list(color = AMS_COLORS$primary, width = 3),
              marker = list(size = 8,
                            color = ~if_else(z_flag, AMS_COLORS$red,
                                             AMS_COLORS$primary))) |>
        layout(xaxis = list(title = "", automargin = TRUE),
               yaxis = list(title = "Readiness %", range = c(0, 100),
                            automargin = TRUE)) |>
        ams_plotly_layout(NULL, hovermode = "x unified", margin_t = 24)
    })

    output$wellness_note <- renderUI({
      d <- ath_wellness()
      if (nrow(d) == 0) return(NULL)
      last <- d |> slice_max(date, n = 1, with_ties = FALSE)
      p(class = "small mb-0",
        strong("Latest: "), sprintf("%.0f%% readiness", last$readiness),
        if (!is.na(last$readiness_z))
          sprintf(" (z = %.2f)", last$readiness_z),
        if (isTRUE(last$injury_flag))
          span(style = paste0("color:", AMS_COLORS$red, ";font-weight:600"),
               paste0(" · reports: ", last$aches)))
    })

    # --- PDF export (base device, ASCII-safe) --------------------------------
    output$export_pdf <- downloadHandler(
      filename = function()
        paste0(gsub("[^A-Za-z0-9]+", "-", input$athlete), "-report-",
               Sys.Date(), ".pdf"),
      content = function(file) {
        info <- ath_info()
        m    <- ath_matches()
        bd   <- tryCatch(bench_data(), error = function(e) tibble())
        td   <- tryCatch(test_data(), error = function(e) tibble())
        wl   <- tryCatch(ath_wellness(), error = function(e) tibble())
        sp   <- tryCatch(spider_data(), error = function(e) tibble())
        sp_mode <- input$spider_mode %||% "testing"

        ascii <- function(x) iconv(gsub("[^ -~]", "-", x),
                                   to = "ASCII//TRANSLIT", sub = "")

        grDevices::pdf(file, width = 8.5, height = 11)
        on.exit(grDevices::dev.off(), add = TRUE)
        par(mar = c(0, 0, 0, 0))
        plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))

        # --- Layout primitives -------------------------------------------
        # Text is TOP-anchored at y. Heights and widths are MEASURED with
        # strheight()/strwidth() rather than guessed, which is what caused
        # titles to collide and separator rules to strike through text.
        L <- 0.03; R <- 0.97
        th <- function(cex, font = 1) strheight("Ag", cex = cex, font = font)
        tw <- function(s, cex, font = 1) strwidth(s, cex = cex, font = font)
        put <- function(x, y, s, cex = 0.76, font = 1, col = "#222222",
                        adj = 0) {
          text(x, y, ascii(s), adj = c(adj, 1), cex = cex, font = font,
               col = col)
          invisible(y - th(cex, font))
        }
        # Rule sits a clear gap BELOW the text block that precedes it.
        rule <- function(y, col = "#999999", lwd = 0.8, gap = 0.006) {
          yy <- y - gap
          segments(L, yy, R, yy, col = col, lwd = lwd)
          yy - gap
        }
        sec <- function(y, title) {
          y <- put(L, y, title, cex = 1.0, font = 2, col = "#111111")
          y - 0.008
        }

        hbar <- function(x0, y, w, frac, label, val, good) {
          h <- 0.013
          rect(x0, y - h, x0 + w, y, col = "#EEEEEE", border = NA)
          rect(x0, y - h, x0 + w * max(0, min(1, frac)), y,
               col = if (good) "#4C9A00" else "#B8860B", border = NA)
          text(x0 - 0.006, y - h / 2, ascii(label), adj = c(1, 0.5),
               cex = 0.70)
          text(x0 + w + 0.008, y - h / 2, ascii(val), adj = c(0, 0.5),
               cex = 0.70, font = 2)
          y - h - 0.008
        }

        # Radar, drawn circular despite the 8.5x11 page (rx scaled by 11/8.5).
        draw_radar <- function(cx, cy, ry, labels, scores, rmax) {
          n <- length(labels)
          rx <- ry * 11 / 8.5
          ang <- 2 * pi * (seq_len(n) - 1) / n
          px <- function(rr, a) cx + rx * (rr / rmax) * sin(a)
          py <- function(rr, a) cy + ry * (rr / rmax) * cos(a)
          aa <- seq(0, 2 * pi, length.out = 120)
          for (ring in c(50, 100, rmax))
            lines(px(ring, aa), py(ring, aa),
                  col = if (ring == 100) "#8A8A8A" else "#DDDDDD",
                  lty = if (ring == 100) 2 else 1,
                  lwd = if (ring == 100) 1.1 else 0.5)
          segments(cx, cy, px(rmax, ang), py(rmax, ang),
                   col = "#DDDDDD", lwd = 0.5)
          polygon(px(rep(100, n), ang), py(rep(100, n), ang),
                  border = "#B8860B", lty = 2, lwd = 1.3)
          sc <- pmin(scores, rmax)
          polygon(px(sc, ang), py(sc, ang), border = "#4C9A00", lwd = 2,
                  col = grDevices::adjustcolor("#4C9A00", alpha.f = 0.18))
          points(px(sc, ang), py(sc, ang), pch = 19, cex = 0.45,
                 col = "#4C9A00")
          for (i in seq_len(n)) {
            a <- ang[i]
            adj <- if (sin(a) > 0.3) 0 else if (sin(a) < -0.3) 1 else 0.5
            text(px(rmax * 1.17, a), py(rmax * 1.17, a), ascii(labels[i]),
                 cex = 0.52, col = "#222222", adj = c(adj, 0.5))
          }
        }

        # Readiness line chart (same construction as the ACWR panel).
        draw_readiness <- function(d, x0, x1, y_top, h) {
          yt <- y_top; yb <- y_top - h
          n <- nrow(d)
          xs <- if (n > 1) x0 + (x1 - x0) * (seq_len(n) - 1) / (n - 1)
                else rep((x0 + x1) / 2, n)
          ry <- function(v) yb + (yt - yb) * pmin(pmax(v, 0), 100) / 100
          for (v in c(25, 50, 75, 100)) {
            segments(x0, ry(v), x1, ry(v), col = "#E8E8E8", lwd = 0.4)
            text(x0 - 0.006, ry(v), v, adj = c(1, 0.5), cex = 0.5,
                 col = "#777777")
          }
          # 70% working reference line.
          segments(x0, ry(70), x1, ry(70), col = "#B8860B", lty = 2,
                   lwd = 0.8)
          segments(x0, yb, x1, yb, col = "#333333", lwd = 0.8)
          segments(x0, yb, x0, yt, col = "#333333", lwd = 0.8)
          lines(xs, ry(d$readiness), col = "#1E8449", lwd = 1.6)
          flag <- !is.na(d$z_flag) & d$z_flag
          points(xs[!flag], ry(d$readiness[!flag]), pch = 19, cex = 0.4,
                 col = "#1E8449")
          if (any(flag))
            points(xs[flag], ry(d$readiness[flag]), pch = 19, cex = 0.62,
                   col = "#C0392B")
          idx <- unique(round(seq(1, n, length.out = min(6, n))))
          for (i in idx)
            text(xs[i], yb - 0.004, format(d$date[i], "%b %d"),
                 adj = c(0.5, 1), cex = 0.5, col = "#777777")
          yb - 0.020
        }

        short_lab <- function(x) {
          x <- gsub("CMJ Jump Height \\(cm\\)", "CMJ", x)
          x <- gsub("CMJ Jump Height", "CMJ", x)
          x <- gsub("IMTP Peak Force", "IMTP", x)
          x <- gsub("Chin Up 1RM", "ChinUp", x)
          x <- gsub("Squat 3RM", "Squat", x); x <- gsub("Bench 3RM", "Bench", x)
          x <- gsub("Body Fat %", "BF%", x);  x <- gsub("Distance", "Dist", x)
          x <- gsub("Max vel", "Vmax", x)
          ascii(x)
        }

        # --- Header --------------------------------------------------------
        # Leading is proportional to the type size: strheight() measures the
        # glyphs only, so a 1.75-cex name needs real space beneath it before
        # the subtitle, otherwise the two read as one bunched block.
        y <- 0.975
        y <- put(L, y, info$name, cex = 1.75, font = 2, col = "#111111")
        y <- y - 0.40 * th(1.75, 2)
        y <- put(L, y, sprintf("%s  |  Top speed %s m/s  |  %d matches, %d min",
                               info$cohort,
                               if (is.finite(info$vmax))
                                 sprintf("%.2f", info$vmax) else "n/a",
                               nrow(m),
                               round(sum(m$match_minutes, na.rm = TRUE))),
                 cex = 0.88, col = "#444444")
        y <- rule(y, col = "#222222", lwd = 1.6, gap = 0.012)
        y <- y - 0.014

        # --- Match output vs benchmark -------------------------------------
        y <- sec(y, "MATCH OUTPUT vs COHORT BENCHMARK (per minute)")
        if (nrow(bd) > 0) {
          for (i in seq_len(nrow(bd)))
            y <- hbar(0.17, y, 0.46, bd$pct[i] / 130, bd$metric[i],
                      sprintf("%.0f%%  (bench %s)", bd$pct[i],
                              format(round(bd$bench_80[i]), big.mark = ",")),
                      bd$pct[i] >= 100)
          y <- put(0.17, y, "bar scale: 0-130% of benchmark", cex = 0.58,
                   col = "#777777")
        } else {
          y <- put(0.06, y, "No match data.", cex = 0.76, col = "#666666")
        }
        y <- y - 0.014

        # --- Testing bars (left) + radar (right), same vertical band -------
        band_top <- y
        left_title <- "PERFORMANCE TESTING"
        y <- sec(y, left_title)
        if (nrow(td) > 0) {
          tp <- td |> mutate(.o = match(label, REPORT_METRICS$label)) |>
            arrange(.o)
          for (i in seq_len(nrow(tp)))
            y <- hbar(0.20, y, 0.24, tp$pct[i] / 100, short_lab(tp$label[i]),
                      sprintf("%.0fth (%s)", tp$pct[i],
                              fmt_metric_value(tp$value[i])),
                      tp$pct[i] >= 50)
        } else {
          y <- put(0.06, y, "No testing data.", cex = 0.76, col = "#666666")
        }

        if (nrow(sp) >= 3) {
          # Place the radar's caption so it cannot collide with the section
          # title on the left: start it past the measured title width.
          rad_cx <- 0.80
          cap <- sprintf("vs %s - %s", info$cohort,
                         if (sp_mode == "match") "MATCH" else "TESTING")
          cap_w <- tw(cap, 0.68, 2)
          cap_x <- max(rad_cx - cap_w / 2,
                       L + tw(left_title, 1.0, 2) + 0.03)
          put(cap_x, band_top, cap, cex = 0.68, font = 2, col = "#111111")
          rmax_pdf <- max(140, ceiling(max(sp$score, na.rm = TRUE) / 10) * 10)
          draw_radar(cx = rad_cx, cy = band_top - 0.135, ry = 0.078,
                     labels = short_lab(sp$label), scores = sp$score,
                     rmax = rmax_pdf)
          text(rad_cx, band_top - 0.245, "gold dashes = cohort average (100)",
               cex = 0.5, col = "#777777")
          y <- min(y, band_top - 0.262)
        }
        y <- y - 0.014

        # --- Match log ------------------------------------------------------
        y <- sec(y, "MATCH LOG")
        cols <- c(0.04, 0.17, 0.37, 0.46, 0.58, 0.70, 0.84)
        hdr  <- c("Date", "Opponent", "Min", "Dist (m)", "m/min", "HSR (m)",
                  "A+D")
        for (j in seq_along(hdr))
          put(cols[j], y, hdr[j], cex = 0.66, font = 2, col = "#666666")
        y <- rule(y - th(0.66, 2), gap = 0.005)
        if (nrow(m) > 0) {
          # Cap rows so the wellness panel below always has room.
          room <- y - 0.20
          max_rows <- max(3, floor(room / 0.0175))
          ml <- m |> arrange(desc(date)) |> head(min(10, max_rows))
          for (i in seq_len(nrow(ml))) {
            vals <- c(format(ml$date[i], "%b %d"),
                      substr(coalesce(ml$opponent[i], "-"), 1, 18),
                      as.character(round(ml$match_minutes[i])),
                      format(round(ml$distance[i]), big.mark = ","),
                      sprintf("%.1f", ml$distance[i] / ml$match_minutes[i]),
                      format(round(ml$hsr_distance[i]), big.mark = ","),
                      as.character(ml$accels[i] + coalesce(ml$decels[i], 0)))
            for (j in seq_along(vals)) put(cols[j], y, vals[j], cex = 0.70)
            y <- y - 0.0175
          }
        }
        y <- y - 0.012

        # --- Wellness -------------------------------------------------------
        y <- sec(y, "READINESS - LAST 21 DAYS")
        if (nrow(wl) > 0) {
          wlo <- wl |> arrange(date)
          y <- draw_readiness(wlo, 0.10, 0.74, y, 0.105)
          last <- wlo |> slice_max(date, n = 1, with_ties = FALSE)
          y <- put(L, y, sprintf(
            "Latest %.0f%%  |  21-day mean %.0f%%  |  z-flags: %d  |  dashed line = 70%%",
            last$readiness, mean(wlo$readiness, na.rm = TRUE),
            sum(wlo$z_flag, na.rm = TRUE)), cex = 0.70)
          if (isTRUE(last$injury_flag))
            y <- put(L, y, paste("Reported:", last$aches), cex = 0.70,
                     col = "#B22222")
        } else {
          y <- put(0.06, y, "No wellness submissions.", cex = 0.76,
                   col = "#666666")
        }

        ver <- tryCatch(if (exists("APP_VERSION")) APP_VERSION else "",
                        error = function(e) "")
        text(L, 0.015, ascii(paste0(
          "Life University Rugby AMS  |  generated ",
          format(Sys.Date(), "%b %d, %Y"),
          if (nzchar(ver)) paste0("  |  build ", ver) else "")),
          adj = c(0, 0), cex = 0.58, col = "#888888")
      })
  })
}
