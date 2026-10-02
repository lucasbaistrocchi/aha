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
        card_header(
          div(class = paste("d-flex justify-content-between",
                            "align-items-center gap-2 flex-wrap"),
              span("Controls"),
              downloadButton(ns("export_pdf"), "PDF", class = "btn-sm"))
        ),
        # Cohorts and individuals share one selector: the same ACWR and
        # weekly views work for either, so there's no reason to split them.
        selectInput(ns("athlete"), "Athlete or position group",
                    choices = NULL),
        selectInput(ns("load_metric"), "Load metric", choices = NULL),
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
      p(class = "text-muted small mb-1",
        sprintf(paste("Banded on the size of the change in either direction:",
                      "OK within ±%d%%, MONITOR ±%d-%d%%, HIGH beyond ±%d%%.",
                      "A sharp drop matters as much as a spike — planned",
                      "deloads will read MONITOR or HIGH by design."),
                round(THRESHOLDS$wow_jump_pct * 100),
                round(THRESHOLDS$wow_jump_pct * 100),
                round(THRESHOLDS$wow_watch_pct * 100),
                round(THRESHOLDS$wow_watch_pct * 100))),
      reactableOutput(ns("wow_table"))
    )
  )
}

mod_longitudinal_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    observeEvent(data(), {
      cohorts <- intersect(POSITION_GROUPS,
                           unique(data()$gps$position_group))
      extra <- setdiff(unique(data()$gps$position_group), POSITION_GROUPS)
      updateSelectInput(session, "athlete", choices = list(
        `Position groups` = c(cohorts, sort(extra)),
        Athletes = sort(unique(data()$gps$athlete_name))))
      # Only offer load metrics the current GPS source actually reports.
      metrics <- c("PlayerLoad" = "player_load", "Distance (m)" = "distance",
                   "HSR (m)" = "hsr_distance", "HMLD (m)" = "hmld")
      metrics <- metrics[vapply(metrics,
                                \(m) any(!is.na(data()$gps[[m]])),
                                logical(1))]
      updateSelectInput(session, "load_metric", choices = metrics)
    })

    # Is the current selection a cohort or an individual?
    is_cohort <- reactive({
      req(input$athlete)
      input$athlete %in% unique(data()$gps$position_group)
    })

    acwr_data <- reactive({
      req(input$load_metric)
      compute_acwr(data()$gps, load_col = input$load_metric)
    })

    output$acwr_plot <- renderPlotly({
      req(input$athlete, input$load_metric)
      d <- if (is_cohort())
        compute_cohort_acwr(data()$gps, input$athlete, input$load_metric)
      else
        acwr_data() |> filter(athlete_name == input$athlete)
      validate(need(!is.null(d) && nrow(d) > 0, "No data for this selection."))

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
        ams_plotly_layout(paste0("ACWR — ", input$athlete,
                                 if (is_cohort())
                                   " (per-athlete average)" else ""),
                          hovermode = "x unified", margin_b = 90,
                          margin_l = 64)
    })

    # --- Weekly breakdown for the selected athlete ---------------------------
    weekly_data <- reactive({
      req(input$athlete)
      if (is_cohort()) compute_cohort_weekly(data()$gps, input$athlete)
      else compute_athlete_weekly(data()$gps, input$athlete)
    })

    output$weekly_header <- renderUI({
      div(class = "d-flex justify-content-between align-items-center gap-2",
          span(paste0("Weekly breakdown",
                      if (!is.null(input$athlete) && nzchar(input$athlete))
                        paste0(" — ", input$athlete) else "")),
          if (isTRUE(try(is_cohort(), silent = TRUE)))
            span(class = "small", style = paste0("color:", AMS_COLORS$gold),
                 "per-athlete averages"))
    })

    output$athlete_weekly <- renderReactable({
      w <- weekly_data()
      validate(need(nrow(w) > 0, "No sessions recorded for this selection."))

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

    # Draw the ACWR series in base graphics on the open pdf() page, inside
    # the 0-1 coordinate space pdf_table() sets up. Returns the y to carry
    # on from, so the weekly table follows directly underneath.
    draw_acwr_panel <- function(d, y_top, metric_lbl) {
      if (is.null(d) || nrow(d) == 0) return(y_top)
      d <- d |> arrange(date)
      x0 <- 0.11; x1 <- 0.98
      yt <- y_top - 0.018
      yb <- yt - 0.235
      n  <- nrow(d)
      xs <- if (n > 1) x0 + (x1 - x0) * (seq_len(n) - 1) / (n - 1)
            else rep((x0 + x1) / 2, n)

      amax <- 2.2
      ay <- function(v) yb + (yt - yb) * pmin(pmax(v, 0), amax) / amax

      # 0.8-1.5 "sweet spot" band, then daily load as light bars behind.
      rect(x0, ay(THRESHOLDS$acwr_low), x1, ay(THRESHOLDS$acwr_high),
           col = "#EDEDED", border = NA)
      lmax <- suppressWarnings(max(d$daily_load, na.rm = TRUE))
      if (is.finite(lmax) && lmax > 0) {
        bh <- (yt - yb) * 0.34
        segments(xs, yb, xs, yb + bh * d$daily_load / lmax,
                 col = "#C9C9C9", lwd = 0.6)
      }
      for (v in c(0.5, 1.0, 1.5, 2.0)) {
        segments(x0, ay(v), x1, ay(v), col = "#E0E0E0", lwd = 0.4)
        text(x0 - 0.008, ay(v), sprintf("%.1f", v), adj = c(1, 0.5),
             cex = 0.55, col = "#666666")
      }
      segments(x0, yb, x1, yb, col = "#333333", lwd = 0.8)
      segments(x0, yb, x0, yt, col = "#333333", lwd = 0.8)

      ok <- !is.na(d$acwr)
      if (any(ok)) lines(xs[ok], ay(d$acwr[ok]), col = "#1E8449", lwd = 1.8)

      idx <- unique(round(seq(1, n, length.out = min(7, n))))
      for (i in idx) {
        segments(xs[i], yb, xs[i], yb - 0.005, col = "#333333", lwd = 0.6)
        text(xs[i], yb - 0.009, format(d$date[i], "%b %d"),
             adj = c(0.5, 1), cex = 0.55, col = "#666666")
      }
      text(x0, yt + 0.010, pdf_ascii(sprintf(
        "ACWR (line) and daily %s (bars); shaded band = %.1f-%.1f",
        metric_lbl, THRESHOLDS$acwr_low, THRESHOLDS$acwr_high)),
        adj = c(0, 0), cex = 0.6, col = "#555555")

      yb - 0.034
    }

    # --- PDF export: ACWR chart + weekly breakdown --------------------------
    output$export_pdf <- downloadHandler(
      filename = function()
        paste0("longitudinal-",
               gsub("[^A-Za-z0-9]+", "-", input$athlete %||% "selection"),
               "-", Sys.Date(), ".pdf"),
      content = function(file) {
        w <- weekly_data()
        has_h <- isTRUE(data()$has_hmld)
        # Keep the whole series for the chart, not just the latest value.
        acwr_series <- tryCatch({
          if (is_cohort())
            compute_cohort_acwr(data()$gps, input$athlete, input$load_metric)
          else acwr_data() |> filter(athlete_name == input$athlete)
        }, error = function(e) NULL)
        acwr_now <- {
          a <- if (is.null(acwr_series)) NULL else
            acwr_series |> filter(!is.na(acwr))
          if (!is.null(a) && nrow(a)) tail(a$acwr, 1) else NA_real_
        }
        metric_lbl <- names(which(
          c("PlayerLoad" = "player_load", "Distance (m)" = "distance",
            "HSR (m)" = "hsr_distance", "HMLD (m)" = "hmld") ==
            input$load_metric))[1] %||% input$load_metric

        num <- function(x) formatC(round(x), big.mark = ",", format = "d")
        pct <- function(x) if (is.na(x)) "-" else sprintf("%+.0f%%", x)
        lim <- THRESHOLDS$wow_jump_pct * 100

        cols <- list(
          list(label = "WEEK",  x = 0.00, align = "left"),
          list(label = "SESS",  x = 0.15, align = "left"),
          list(label = "TD (m)", x = 0.30, align = "right"),
          list(label = "d%",    x = 0.38, align = "right"),
          list(label = "HSR",   x = 0.52, align = "right"),
          list(label = "d%",    x = 0.60, align = "right"),
          list(label = "A+D",   x = 0.74, align = "right"),
          list(label = "d%",    x = 0.82, align = "right"))
        if (has_h) cols <- c(cols, list(
          list(label = "HMLD", x = 0.94, align = "right")))

        wd <- w |> arrange(desc(week))
        rows <- lapply(seq_len(nrow(wd)), function(i) {
          v <- c(format(wd$week[i], "%b %d"), as.character(wd$sessions[i]),
                 num(wd$td[i]), pct(wd$d_td[i]),
                 num(wd$hsr[i]), pct(wd$d_hsr[i]),
                 num(wd$ad[i]), pct(wd$d_ad[i]))
          if (has_h) v <- c(v, num(wd$hmld[i]))
          v
        })
        # Delta columns keep the on-screen colour coding.
        delta_j <- c(4, 6, 8)
        delta_src <- list(`4` = "d_td", `6` = "d_hsr", `8` = "d_ad")
        colour_fn <- function(i, j) {
          if (!j %in% delta_j) return("#222222")
          val <- wd[[delta_src[[as.character(j)]]]][i]
          if (is.na(val)) return("#999999")
          if (val > lim) "#C0392B" else if (val < -lim) "#B7950B"
          else "#1E8449"
        }

        grDevices::pdf(file, width = 8.5, height = 11)
        on.exit(grDevices::dev.off(), add = TRUE)
        pdf_table(
          title = paste0("Longitudinal — ", input$athlete),
          subtitle = sprintf(
            "%s | metric: %s | latest ACWR: %s | weeks Mon-Sun%s",
            if (is_cohort()) "Position group (per-athlete averages)"
              else "Individual athlete",
            metric_lbl,
            if (is.na(acwr_now)) "n/a" else sprintf("%.2f", acwr_now),
            sprintf("  |  OK within +/-%.0f%%, HIGH beyond +/-%.0f%%",
                    lim, THRESHOLDS$wow_watch_pct * 100)),
          cols = cols, rows = rows, colour_fn = colour_fn,
          header_draw = function(y)
            draw_acwr_panel(acwr_series, y, metric_lbl))
      })

    output$wow_table <- renderReactable({
      req(input$load_metric)
      wow <- compute_wow_change(data()$gps, load_col = input$load_metric) |>
        filter(week == max(week) | week == max(week) - 7) |>
        filter(week == max(week)) |>
        mutate(wow_pct = round(100 * wow_pct, 1)) |>
        arrange(desc(abs(wow_pct))) |>   # biggest movers, either direction
        select(Athlete = athlete_name, Group = position_group,
               `Weekly load` = weekly_load, `WoW %` = wow_pct) |>
        # Status carries the same number; the cell renderer turns it into a
        # band so the column has real data behind it.
        mutate(Status = `WoW %`)

      reactable(
        wow, compact = TRUE, striped = TRUE, defaultPageSize = 20,
        columns = list(
          `Weekly load` = colDef(format = colFormat(separators = TRUE)),
          `WoW %` = colDef(
            cell = function(value) {
              if (is.na(value)) "—" else sprintf("%+.1f%%", value)
            },
            style = function(value) {
              list(color = wow_band(value)$colour, fontWeight = 700)
            }),
          # Banded on MAGNITUDE, so a large drop is surfaced as well.
          Status = colDef(width = 110, cell = function(value) {
            b <- wow_band(value)
            status_badge(b$colour, b$label)
          })
        ),
        theme = ams_react_theme
      )
    })
  })
}
