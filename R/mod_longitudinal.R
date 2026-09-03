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

    # --- PDF export: weekly breakdown for the current selection -------------
    output$export_pdf <- downloadHandler(
      filename = function()
        paste0("longitudinal-",
               gsub("[^A-Za-z0-9]+", "-", input$athlete %||% "selection"),
               "-", Sys.Date(), ".pdf"),
      content = function(file) {
        w <- weekly_data()
        has_h <- isTRUE(data()$has_hmld)
        acwr_now <- tryCatch({
          a <- if (is_cohort())
            compute_cohort_acwr(data()$gps, input$athlete, input$load_metric)
          else acwr_data() |> filter(athlete_name == input$athlete)
          a <- a |> filter(!is.na(acwr))
          if (nrow(a)) tail(a$acwr, 1) else NA_real_
        }, error = function(e) NA_real_)

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
            names(which(c("PlayerLoad" = "player_load",
                          "Distance (m)" = "distance", "HSR (m)" = "hsr_distance",
                          "HMLD (m)" = "hmld") == input$load_metric))[1] %||%
              input$load_metric,
            if (is.na(acwr_now)) "n/a" else sprintf("%.2f", acwr_now),
            sprintf("  |  change flagged beyond +/-%.0f%%", lim)),
          cols = cols, rows = rows, colour_fn = colour_fn)
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
