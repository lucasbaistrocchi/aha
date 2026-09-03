# ==============================================================================
# mod_home.R -- Tab 1: Home Dashboard & Daily Readiness
# ==============================================================================

mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box(title = "Squad Mean Readiness",
                value = textOutput(ns("vb_readiness")),
                theme = "primary", class = "vb-compact"),
      value_box(title = "Injured / Restricted",
                value = textOutput(ns("vb_injured")),
                theme = "secondary", class = "vb-compact"),
      value_box(title = "Weekly Distance vs Forecast",
                value = textOutput(ns("vb_distance")),
                theme = "success", class = "vb-compact"),
      value_box(title = "Speed Vaccine Red Flags",
                value = textOutput(ns("vb_vaccine")),
                theme = "danger", class = "vb-compact")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Quick Alerts — z < -1.5, soreness ≥7, or reported ache"),
        reactableOutput(ns("alert_table"))
      ),
      card(
        card_header("Today's Briefing — remaining load by positional group"),
        reactableOutput(ns("group_briefing")),
        uiOutput(ns("briefing_notes"))
      )
    ),
    card(
      card_header(
        div(class = paste("d-flex justify-content-between",
                          "align-items-center gap-2 flex-wrap"),
            span("Athlete detail by positional group"),
            div(class = "d-flex gap-2 flex-wrap",
                div(style = "width:190px;margin-bottom:0",
                    selectInput(ns("bar_cohort"), NULL, choices = NULL,
                                width = "180px")),
                div(style = "width:190px;margin-bottom:0",
                    selectInput(ns("bar_metric"), NULL,
                                choices = c("Distance (m)" = "distance",
                                            "HSR (m)" = "hsr_distance",
                                            "A+D efforts" = "ad",
                                            "HMLD (m)" = "hmld"),
                                width = "180px"))))
      ),
      p(class = "text-muted small mb-1",
        "This week's accumulated load per athlete. Dashed line = the cohort's
         forecast for the selected pre-season week; bars are gold where an
         athlete is under 70% of it."),
      uiOutput(ns("cohort_bar_ui"))
    )
  )
}

mod_home_server <- function(id, data, wellness_scored, vaccine, pre_week) {
  moduleServer(id, function(input, output, session) {

    today_wellness <- reactive({
      ws <- wellness_scored()
      ws |> filter(date == max(date))
    })

    group_progress <- reactive(
      compute_group_progress(data()$gps, pre_week()))

    output$vb_readiness <- renderText({
      sprintf("%.0f%%", mean(today_wellness()$readiness, na.rm = TRUE))
    })

    # Real medical signal from the Form: any athlete reporting an ache/pain
    # (non-"N/A") on today's submission counts as injured/restricted.
    output$vb_injured <- renderText({
      as.character(sum(today_wellness()$injury_flag, na.rm = TRUE))
    })

    output$vb_distance <- renderText({
      gp <- group_progress()
      sprintf("%.0f%%", 100 * sum(gp$acc_distance) / sum(gp$tg_distance))
    })

    output$vb_vaccine <- renderText({
      as.character(sum(vaccine()$status == "Red"))
    })

    output$alert_table <- renderReactable({
      alerts <- today_wellness() |>
        filter(z_flag | severe_sore | injury_flag) |>
        transmute(
          Athlete = athlete_name,
          `Readiness %` = round(readiness),
          `Z-Score` = round(readiness_z, 2),
          `Soreness (1-10)` = soreness,
          Reported = if_else(injury_flag, aches, "—"),
          Trigger = case_when(
            z_flag & severe_sore ~ "Z-flag + severe soreness",
            z_flag ~ "Z-score < -1.5",
            severe_sore ~ "Severe soreness (≥7)",
            TRUE ~ "Ache/pain reported"
          )
        ) |>
        arrange(`Z-Score`)

      reactable(
        alerts, compact = TRUE, striped = TRUE,
        defaultPageSize = 8,
        columns = list(
          `Z-Score` = colDef(style = function(value) {
            list(color = if (!is.na(value) && value < -1.5)
              AMS_COLORS$red else NULL, fontWeight = 600)
          }),
          Trigger = colDef(minWidth = 160)
        ),
        theme = ams_react_theme
      )
    })

    # Positional briefing: remaining weekly load per cohort, colour-coded so
    # the coach sees at a glance which unit is behind budget before today's
    # session plan is finalised.
    output$group_briefing <- renderReactable({
      gp <- group_progress() |>
        transmute(
          Cohort = as.character(position_group),
          `Dist rem (m)` = round(rem_distance),
          `HSR rem (m)` = round(rem_hsr),
          `A+D rem` = round(rem_ad),
          `HMLD rem (m)` = round(rem_hmld),
          `Forecast %` = pct_distance
        )
      if (!isTRUE(data()$has_hmld)) gp <- select(gp, -`HMLD rem (m)`)

      reactable(
        gp, compact = TRUE, defaultPageSize = 6,
        defaultColDef = colDef(format = colFormat(separators = TRUE)),
        columns = list(
          Cohort = colDef(style = list(fontWeight = 700,
                                       color = AMS_COLORS$primary)),
          `Forecast %` = colDef(
            cell = function(value) sprintf("%.0f%%", value),
            style = function(value) {
              col <- if (is.na(value) || value < 70) AMS_COLORS$red
                     else if (value < 90) AMS_COLORS$gold
                     else AMS_COLORS$primary
              list(color = col, fontWeight = 700)
            })
        ),
        theme = ams_react_theme
      )
    })

    # --- Athlete bars within a cohort ----------------------------------------
    observeEvent(data(), {
      groups <- intersect(POSITION_GROUPS,
                          unique(data()$gps$position_group))
      extra <- setdiff(unique(data()$gps$position_group), POSITION_GROUPS)
      updateSelectInput(session, "bar_cohort",
                        choices = c(groups, sort(extra)))
    })

    bar_data <- reactive({
      req(input$bar_cohort, input$bar_metric)
      win <- preseason_week_window(pre_week())
      d <- data()$gps |>
        filter(position_group == input$bar_cohort,
               date >= win[1], date <= win[2])
      validate(need(nrow(d) > 0,
                    "No sessions for this group in the selected week."))

      d |>
        mutate(ad = accels + coalesce(decels, 0)) |>
        group_by(athlete_name) |>
        summarise(value = sum(.data[[input$bar_metric]], na.rm = TRUE),
                  .groups = "drop") |>
        arrange(value)
    })

    # Per-athlete forecast for the selected metric = cohort benchmark x the
    # week multiplier, i.e. exactly the target the Weekly Load tab uses.
    bar_target <- reactive({
      bm <- MATCH_BENCHMARKS |>
        filter(position_group == input$bar_cohort)
      if (nrow(bm) != 1) return(NA_real_)
      mult <- WEEK_MULTIPLIERS[max(1, min(pre_week(),
                                          length(WEEK_MULTIPLIERS)))]
      switch(input$bar_metric,
             distance     = bm$bm_distance,
             hsr_distance = bm$bm_hsr,
             ad           = bm$bm_ad,
             hmld         = bm$bm_hmld,
             NA_real_) * mult
    })

    output$cohort_bar_ui <- renderUI({
      n <- tryCatch(nrow(bar_data()), error = function(e) 0)
      div(style = "max-height:560px;overflow-y:auto",
          plotlyOutput(session$ns("cohort_bar"),
                       height = bar_chart_height(n, px_per_row = 21,
                                                 min_px = 240)))
    })

    output$cohort_bar <- renderPlotly({
      d <- bar_data()
      tgt <- bar_target()
      lbl <- c(distance = "Distance (m)", hsr_distance = "HSR (m)",
               ad = "A+D efforts", hmld = "HMLD (m)")[[input$bar_metric]]

      d <- d |>
        mutate(col = if (is.na(tgt)) AMS_COLORS$primary
                     else if_else(value < 0.7 * tgt, AMS_COLORS$gold,
                                  AMS_COLORS$primary))

      p <- plot_ly(d, x = ~value,
                   y = ~factor(athlete_name, levels = athlete_name),
                   type = "bar", orientation = "h",
                   marker = list(color = ~col),
                   hovertemplate = paste0("%{y}: %{x:,.0f}",
                                          "<extra></extra>")) |>
        layout(xaxis = list(title = lbl, automargin = TRUE),
               yaxis = list(title = "", tickfont = list(size = 10),
                            automargin = TRUE),
               bargap = 0.25, showlegend = FALSE)

      if (is.finite(tgt))
        p <- p |> layout(shapes = list(list(
          type = "line", x0 = tgt, x1 = tgt,
          y0 = -0.5, y1 = nrow(d) - 0.5,
          line = list(dash = "dash", color = AMS_COLORS$red, width = 2))))

      p |> ams_plotly_layout(
        sprintf("%s — %s this week%s", input$bar_cohort, lbl,
                if (is.finite(tgt))
                  sprintf(" (forecast %s)",
                          format(round(tgt), big.mark = ",")) else ""),
        margin_l = 150)
    })

    output$briefing_notes <- renderUI({
      gp    <- group_progress()
      red_v <- vaccine() |> filter(status == "Red")
      behind <- gp |> filter(pct_distance < 70)

      tagList(
        if (nrow(behind) > 0) p(
          class = "mt-2 mb-1",
          strong("Units behind budget: ",
                 style = paste0("color:", AMS_COLORS$gold)),
          paste(behind$position_group, collapse = ", "),
          " — bias today's session design toward their remaining targets."),
        p(class = "mb-1 mt-2", strong("Speed exposure needed today:")),
        if (nrow(red_v) == 0) p("None — all athletes vaccinated.") else
          p(paste(red_v$athlete_name, collapse = ", "),
            style = paste0("color:", AMS_COLORS$red, ";font-weight:600")),
        p(class = "text-muted small mt-2",
          "Coach note: bank HSR early while athletes are fresh; chase accel
           and HMLD counts in small-sided games late.")
      )
    })
  })
}
