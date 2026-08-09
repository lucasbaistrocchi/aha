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
