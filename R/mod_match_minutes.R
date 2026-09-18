# ==============================================================================
# mod_match_minutes.R -- Tab 6: Match Minute Tracker & Conditioning Top-Up
# Single-match top-ups (prescribe_topup) + longitudinal exposure across the
# last N matches (compute_match_aggregate): 3 x 80 min = freshness problem,
# 3 x 30 min = match-fitness problem. Same table, opposite interventions.
# ==============================================================================

mod_match_minutes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 9),
      card(
        card_header("Match"),
        selectInput(ns("match_date"), "Match date", choices = NULL),
        sliderInput(ns("min_filter"), "Filter by minutes played",
                    min = 0, max = 80, value = c(0, 80)),
        helpText("Top-ups should run within 24 h of the match (Sunday am is
                  ideal) so the weekly loading rhythm holds for every squad
                  member, starter or not. Minutes come straight from the
                  match report's 'Mins Played'. The Tuesday −15% flag uses
                  top-quartile impact/accel load within each match.")
      ),
      card(
        card_header(
          div(class = paste("d-flex justify-content-between",
                            "align-items-center gap-2 flex-wrap"),
              span("Top-up prescriptions (selected match)"),
              downloadButton(ns("export_topup"), "PDF", class = "btn-sm"))
        ),
        reactableOutput(ns("topup_table"))
      )
    ),
    layout_columns(
      col_widths = c(3, 9),
      card(
        card_header("Longitudinal window"),
        sliderInput(ns("n_matches"), "Aggregate last N matches",
                    min = 2, max = 6, value = 3, step = 1),
        helpText("High exposure (≥75% of available minutes): manage
                  freshness. Low exposure (<40%): the athlete is slowly
                  de-training for match demands — conditioning must
                  substitute what selection is not providing.")
      ),
      card(
        card_header(
          div(class = paste("d-flex justify-content-between",
                            "align-items-center gap-2 flex-wrap"),
              span("Chronic match exposure — cumulative minutes & load"),
              downloadButton(ns("export_exposure"), "PDF", class = "btn-sm"))
        ),
        uiOutput(ns("exposure_ui")),
        reactableOutput(ns("exposure_table"))
      )
    )
  )
}

mod_match_minutes_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    matches <- reactive({
      data()$gps |> filter(session_type == "match")
    })

    observeEvent(data(), {
      mm <- matches() |> distinct(date, opponent) |> arrange(desc(date))
      labs <- ifelse(is.na(mm$opponent), as.character(mm$date),
                     paste0(format(mm$date, "%b %d"), " · ", mm$opponent))
      updateSelectInput(session, "match_date",
                        choices = setNames(as.character(mm$date), labs))
    })

    match_day <- reactive({
      req(input$match_date)
      matches() |>
        filter(date == as_date(input$match_date),
               match_minutes >= input$min_filter[1],
               match_minutes <= input$min_filter[2]) |>
        prescribe_topup()
    })

    output$topup_table <- renderReactable({
      tbl <- match_day() |>
        arrange(match_minutes) |>
        transmute(
          Athlete = athlete_name, Group = position_group,
          Minutes = match_minutes, Contacts = contacts, Accels = accels,
          Tier = tier,
          `Tuesday -15%` = reduce_tuesday,
          Prescription = prescription
        )

      reactable(
        tbl, compact = TRUE, striped = TRUE, defaultPageSize = 25,
        columns = list(
          Tier = colDef(cell = function(value) {
            col <- switch(value,
                          "High Top-Up" = AMS_COLORS$red,
                          "Moderate Top-Up" = AMS_COLORS$gold,
                          AMS_COLORS$primary)
            status_badge(col, value)
          }),
          `Tuesday -15%` = colDef(cell = function(value) {
            if (isTRUE(value)) status_badge(AMS_COLORS$red, "REDUCE") else "—"
          }, width = 110),
          Prescription = colDef(minWidth = 320)
        ),
        theme = ams_react_theme
      )
    })

    # Label for the selected match, reused by the exports.
    match_label <- reactive({
      req(input$match_date)
      d <- as_date(input$match_date)
      opp <- matches() |> filter(date == d) |> pull(opponent) |> unique()
      opp <- opp[!is.na(opp)]
      paste0(format(d, "%a %b %d, %Y"),
             if (length(opp)) paste0(" vs ", opp[1]) else "")
    })

    # ---- PDF: top-up prescriptions for the selected match -------------------
    output$export_topup <- downloadHandler(
      filename = function() paste0("topups-", input$match_date, ".pdf"),
      content = function(file) {
        d <- match_day() |> arrange(match_minutes)
        validate(need(nrow(d) > 0, "No athletes in this match."))

        cols <- list(
          list(label = "ATHLETE", x = 0.00, align = "left"),
          list(label = "GROUP",   x = 0.24, align = "left"),
          list(label = "MIN",     x = 0.50, align = "right"),
          list(label = "IMPACTS", x = 0.60, align = "right"),
          list(label = "A+D",     x = 0.68, align = "right"),
          list(label = "TIER",    x = 0.72, align = "left"),
          list(label = "TUE",     x = 0.99, align = "right"))

        n0 <- function(x) if (is.na(x)) "-" else
          format(round(x), big.mark = ",")
        rows <- lapply(seq_len(nrow(d)), function(i) c(
          d$athlete_name[i], as.character(d$position_group[i]),
          n0(d$match_minutes[i]), n0(d$contacts[i]),
          n0(d$accels[i] + coalesce(d$decels[i], 0)),
          d$tier[i],
          if (isTRUE(d$reduce_tuesday[i])) "-15%" else "-"))

        colour_fn <- function(i, j) {
          if (j == 6) return(switch(d$tier[i],
                                    "High Top-Up" = "#C0392B",
                                    "Moderate Top-Up" = "#B7950B",
                                    "#1E8449"))
          if (j == 7 && isTRUE(d$reduce_tuesday[i])) return("#C0392B")
          "#222222"
        }

        grDevices::pdf(file, width = 8.5, height = 11)
        on.exit(grDevices::dev.off(), add = TRUE)
        pdf_table(
          title = "Match Top-Up Prescriptions",
          subtitle = paste0(match_label(),
                            "  |  run within 24 h of the match"),
          cols = cols, rows = rows, colour_fn = colour_fn,
          note_fn = function(i) d$prescription[i])
      })

    # ---- Longitudinal aggregate (last N matches) -----------------------------
    exposure <- reactive({
      compute_match_aggregate(data()$gps, n_matches = input$n_matches %||% 3)
    })

    # Stacked bars: minutes contributed by each match, one column per player.
    output$exposure_ui <- renderUI({
      n <- tryCatch(nrow(exposure()), error = function(e) 0)
      div(style = "overflow-x:auto",
          plotlyOutput(session$ns("exposure_plot"), height = "440px",
                       width = paste0(max(680, n * 34), "px")))
    })

    output$exposure_plot <- renderPlotly({
      n <- input$n_matches %||% 3
      match_dates <- matches() |>
        distinct(date) |> slice_max(date, n = n) |> pull(date) |> sort()

      d <- matches() |>
        filter(date %in% match_dates) |>
        mutate(match_lbl = format(date, "%b %d"))

      order_df <- exposure()

      p <- plot_ly()
      pal <- colorRampPalette(c(AMS_COLORS$primary, AMS_COLORS$gold))(length(match_dates))
      for (i in seq_along(match_dates)) {
        di <- d |> filter(date == match_dates[i])
        p <- p |> add_bars(
          data = di,
          x = ~factor(athlete_name, levels = order_df$athlete_name),
          y = ~match_minutes,
          name = format(match_dates[i], "%b %d"),
          marker = list(color = pal[i],
                        line = list(color = "#0A0A0A", width = 1)))
      }
      p |> layout(
        barmode = "stack",
        xaxis = list(title = "", tickangle = -45, automargin = TRUE,
                     tickfont = list(size = 10)),
        yaxis = list(title = "Cumulative minutes", automargin = TRUE),
        legend = list(orientation = "h", y = -0.42, yanchor = "top", x = 0),
        shapes = list(list(
          type = "line", x0 = -0.5, x1 = nrow(order_df) - 0.5,
          y0 = 0.75 * length(match_dates) * THRESHOLDS$match_full_min,
          y1 = 0.75 * length(match_dates) * THRESHOLDS$match_full_min,
          line = list(dash = "dot", color = AMS_COLORS$red)))
      ) |>
        ams_plotly_layout(
          sprintf("Minutes across last %d matches (dotted = 75%% high-exposure line)",
                  length(match_dates)),
          margin_b = 160)
    })

    # ---- PDF: chronic match exposure ----------------------------------------
    output$export_exposure <- downloadHandler(
      filename = function()
        paste0("match-exposure-last", input$n_matches %||% 3, "-",
               Sys.Date(), ".pdf"),
      content = function(file) {
        d <- exposure()
        validate(need(nrow(d) > 0, "No match data."))
        has_h <- isTRUE(data()$has_hmld)

        # Right-aligned numerics end at their x; the trailing left-aligned
        # TIER must start early enough that "Moderate" still fits on-page.
        cols <- list(
          list(label = "ATHLETE",  x = 0.00, align = "left"),
          list(label = "GROUP",    x = 0.21, align = "left"),
          list(label = "APPS",     x = 0.42, align = "right"),
          list(label = "TOT MIN",  x = 0.51, align = "right"),
          list(label = "MEAN",     x = 0.58, align = "right"),
          list(label = "EXP %",    x = 0.66, align = "right"),
          list(label = "DIST",     x = 0.76, align = "right"),
          list(label = "HSR",      x = 0.84, align = "right"),
          list(label = "TIER",     x = 0.86, align = "left"))

        n0 <- function(x) if (is.na(x)) "-" else
          format(round(x), big.mark = ",")
        rows <- lapply(seq_len(nrow(d)), function(i) c(
          d$athlete_name[i], as.character(d$position_group[i]),
          as.character(d$matches_played[i]), n0(d$total_minutes[i]),
          n0(d$mean_minutes[i]), paste0(round(d$exposure_pct[i]), "%"),
          n0(d$total_distance[i]), n0(d$total_hsr[i]),
          sub(" exposure", "", d$exposure_tier[i])))

        # Exposure % and tier carry the on-screen colour meaning: red = high
        # (manage freshness), gold = low (match-fitness deficit).
        colour_fn <- function(i, j) {
          if (!j %in% c(6, 9)) return("#222222")
          p <- d$exposure_pct[i]
          if (p >= 75) "#C0392B" else if (p >= 40) "#1E8449" else "#B7950B"
        }

        grDevices::pdf(file, width = 8.5, height = 11)
        on.exit(grDevices::dev.off(), add = TRUE)
        pdf_table(
          title = "Chronic Match Exposure",
          subtitle = sprintf(
            "Last %d matches  |  %d athletes  |  >=75%% = manage freshness, <40%% = match-fitness deficit%s",
            input$n_matches %||% 3, nrow(d),
            if (has_h) "" else "  |  HMLD unavailable"),
          cols = cols, rows = rows, colour_fn = colour_fn,
          note_fn = function(i) d$implication[i])
      })

    output$exposure_table <- renderReactable({
      tbl <- exposure() |>
        transmute(
          Athlete = athlete_name, Group = position_group,
          Apps = matches_played,
          `Total min` = round(total_minutes),
          `Mean min` = round(mean_minutes),
          `Exposure %` = round(exposure_pct),
          `Match dist (m)` = round(total_distance),
          `Match HSR (m)` = round(total_hsr),
          `Match HMLD (m)` = round(total_hmld),
          Contacts = round(total_contacts),
          Tier = exposure_tier,
          Action = implication
        )

      reactable(
        tbl, compact = TRUE, striped = TRUE, defaultPageSize = 25,
        defaultColDef = colDef(format = colFormat(separators = TRUE)),
        columns = list(
          `Exposure %` = colDef(
            cell = function(value) sprintf("%d%%", value),
            style = function(value) {
              col <- if (value >= 75) AMS_COLORS$red
                     else if (value >= 40) AMS_COLORS$primary
                     else AMS_COLORS$gold
              list(color = col, fontWeight = 700)
            }),
          Tier = colDef(cell = function(value) {
            col <- switch(value,
                          "High exposure" = AMS_COLORS$red,
                          "Moderate exposure" = AMS_COLORS$primary,
                          AMS_COLORS$gold)
            status_badge(col, value)
          }, width = 150),
          Action = colDef(minWidth = 300)
        ),
        theme = ams_react_theme
      )
    })
  })
}
