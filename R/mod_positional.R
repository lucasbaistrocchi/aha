# ==============================================================================
# mod_positional.R -- Tab 5: Positional Group Benchmarks
# Heatmapped reactable: each athlete's daily output as a ratio of (a) their
# cohort's average and (b) the 80-min Match Day Benchmark. Ratios make one
# glance answer "who over/under-shot their positional job today?"
# ==============================================================================

mod_positional_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center",
            span("Daily output vs cohort average & 80-min match benchmark"),
            div(class = "d-flex gap-2",
                selectInput(ns("day"), NULL, choices = NULL, width = "170px"),
                selectInput(ns("cohort"), NULL,
                            choices = "All cohorts", width = "160px"))
        )
      ),
      p(class = "text-muted small px-3",
        "Cell shading = % of Match Day Benchmark (darker = closer to a full
         80-min game output). Training days typically land 40-70% of MDB;
         a training day >85% MDB deserves a recovery conversation."),
      reactableOutput(ns("bench_table"))
    )
  )
}

mod_positional_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    observeEvent(data(), {
      days <- sort(unique(data()$gps$date), decreasing = TRUE)
      updateSelectInput(session, "day", choices = as.character(days))
      # Cohorts come from the data: 6 rugby cohorts with a full roster
      # mapping, Forwards/Backs with the raw GPS sheet.
      updateSelectInput(session, "cohort",
                        choices = c("All cohorts",
                                    sort(unique(data()$gps$position_group))))
    })

    output$bench_table <- renderReactable({
      req(input$day)
      day_data <- data()$gps |>
        filter(date == as_date(input$day)) |>
        {\(d) if (input$cohort != "All cohorts")
          filter(d, position_group == input$cohort) else d}()

      validate(need(nrow(day_data) > 0, "No sessions recorded on this day."))

      scored <- day_data |>
        mutate(ad = accels + coalesce(decels, 0)) |>
        group_by(position_group) |>
        mutate(grp_dist = mean(distance), grp_hsr = mean(hsr_distance),
               grp_ad = mean(ad), grp_hmld = mean(hmld)) |>
        ungroup() |>
        left_join(MATCH_BENCHMARKS, by = "position_group") |>
        transmute(
          Athlete = athlete_name, Cohort = position_group,
          `Dist (m)` = distance,
          `vs Cohort` = distance / grp_dist,
          `vs MDB` = distance / bm_distance,
          `HSR (m)` = hsr_distance,
          `HSR vs MDB` = hsr_distance / bm_hsr,
          `HMLD (m)` = hmld,
          `HMLD vs MDB` = hmld / bm_hmld,
          `A+D` = ad,
          `A+D vs MDB` = ad / bm_ad
        )
      if (!isTRUE(data()$has_hmld))
        scored <- select(scored, -starts_with("HMLD"))

      ratio_col <- function(name) colDef(
        name = name,
        cell = function(value) sprintf("%.0f%%", value * 100),
        style = function(value) heat_style(value)
      )

      cols <- list(
        `vs Cohort`   = ratio_col("vs Cohort Avg"),
        `vs MDB`      = ratio_col("Dist vs MDB"),
        `HSR vs MDB`  = ratio_col("HSR vs MDB"),
        `HMLD vs MDB` = ratio_col("HMLD vs MDB"),
        `A+D vs MDB`  = ratio_col("A+D vs MDB")
      )
      reactable(
        scored |> arrange(desc(`vs MDB`)),
        compact = TRUE, defaultPageSize = 20,
        defaultColDef = colDef(format = colFormat(separators = TRUE)),
        columns = cols[names(cols) %in% names(scored)],
        theme = ams_react_theme
      )
    })
  })
}
