# ==============================================================================
# mod_availability.R -- Tab: Availability
#
# Populated from the staff availability Google Sheet (wide: athletes down,
# session dates across). Pick a date; each athlete shows the status from the
# sheet, an editable ATC notes field, and auto-generated training-load
# guidance that blends the status call with wellness + GPS data.
#
# Edits write back into that date's two columns only. Without a service
# account the board is read-only and says so -- staff edit the sheet directly.
# ==============================================================================

mod_availability_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center gap-2",
            span("Player availability board"),
            div(class = "d-flex align-items-center gap-2 flex-wrap",
                div(style = "width:170px;margin-bottom:0",
                    selectInput(ns("day"), NULL, choices = NULL,
                                width = "160px")),
                div(style = "width:180px;margin-bottom:0",
                    selectInput(ns("sort_by"), NULL,
                                choices = c("Sheet order" = "sheet",
                                            "Status (most restricted)" = "status",
                                            "Status (available first)" = "status_rev",
                                            "Name (A-Z)" = "name"),
                                selected = "sheet", width = "175px")),
                uiOutput(ns("summary_chips")),
                downloadButton(ns("export_pdf"), "PDF", class = "btn-sm")))
      ),
      div(class = "d-flex justify-content-between align-items-center",
          p(class = "text-muted small mb-1",
            "Status comes from the availability sheet; guidance folds in
             today's wellness, ACWR, and speed-vaccine state."),
          uiOutput(ns("save_note"))),
      uiOutput(ns("rows"))
    )
  )
}

mod_availability_server <- function(id, data, wellness_scored, vaccine) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Re-read on demand so sheet edits by staff show up on refresh.
    avail <- reactiveVal(NULL)
    observeEvent(data(), {
      avail(tryCatch(read_availability_sheet(),
                     error = function(e) empty_availability()))
    })

    observeEvent(avail(), {
      a <- avail()
      req(length(a$dates) > 0)
      d <- sort(a$dates)
      # Default to today, else the most recent session date that has passed.
      sel <- if (Sys.Date() %in% d) Sys.Date()
             else if (any(d <= Sys.Date())) max(d[d <= Sys.Date()])
             else min(d)
      updateSelectInput(session, "day",
                        choices = setNames(as.character(d),
                                           format(d, "%a %b %d")),
                        selected = as.character(sel))
    })

    # Athletes in sheet order, with the selected date's status/notes.
    day_board <- reactive({
      a <- avail(); req(!is.null(a), input$day)
      b <- a$board |> filter(date == as_date(input$day))
      validate(need(nrow(b) > 0, "No column for this date in the sheet."))
      b |>
        mutate(status = if_else(nzchar(status), status,
                                "Full Participation"),
               row_i = row_number())
    })

    # --- Monitoring context per athlete (feeds the guidance line) ------------
    ctx <- reactive({
      tw <- wellness_scored() |>
        filter(date == max(date)) |>
        select(athlete_name, readiness, readiness_z, soreness, aches)
      vc <- vaccine() |>
        transmute(athlete_name, vaccine_status = as.character(status))
      ac <- compute_acwr(data()$gps, "distance") |>
        group_by(athlete_name) |>
        slice_max(date, n = 1, with_ties = FALSE) |>
        ungroup() |>
        select(athlete_name, acwr)

      day_board() |>
        select(athlete_name) |>
        left_join(tw, by = "athlete_name") |>
        left_join(vc, by = "athlete_name") |>
        left_join(ac, by = "athlete_name")
    })

    editable <- reactive(can_write_sheets())

    # Most-restricted first; unknown statuses sort last.
    severity <- function(s) {
      m <- match(s, c("Injured", "Out", "Sick", "Off-Feet",
                      "Limited Running", "Non-Contact", "Full Participation"))
      coalesce(m, 99L)
    }

    apply_sort <- function(df, how) {
      switch(how %||% "sheet",
        status     = df |> arrange(severity(status), athlete_name),
        status_rev = df |> arrange(desc(severity(status)), athlete_name),
        name       = df |> arrange(athlete_name),
        df)
    }

    # Display order only. Input IDs stay keyed to the SHEET row (row_i), so
    # re-sorting the view can never misalign what gets written back.
    display_board <- reactive(apply_sort(day_board(), input$sort_by))

    # --- Player rows ---------------------------------------------------------
    output$rows <- renderUI({
      b <- display_board()
      ed <- editable()
      tagList(
        div(class = "avail-header d-flex gap-2 flex-wrap",
            span("Athlete", style = "min-width:150px;flex:0 0 150px"),
            span("Status", style = "min-width:170px;flex:0 0 170px"),
            span("ATC Notes", style = "min-width:200px;flex:0 0 200px"),
            span("Today's load guidance",
                 style = "flex:1 1 240px;min-width:240px")),
        lapply(seq_len(nrow(b)), function(i) {
          key <- paste0("r", b$row_i[i])   # sheet row, not display position
          div(class = "avail-row d-flex align-items-center gap-2 flex-wrap",
              span(b$athlete_name[i],
                   style = "min-width:150px;flex:0 0 150px;font-weight:600"),
              div(style = "min-width:170px;flex:0 0 170px",
                  if (ed)
                    selectInput(ns(paste0("st_", key)), NULL,
                                choices = AVAILABILITY_STATUSES,
                                selected = b$status[i], width = "160px")
                  else
                    span(b$status[i], style = paste0(
                      "font-weight:600;color:", status_colour(b$status[i])))),
              div(style = "min-width:200px;flex:0 0 200px",
                  if (ed)
                    textInput(ns(paste0("atc_", key)), NULL,
                              value = b$atc_notes[i],
                              placeholder = "ATC notes...", width = "190px")
                  else
                    span(class = "sug-text", b$atc_notes[i])),
              div(style = "flex:1 1 240px;min-width:240px",
                  uiOutput(ns(paste0("sug_", key)))))
        })
      )
    })
    outputOptions(output, "rows", suspendWhenHidden = FALSE)

    # Guidance line per row (keyed by sheet row, same as the inputs).
    observeEvent(day_board(), {
      b <- day_board()
      for (i_ in seq_len(nrow(b))) {
        local({
          i <- i_
          key <- paste0("r", b$row_i[i])
          output[[paste0("sug_", key)]] <- renderUI({
            st <- if (editable())
              input[[paste0("st_", key)]] %||% b$status[i] else b$status[i]
            cx <- ctx()[i, ]
            txt <- suggest_training_load(
              status = st,
              readiness = cx$readiness, readiness_z = cx$readiness_z,
              soreness = cx$soreness, aches = cx$aches,
              vaccine_status = cx$vaccine_status, acwr = cx$acwr)
            col <- if (st %in% c("Out", "Sick", "Injured")) AMS_COLORS$red
                   else if (st != "Full Participation") AMS_COLORS$gold
                   else NULL
            span(class = "sug-text", txt,
                 style = if (!is.null(col)) paste0("color:", col))
          })
        })
      }
    })

    # --- Current state (sheet values overridden by any in-app edits) ---------
    # ALWAYS in sheet row order -- this is what gets written back.
    statuses <- reactive({
      b <- day_board() |> arrange(row_i)
      ed <- editable()
      b |>
        mutate(
          status = vapply(seq_len(n()), function(i)
            if (ed) input[[paste0("st_r", row_i[i])]] %||% status[i]
            else status[i], character(1)),
          atc_notes = vapply(seq_len(n()), function(i)
            if (ed) input[[paste0("atc_r", row_i[i])]] %||% atc_notes[i]
            else atc_notes[i], character(1))
        )
    })

    output$summary_chips <- renderUI({
      st <- statuses()
      n_full <- sum(st$status == "Full Participation")
      n_out  <- sum(st$status %in% c("Out", "Sick", "Injured"))
      n_mod  <- nrow(st) - n_full - n_out
      div(
        span(class = "stat-chip-sm",
             style = paste0("background:", AMS_COLORS$primary, ";color:#0A0A0A"),
             paste("Full", n_full)),
        span(class = "stat-chip-sm",
             style = paste0("background:", AMS_COLORS$gold, ";color:#0A0A0A"),
             paste("Modified", n_mod)),
        span(class = "stat-chip-sm",
             style = paste0("background:", AMS_COLORS$red, ";color:white"),
             paste("Out", n_out))
      )
    })

    output$save_note <- renderUI({
      if (editable())
        span(class = "small", style = paste0("color:", AMS_COLORS$grey),
             "Edits save to the availability sheet")
      else
        span(class = "small", style = paste0("color:", AMS_COLORS$gold),
             "Read-only — edit the availability sheet directly")
    })

    # --- Write back the selected date's two columns --------------------------
    statuses_deb <- debounce(statuses, 1200)
    observeEvent(statuses_deb(), {
      req(editable())
      st <- statuses_deb()
      write_availability_day(avail(), as_date(input$day),
                             st$status, st$atc_notes)
    }, ignoreInit = TRUE)

    # --- PDF export ----------------------------------------------------------
    board <- reactive({
      # statuses() and ctx() are both in sheet order, so bind_cols aligns;
      # the printout then follows whatever sort is on screen.
      statuses() |>
        bind_cols(ctx() |> select(-athlete_name)) |>
        apply_sort(input$sort_by) |>
        mutate(guidance = vapply(seq_len(n()), function(i)
          suggest_training_load(
            status = status[i], readiness = readiness[i],
            readiness_z = readiness_z[i], soreness = soreness[i],
            aches = aches[i], vaccine_status = vaccine_status[i],
            acwr = acwr[i]), character(1)))
    })

    output$export_pdf <- downloadHandler(
      filename = function() paste0("availability-", input$day, ".pdf"),
      content = function(file) {
        b <- board()
        ascii <- function(x) {
          x <- gsub("≥", ">=", x); x <- gsub("—", "-", x)
          x <- gsub("·", "|", x)
          iconv(x, to = "ASCII//TRANSLIT", sub = "")
        }
        b$guidance     <- ascii(b$guidance)
        b$athlete_name <- ascii(b$athlete_name)
        b$atc_notes    <- ascii(coalesce(b$atc_notes, ""))

        n_full <- sum(b$status == "Full Participation")
        n_out  <- sum(b$status %in% c("Out", "Sick", "Injured"))
        n_mod  <- nrow(b) - n_full - n_out
        day_lbl <- format(as_date(input$day), "%A %B %d, %Y")

        grDevices::pdf(file, width = 8.5, height = 11)
        on.exit(grDevices::dev.off(), add = TRUE)
        col_x <- c(athlete = 0.00, status = 0.19, notes = 0.35,
                   guidance = 0.62)

        draw_header <- function() {
          par(mar = c(0.4, 0.6, 0.4, 0.6))
          plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
          y <- 0.98
          text(0, y, "Life University Rugby - Availability Board",
               adj = c(0, 1), cex = 1.35, font = 2)
          y <- y - 0.024
          text(0, y, day_lbl, adj = c(0, 1), cex = 0.9, col = "#444444")
          y <- y - 0.022
          text(0, y, sprintf("Full: %d   Modified: %d   Out: %d",
                             n_full, n_mod, n_out),
               adj = c(0, 1), cex = 0.9, col = "#222222", font = 2)
          y <- y - 0.030
          for (nm in names(col_x))
            text(col_x[[nm]], y,
                 c(athlete = "ATHLETE", status = "STATUS",
                   notes = "ATC NOTES",
                   guidance = "TODAY'S LOAD GUIDANCE")[[nm]],
                 adj = c(0, 1), cex = 0.75, font = 2, col = "#666666")
          y <- y - 0.012
          segments(0, y, 1, y, col = "#333333", lwd = 1.4)
          y - 0.012
        }

        y <- draw_header()
        for (i in seq_len(nrow(b))) {
          w_guid <- strwrap(b$guidance[i], width = 44)
          if (length(w_guid) == 0) w_guid <- ""
          w_note <- strwrap(b$atc_notes[i], width = 30)
          if (length(w_note) == 0) w_note <- ""
          n_lines <- max(length(w_guid), length(w_note), 1)
          row_h <- n_lines * 0.0155 + 0.010
          if (y - row_h < 0.03) y <- draw_header()

          text(col_x["athlete"], y, b$athlete_name[i], adj = c(0, 1),
               cex = 0.82, font = 2)
          text(col_x["status"], y, b$status[i], adj = c(0, 1), cex = 0.82,
               font = 2, col = status_colour(b$status[i], print = TRUE))
          for (j in seq_along(w_note))
            text(col_x["notes"], y - (j - 1) * 0.0155, w_note[j],
                 adj = c(0, 1), cex = 0.78, col = "#333333")
          for (j in seq_along(w_guid))
            text(col_x["guidance"], y - (j - 1) * 0.0155, w_guid[j],
                 adj = c(0, 1), cex = 0.78, col = "#222222")
          y <- y - row_h
          segments(0, y + 0.006, 1, y + 0.006, col = "#DDDDDD", lwd = 0.5)
        }
      })

    statuses
  })
}
