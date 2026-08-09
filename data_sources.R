# ==============================================================================
# data_sources.R -- Catapult OpenField API + Google Sheets wellness ingestion
#
# DESIGN: every fetch_*() function first checks whether live credentials exist.
# If not, it transparently falls back to generate_dummy_*() so the app runs
# out-of-the-box. Live vs dummy is reported in the navbar so staff never
# mistake demo data for real data.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. GOOGLE SHEETS AUTH
# ------------------------------------------------------------------------------
# Reading link-shared sheets needs no login. WRITING (the availability board)
# needs a service account -- a robot Google identity whose key travels with
# the deployment, so no human ever has to click an OAuth prompt on a server.
#
# Set GS4_SERVICE_ACCOUNT_JSON to EITHER the key file's path (local dev) or
# the JSON text itself (Connect Cloud secret). Without it the app still runs:
# reads work, and availability falls back to a local CSV.
.sheet_auth <- new.env(parent = emptyenv())
.sheet_auth$state <- NULL   # NULL = untried, TRUE = service acct, FALSE = anon

ensure_sheet_auth <- function() {
  if (!is.null(.sheet_auth$state)) return(.sheet_auth$state)

  key <- Sys.getenv("GS4_SERVICE_ACCOUNT_JSON", "")
  if (!nzchar(key)) {
    gs4_deauth()
    .sheet_auth$state <- FALSE
    return(FALSE)
  }

  path <- if (file.exists(key)) key else {
    tmp <- tempfile(fileext = ".json")
    writeLines(key, tmp)
    tmp
  }
  ok <- tryCatch({
    gs4_auth(path = path)
    TRUE
  }, error = function(e) {
    warning("Service account auth failed, falling back to read-only: ",
            conditionMessage(e))
    gs4_deauth()
    FALSE
  })
  .sheet_auth$state <- ok
  ok
}

can_write_sheets <- function() isTRUE(ensure_sheet_auth())

# ------------------------------------------------------------------------------
# 1. CATAPULT OPENFIELD CLOUD API (httr2)
# ------------------------------------------------------------------------------
# Auth  : long-lived API token passed as a Bearer header.
#         Set once per machine:  Sys.setenv(CATAPULT_API_TOKEN = "xxx")
#         or better, put it in ~/.Renviron.
# Base  : region-specific. US = connect-us, EU = connect-eu, AU = connect-au.
# Flow  : /activities (list sessions in a date window)  ->
#         /activities/{id}/athletes/{id}/stats (per-athlete session stats)
# Dates : OpenField expects UNIX epoch seconds for startTime/endTime params.
CATAPULT_BASE <- Sys.getenv("CATAPULT_API_BASE",
                            "https://connect-us.catapultsports.com/api/v6")

catapult_token <- function() Sys.getenv("CATAPULT_API_TOKEN", "")

has_catapult <- function() nzchar(catapult_token())

catapult_req <- function(endpoint) {
  request(CATAPULT_BASE) |>
    req_url_path_append(endpoint) |>
    req_auth_bearer_token(catapult_token()) |>
    req_headers(Accept = "application/json") |>
    req_retry(max_tries = 3, backoff = ~ 2) |>   # polite retry on 429/5xx
    req_timeout(30)
}

# Pull activities (sessions) in a window, then per-athlete stats for each.
# Returns a tidy tibble: one row = one athlete-session.
fetch_catapult_sessions <- function(start_date, end_date) {
  # OpenField wants epoch seconds; lubridate makes the conversion explicit.
  start_epoch <- as.integer(as_datetime(start_date))
  end_epoch   <- as.integer(as_datetime(end_date) + days(1))

  activities <- catapult_req("activities") |>
    req_url_query(startTime = start_epoch, endTime = end_epoch) |>
    req_perform() |>
    resp_body_json(simplifyVector = TRUE)

  if (length(activities) == 0) return(tibble())

  # For each activity, request athlete stats. The `params` filter keeps the
  # payload lean -- ask only for what the dashboard consumes.
  wanted <- c("total_distance", "total_player_load", "max_vel",
              "gen2_acceleration_band2plus_total_effort_count",   # accels >2.5
              "gen2_deceleration_band2plus_total_effort_count",   # decels >2.5
              "high_speed_distance",                              # >5.0 m/s zone
              "metabolic_power_high_distance",                    # HMLD >25.5 W/kg
              "field_time")

  map_dfr(activities$id, function(aid) {
    stats <- catapult_req(paste0("activities/", aid, "/athletes")) |>
      req_url_query(params = paste(wanted, collapse = ",")) |>
      req_perform() |>
      resp_body_json(simplifyVector = TRUE)
    as_tibble(stats) |> mutate(activity_id = aid)
  }) |>
    transmute(
      date          = as_date(as_datetime(start_time %||% NA)),
      athlete_id    = as.character(athlete_id),
      athlete_name  = paste(first_name, last_name),
      session_type  = tags %||% "training",
      activity_tag  = tags %||% NA_character_,
      opponent      = NA_character_,
      distance      = total_distance,
      hsr_distance  = high_speed_distance,
      accels        = gen2_acceleration_band2plus_total_effort_count,
      decels        = gen2_deceleration_band2plus_total_effort_count,
      hmld          = metabolic_power_high_distance,
      max_vel       = max_vel,
      player_load   = total_player_load,
      duration_min  = field_time / 60
    )
}

# ------------------------------------------------------------------------------
# 1b. GPS GOOGLE SHEET (primary GPS source until Catapult API goes live)
# ------------------------------------------------------------------------------
# Match report export, one row per player-match. Columns (matched by name
# pattern, robust to reordering): Player Name | Date (ISO) | Opponent |
# Position (Back/Forward) | Acceleration/Deceleration Efforts | Mins Played |
# Duration (h:mm:ss) | Distance | Player Load | Max Velocity (m/s) |
# Max Vel (% Max) | High Metabolic Load Distance | Running/HI/Sprint Distance |
# High Speed Distance (= HI + Sprint) | Impacts | ... plus per-minute rates.
#
# Mapping notes:
# * Rich feed: PlayerLoad, HMLD, REAL minutes played, and Impacts (mapped to
#   contacts) are all present -- the app's full metric set lights up.
# * Vmax: "Max Vel (% Max)" implies an individual max on file with the vendor;
#   vmax = median(max_vel / pct_max) per athlete -- effectively recovering
#   their configured/tested max. Roster override still wins if provided.
# * Activity Tag drives session classification: "MD" = match day (the only
#   rows the Match Day / Match Minutes tabs analyse); every other tag
#   (e.g. "Pre-Season Day 0") is training. ALL tags feed weekly volume,
#   ACWR, and longitudinal tracking.
GPS_SHEET_DEFAULT <- "1VRHrsfxPC197OichZ7k7oMeaRiGzZV448wAs_QTWHt8"

gps_sheet_id <- function() Sys.getenv("GPS_SHEET_ID", GPS_SHEET_DEFAULT)
has_gps_sheet <- function() nzchar(gps_sheet_id())

# Map the sheet's Position labels (any casing/spelling variant) to the six
# rugby cohorts used across the app. Unrecognised labels fall back to the
# coarse Forwards/Backs buckets so nothing ever drops out of the dashboards.
map_position_to_cohort <- function(pos) {
  p <- str_squish(str_to_lower(as.character(pos)))
  case_when(
    p %in% c("prop", "hooker")                              ~ "Front Row",
    p %in% c("lock", "second row")                          ~ "Locks",
    p %in% c("loose forward", "back row", "flanker",
             "number 8", "no. 8", "no 8", "8th man")        ~ "Loose Forwards",
    p %in% c("scrum-half", "scrumhalf", "scrum half",
             "fly-half", "flyhalf", "fly half",
             "half-back", "halfback")                       ~ "Half-Backs",
    p %in% c("center", "centre", "midfield")                ~ "Midfield",
    p %in% c("wing", "winger", "fullback", "full-back",
             "full back", "back three")                     ~ "Back Three",
    str_detect(p, "back")                                   ~ "Backs",
    TRUE                                                    ~ "Forwards"
  )
}

# Name spellings that differ between the GPS sheet and the official roster.
# Canonical form = the roster spelling, so the override join lands.
NAME_ALIASES <- c(
  "Leo Keesler-Venables"   = "Leo Venables",
  "Shaun Matthysen"        = "Shaun Matthyssen",
  "Alai Uiasele"           = "Alai Uaisele",
  "Dominic Gigliotti"      = "Dom Gigliotti",
  "Nik Kehrer"             = "Nikolai Kehrer",
  # Spellings unique to the availability sheet
  "Aiden Gallant"          = "Aidan Gallant",
  "Charlie Humphreys"      = "Charlie Humphries",
  "Manuel Gonzales Deibe"  = "Manuel Gonzalez",
  "Wyatt Appelton"         = "Wyatt Appleton"
)

canonical_name <- function(x) {
  x <- str_squish(as.character(x))
  ifelse(x %in% names(NAME_ALIASES), NAME_ALIASES[x], x)
}

fetch_gps_sheet <- function() {
  ensure_sheet_auth()  # reads work anon or authed; never drop a service-account session
  raw <- read_sheet(gps_sheet_id(), .name_repair = "unique")

  num <- function(x) suppressWarnings(as.numeric(as.character(x)))

  # "1:19:20" (h:mm:ss) or "41:14" (mm:ss) -> minutes.
  dur_to_min <- function(x) {
    vapply(strsplit(as.character(x), ":"), function(p) {
      p <- suppressWarnings(as.numeric(p))
      if (length(p) == 0 || all(is.na(p))) return(NA_real_)
      p <- c(rep(0, max(0, 3 - length(p))), p)
      p[1] * 60 + p[2] + p[3] / 60
    }, numeric(1))
  }

  raw |>
    rename(
      athlete_raw  = matches("Player Name"),
      date_raw     = matches("^Date"),
      tag_raw      = matches("Activity Tag"),
      opponent_raw = matches("Opponent"),
      position_raw = matches("^Position"),
      accel_raw    = matches("^Acceleration Efforts"),
      decel_raw    = matches("^Deceleration Efforts"),
      mins_raw     = matches("Mins Played"),
      duration_raw = matches("^Duration"),
      dist_raw     = matches("^Distance$"),
      pl_raw       = matches("^Player Load$"),
      maxvel_raw   = matches("^Max Velocity"),
      pctmax_raw   = matches("Max Vel \\(% Max\\)"),
      hmld_raw     = matches("^High Metabolic Load Distance$"),
      hsd_raw      = matches("^High Speed Distance$"),
      sprint_raw   = matches("^Sprint Distance$"),
      impacts_raw  = matches("^Impacts$")
    ) |>
    filter(!is.na(athlete_raw), nzchar(as.character(athlete_raw))) |>
    transmute(
      date = as_date(as.character(date_raw)),
      athlete_name = canonical_name(athlete_raw),
      activity_tag = str_squish(as.character(tag_raw)),
      opponent = as.character(opponent_raw),
      position_group = map_position_to_cohort(position_raw),
      # Activity Tag is authoritative: ONLY "MD" rows are match day. Every
      # other tag (Pre-Season Day N, training, etc.) is training load --
      # counted in weekly volume and ACWR, excluded from match analysis.
      session_type = if_else(
        !is.na(activity_tag) &
          str_detect(activity_tag, regex("^md$|match", ignore_case = TRUE)),
        "match", "training"),
      mins = num(mins_raw),
      match_minutes = if_else(session_type == "match", mins, NA_real_),
      duration_min = coalesce(dur_to_min(duration_raw), mins),
      distance     = num(dist_raw),
      hsr_distance = num(hsd_raw),      # High Speed = HI + Sprint bands
      sprint_distance = num(sprint_raw),
      accels  = num(accel_raw),
      decels  = num(decel_raw),
      max_vel = num(maxvel_raw),        # already m/s
      pct_max = num(pctmax_raw),
      player_load = num(pl_raw),
      hmld = num(hmld_raw),
      contacts = num(impacts_raw)       # Impacts as collision-load proxy
    ) |>
    select(-mins) |>
    group_by(athlete_name) |>
    mutate(
      athlete_id = paste0("gps_", cur_group_id()),
      # One cohort per athlete (modal position) so a player listed at two
      # positions across matches doesn't split into two benchmark groups.
      position_group = names(which.max(table(position_group))),
      vmax_est = if_else(!is.na(pct_max) & pct_max > 0,
                         max_vel / (pct_max / 100), NA_real_),
      vmax = round(coalesce(
        suppressWarnings(median(vmax_est, na.rm = TRUE)),
        suppressWarnings(max(max_vel, na.rm = TRUE))), 2)
    ) |>
    ungroup() |>
    select(-pct_max, -vmax_est)
}

# Official roster override (data/roster.csv, generated from the roster
# workbook): columns athlete_name, position, position_group, vmax.
# The roster is authoritative for BOTH cohort and tested top speed --
# tested Vmax beats an observed session max, which is only ever a floor.
# Athletes in the GPS data but absent from the roster (e.g. departed
# players) keep their sheet-derived values rather than dropping out.
apply_roster_override <- function(gps) {
  path <- "data/roster.csv"
  if (!file.exists(path)) return(gps)
  ov <- tryCatch(readr::read_csv(path, show_col_types = FALSE),
                 error = function(e) NULL)
  if (is.null(ov) || !"athlete_name" %in% names(ov)) return(gps)

  # Keep only the override columns we know how to apply.
  ov <- ov |> select(any_of(c("athlete_name", "position",
                              "position_group", "vmax")))
  out <- gps |>
    left_join(ov, by = "athlete_name", suffix = c("", "_ov")) |>
    mutate(on_roster = athlete_name %in% ov$athlete_name)

  if ("position_group_ov" %in% names(out))
    out <- out |> mutate(position_group = coalesce(position_group_ov,
                                                   position_group))
  if ("vmax_ov" %in% names(out))
    out <- out |> mutate(vmax = coalesce(vmax_ov, vmax))

  out |> select(-ends_with("_ov"))
}

# ------------------------------------------------------------------------------
# 2. GOOGLE SHEETS WELLNESS (googlesheets4)
# ------------------------------------------------------------------------------
# Live source: team Google Form feeding the "WELLNESS" tab. Actual columns:
#   Timestamp | Your Name | Sleep Quantity | Sleep Quality (1-10) |
#   Energy Levels (1-10) | Soreness (1-10) | Stress (1-10) |
#   Aches/Pains | Treatment Plan | ATC Notes | Please list how severe
# Scale direction is MIXED: sleep quality & energy are "higher = better";
# soreness & stress are "higher = worse". compute_wellness_scores() handles
# the inversion -- keep raw values raw here (single source of truth).
WELLNESS_SHEET_DEFAULT <- "1i5qT-r4uJuTU3qd6vQK1ZuF97KrBWxATCbER_a4RcMo"
WELLNESS_TAB <- "WELLNESS"

wellness_sheet_id <- function()
  Sys.getenv("WELLNESS_SHEET_ID", WELLNESS_SHEET_DEFAULT)

has_wellness_sheet <- function() nzchar(wellness_sheet_id())

fetch_wellness <- function() {
  ensure_sheet_auth()  # reads work anon or authed
  raw <- read_sheet(wellness_sheet_id(), sheet = WELLNESS_TAB)

  # Rename by pattern, not position -- survives column reordering in the Form.
  raw |>
    rename(
      timestamp      = matches("Timestamp"),
      athlete_name   = matches("Name"),
      sleep_quantity = matches("Sleep Quantity"),
      sleep_quality  = matches("Sleep Quality"),
      energy         = matches("Energy"),
      soreness       = matches("Soreness"),
      stress         = matches("Stress"),
      aches          = matches("Aches"),
      treatment      = matches("Treatment"),
      atc_notes      = matches("ATC"),
      severity       = matches("severe")
    ) |>
    mutate(
      timestamp = if (inherits(timestamp, "POSIXt")) timestamp
                  else mdy_hms(as.character(timestamp)),
      date = as_date(timestamp),
      # Sheets can hand back list-columns on mixed input; coerce defensively.
      across(c(sleep_quantity, sleep_quality, energy, soreness, stress,
               severity),
             ~ suppressWarnings(as.numeric(as.character(.x)))),
      across(c(athlete_name, aches, treatment, atc_notes), as.character)
    ) |>
    # One row per athlete-day: keep the latest submission (form resubmits).
    group_by(athlete_name, date) |>
    slice_max(timestamp, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(date, athlete_name, sleep_quantity, sleep_quality, energy,
           soreness, stress, aches, treatment, atc_notes, severity)
}

# ------------------------------------------------------------------------------
# 2c. PERFORMANCE TESTING GOOGLE SHEET
# ------------------------------------------------------------------------------
# One row per athlete per testing session. Wide format: Athlete Name | Team |
# Position | anthropometry | CMJ (force-plate) | strength | IMTP columns.
# An optional "Date" / "Test Date" column supports repeat testing sessions;
# without it every row is treated as one session labelled "Current".
# Returned LONG: athlete_name, test_date, metric, value.
TESTING_SHEET_DEFAULT <- "1q3vOpuu2sudCZ44wB76YxvKWgoYX3xxr1oEwfYvk98Q"

testing_sheet_id <- function()
  Sys.getenv("TESTING_SHEET_ID", TESTING_SHEET_DEFAULT)

# Metric catalogue: display grouping + direction. `higher_better = FALSE`
# metrics (body fat, time-to-takeoff, time-to-peak-force) invert their
# rankings and percentiles so "good" is always the top of the chart.
TEST_METRICS <- tribble(
  ~metric,                      ~group,           ~unit,  ~higher_better,
  "Height (in)",                "Anthropometry",  "in",   TRUE,
  "Weight",                     "Anthropometry",  "lbs",  TRUE,
  "Lean Mass",                  "Anthropometry",  "lbs",  TRUE,
  "Body Fat %",                 "Anthropometry",  "%",    FALSE,
  "Chest (cm)",                 "Anthropometry",  "cm",   TRUE,
  "Bicep (cm)",                 "Anthropometry",  "cm",   TRUE,
  "Waist (cm)",                 "Anthropometry",  "cm",   FALSE,
  "Hip (cm)",                   "Anthropometry",  "cm",   TRUE,
  "Thigh (cm)",                 "Anthropometry",  "cm",   TRUE,
  "Jump Height (m)",            "Jump / CMJ",     "m",    TRUE,
  "Peak Braking Force (N)",     "Jump / CMJ",     "N",    TRUE,
  "Peak Propulsive Force (N)",  "Jump / CMJ",     "N",    TRUE,
  "Time To Takeoff",            "Jump / CMJ",     "s",    FALSE,
  "Peak Landing Force (N)",     "Jump / CMJ",     "N",    TRUE,
  "mRSI",                       "Jump / CMJ",     "",     TRUE,
  "Back Squat 3RM (lbs)",       "Strength",       "lbs",  TRUE,
  "Bench Press 3RM (lbs)",      "Strength",       "lbs",  TRUE,
  "Chin Up 1RM (lbs)",          "Strength",       "lbs",  TRUE,
  "SBJ (in)",                   "Power",          "in",   TRUE,
  "IMTP Peak Force (N)",        "IMTP",           "N",    TRUE,
  "Force at 100 ms (N)",        "IMTP",           "N",    TRUE,
  "Force at 200 ms (N)",        "IMTP",           "N",    TRUE,
  "Time to Peak Force (s)",     "IMTP",           "s",    FALSE,
  # Speed & conditioning -- all timed, so faster (lower) is better.
  # "Bronco (mm:ss)" is normalised to "Bronco" and converted to seconds.
  "10 m (s)",                   "Speed",          "s",    FALSE,
  "30 m (s)",                   "Speed",          "s",    FALSE,
  "Bronco",                     "Conditioning",   "s",    FALSE,
  "300mod #1 (s)",              "Conditioning",   "s",    FALSE,
  "300mod #2 (s)",              "Conditioning",   "s",    FALSE
)

# Metrics shown on the Individual Report percentile chart, in display order
# (top to bottom). A fixed set keeps reports comparable athlete to athlete.
REPORT_METRICS <- tribble(
  ~metric,                  ~label,
  "Jump Height (m)",        "CMJ Jump Height",
  "SBJ (in)",               "SBJ",
  "Bronco",                 "Bronco",
  "Back Squat 3RM (lbs)",   "Squat 3RM",
  "Bench Press 3RM (lbs)",  "Bench 3RM",
  "Chin Up 1RM (lbs)",      "Chin Up 1RM",
  "IMTP Peak Force (N)",    "IMTP Peak Force",
  "Body Fat %",             "Body Fat %"
)

fetch_testing <- function() {
  ensure_sheet_auth()  # reads work anon or authed
  raw <- read_sheet(testing_sheet_id(), .name_repair = "unique")
  names(raw) <- str_squish(names(raw))

  name_col <- names(raw)[str_detect(names(raw),
                                    regex("athlete|player", ignore_case = TRUE))][1]
  if (is.na(name_col)) stop("No athlete name column in testing sheet")

  date_col <- names(raw)[str_detect(names(raw),
                                    regex("^(test )?date", ignore_case = TRUE))][1]

  # Normalise any Bronco-ish column name ("Bronco Time", "Bronco (s)"...).
  bronco_col <- names(raw)[str_detect(names(raw),
                                      regex("bronco", ignore_case = TRUE))][1]
  if (!is.na(bronco_col)) {
    names(raw)[names(raw) == bronco_col] <- "Bronco"
    # Bronco is often logged mm:ss -- convert to seconds so it ranks.
    raw$Bronco <- vapply(as.character(raw$Bronco), function(v) {
      v <- str_squish(v)
      if (is.na(v) || !nzchar(v)) return(NA_real_)
      if (str_detect(v, ":")) {
        p <- suppressWarnings(as.numeric(strsplit(v, ":")[[1]]))
        if (length(p) < 2 || anyNA(p)) return(NA_real_)
        return(p[1] * 60 + p[2])
      }
      suppressWarnings(as.numeric(v))
    }, numeric(1))
  }

  present <- intersect(TEST_METRICS$metric, names(raw))
  if (length(present) == 0) stop("No recognised test metrics in testing sheet")

  # Dates must be a real Date type, not text: "5/1/2026" and "10/1/2026"
  # sort wrongly as strings, which would corrupt "latest session" logic.
  parse_test_date <- function(d) {
    if (inherits(d, "Date")) return(d)
    if (inherits(d, "POSIXt")) return(as_date(d))
    if (is.list(d)) d <- vapply(d, function(x)
      if (length(x) == 0) NA_character_ else as.character(x[1]),
      character(1))
    s <- str_squish(as.character(d))
    out <- suppressWarnings(mdy(s))                    # 5/1/2026
    out[is.na(out)] <- suppressWarnings(ymd(s[is.na(out)]))   # 2026-05-01
    out[is.na(out)] <- suppressWarnings(dmy(s[is.na(out)]))
    out
  }

  out <- raw |>
    mutate(
      athlete_name = canonical_name(.data[[name_col]]),
      test_date = if (!is.na(date_col)) parse_test_date(.data[[date_col]])
                  else as_date(NA)
    ) |>
    filter(!is.na(athlete_name), nzchar(athlete_name)) |>
    select(athlete_name, test_date, all_of(present)) |>
    mutate(across(all_of(present),
                  ~ suppressWarnings(as.numeric(as.character(.x))))) |>
    pivot_longer(all_of(present), names_to = "metric", values_to = "value") |>
    filter(!is.na(value))

  # Columns that EXIST in the sheet, including ones with no results logged
  # yet -- so a newly added test still appears in the Testing dropdown.
  attr(out, "available_metrics") <- present
  out
}

# Human label for a testing session date (NA -> "Undated").
test_date_label <- function(d) {
  ifelse(is.na(d), "Undated", format(d, "%b %d, %Y"))
}

# ------------------------------------------------------------------------------
# 2d. AVAILABILITY BOARD (staff-maintained Google Sheet)
# ------------------------------------------------------------------------------
# The board lives in a WIDE sheet the staff already fill in by hand:
#   row 1:  Athlete Name | Team | 8/10 | (blank) | 8/11 | (blank) | ...
#   rows 2+: one athlete per row
# Each session date owns TWO columns: the status, then an unlabelled column
# used for that day's ATC notes. Dates carry no year -- they belong to the
# pre-season, so PRESEASON_START's year is applied.
#
# The app reads this sheet as the source of truth and (with a service
# account) writes edits back into the exact cells, leaving the layout and
# every other date untouched.
AVAILABILITY_SHEET_DEFAULT <- "1YVfYxPCgoFm-FLuvqn71fQFJMXyChWbGqOrn9Q_HTQs"

availability_sheet_id <- function()
  Sys.getenv("AVAILABILITY_SHEET_ID", AVAILABILITY_SHEET_DEFAULT)

# Spreadsheet column number -> letter (1=A, 27=AA, 40=AN).
col_letter <- function(n) {
  s <- ""
  while (n > 0) {
    r <- (n - 1) %% 26
    s <- paste0(LETTERS[r + 1], s)
    n <- (n - 1) %/% 26
  }
  s
}

empty_availability <- function() list(
  board = tibble(athlete_name = character(), date = as_date(character()),
                 status = character(), atc_notes = character()),
  dates = as_date(character()),
  cols  = integer(),
  n_athletes = 0L,
  athletes = character()
)

# Returns the long board plus the geometry needed to write back.
read_availability_sheet <- function() {
  ensure_sheet_auth()
  raw <- tryCatch(
    read_sheet(availability_sheet_id(), sheet = 1, col_names = FALSE,
               col_types = "c"),
    error = function(e) NULL)
  if (is.null(raw) || nrow(raw) < 2) return(empty_availability())

  hdr <- as.character(unlist(raw[1, ]))
  # Date headers look like 8/10 or 08/10 (optionally with a year).
  is_date_hdr <- !is.na(hdr) & str_detect(str_squish(hdr),
                                          "^\\d{1,2}/\\d{1,2}(/\\d{2,4})?$")
  date_cols <- which(is_date_hdr)
  if (length(date_cols) == 0) return(empty_availability())

  yr <- format(PRESEASON_START, "%Y")
  parse_hdr <- function(h) {
    h <- str_squish(h)
    d <- suppressWarnings(mdy(if_else(str_count(h, "/") == 1,
                                      paste0(h, "/", yr), h)))
    d
  }
  dates <- parse_hdr(hdr[date_cols])

  body <- raw[-1, , drop = FALSE]
  names(body) <- paste0("V", seq_len(ncol(body)))
  athletes <- canonical_name(body[[1]])
  keep <- !is.na(athletes) & nzchar(str_squish(athletes))
  body <- body[keep, , drop = FALSE]
  athletes <- athletes[keep]

  cell <- function(j) {
    if (j > ncol(body)) return(rep(NA_character_, nrow(body)))
    v <- as.character(body[[j]])
    v[is.na(v)] <- ""
    str_squish(v)
  }

  board <- map_dfr(seq_along(date_cols), function(i) {
    j <- date_cols[i]
    tibble(athlete_name = athletes,
           date = dates[i],
           status = cell(j),
           # The unlabelled column immediately after each date holds notes.
           atc_notes = cell(j + 1))
  }) |>
    filter(!is.na(date))

  list(board = board, dates = dates, cols = date_cols,
       n_athletes = length(athletes), athletes = athletes)
}

# Write one date's statuses + notes back into their two columns, matching the
# sheet's existing row order. Only those cells are touched.
write_availability_day <- function(avail, day, status_vec, notes_vec) {
  if (!can_write_sheets()) return(FALSE)
  i <- which(avail$dates == day)
  if (length(i) != 1) return(FALSE)

  sc <- avail$cols[i]          # status column
  nc <- sc + 1                 # paired notes column
  n  <- avail$n_athletes
  rng <- function(col) sprintf("%s2:%s%d", col_letter(col),
                               col_letter(col), n + 1)

  tryCatch({
    range_write(availability_sheet_id(),
                data = tibble(x = as.character(status_vec)),
                sheet = 1, range = rng(sc),
                col_names = FALSE, reformat = FALSE)
    range_write(availability_sheet_id(),
                data = tibble(x = as.character(notes_vec)),
                sheet = 1, range = rng(nc),
                col_names = FALSE, reformat = FALSE)
    TRUE
  }, error = function(e) {
    warning("Availability write failed: ", conditionMessage(e))
    FALSE
  })
}

# ------------------------------------------------------------------------------
# 3. DUMMY DATA GENERATORS (deterministic seed -> reproducible demo)
# ------------------------------------------------------------------------------
DUMMY_SEED <- 42

generate_dummy_roster <- function() {
  set.seed(DUMMY_SEED)
  tribble(
    ~athlete_name,       ~position_group, ~jersey,
    "Tom Vaka",          "Front Row",     1,
    "Sione Latu",        "Front Row",     2,
    "Marcus Reid",       "Front Row",     3,
    "Jack O'Neill",      "Locks",    4,
    "Dan Whitfield",     "Locks",    5,
    "Liam Carter",       "Loose Forwards",      6,
    "Sam Tuilagi",       "Loose Forwards",      7,
    "Ben Aualiitia",     "Loose Forwards",      8,
    "Nate Brooks",       "Half-Backs",    9,
    "Kyle Mercer",       "Half-Backs",    10,
    "Jared Fine",        "Back Three",    11,
    "Owen Sisk",         "Midfield",      12,
    "Chris Nkosi",       "Midfield",      13,
    "Alex Duval",        "Back Three",    14,
    "Ryan Kepu",         "Back Three",    15,
    "Hugo Anders",       "Front Row",     16,
    "Pete Malone",       "Locks",    17,
    "Tomas Vega",        "Loose Forwards",      18,
    "Eli Waters",        "Half-Backs",    19,
    "Cole Bryant",       "Midfield",      20
  ) |>
    mutate(
      athlete_id = sprintf("ath_%02d", row_number()),
      # Individual all-time Vmax (m/s): backs faster than tight forwards.
      vmax = case_when(
        position_group %in% c("Front Row", "Locks") ~ runif(n(), 7.6, 8.4),
        position_group == "Loose Forwards"                     ~ runif(n(), 8.2, 8.9),
        position_group %in% c("Half-Backs", "Midfield")  ~ runif(n(), 8.7, 9.4),
        TRUE                                             ~ runif(n(), 9.0, 9.8)
      ) |> round(2)
    )
}

# 10 weeks of daily athlete-sessions: Tue/Thu train, Sat match, else off/gym.
generate_dummy_gps <- function(roster, weeks = 10) {
  set.seed(DUMMY_SEED + 1)
  end   <- Sys.Date()
  dates <- seq(end - weeks * 7 + 1, end, by = "day")

  grid <- expand_grid(date = dates, athlete_id = roster$athlete_id) |>
    left_join(roster, by = "athlete_id") |>
    mutate(
      dow = wday(date, week_start = 1),
      session_type = case_when(
        dow %in% c(2, 4) ~ "training",  # Tue / Thu field sessions
        dow == 6         ~ "match",     # Sat match day
        TRUE             ~ NA_character_
      )
    ) |>
    filter(!is.na(session_type))

  grid |>
    mutate(
      # Positional locomotor scalar: backs cover more HSR, tight five less.
      pos_scalar = case_when(
        position_group == "Front Row"  ~ 0.80,
        position_group == "Locks" ~ 0.88,
        position_group == "Loose Forwards"   ~ 1.00,
        position_group == "Half-Backs" ~ 1.10,
        position_group == "Midfield"   ~ 1.08,
        TRUE                           ~ 1.05
      ),
      is_match     = session_type == "match",
      match_minutes = if_else(is_match,
                              pmin(80, round(rnorm(n(), 61, 22))), NA_real_),
      match_minutes = if_else(is_match, pmax(8, match_minutes), NA_real_),
      minute_scalar = if_else(is_match, match_minutes / 80, 1),
      duration_min = if_else(is_match, match_minutes,
                             round(rnorm(n(), 75, 10))),
      distance = round((if_else(is_match, 6000, 4200) *
                          pos_scalar * minute_scalar) * rnorm(n(), 1, 0.10)),
      hsr_distance = round((if_else(is_match, 420, 260) *
                              pos_scalar^2 * minute_scalar) * rnorm(n(), 1, 0.25)),
      accels = round((if_else(is_match, 48, 30) * minute_scalar) *
                       rnorm(n(), 1, 0.20)),
      decels = round((if_else(is_match, 44, 26) * minute_scalar) *
                       rnorm(n(), 1, 0.20)),
      # HMLD scalar differs from locomotor scalar: tight forwards do MORE
      # relative high-metabolic work (repeat accels, scrums, cleanouts) than
      # their HSR profile suggests.
      hmld_scalar = case_when(
        position_group == "Front Row"  ~ 0.92,
        position_group == "Locks" ~ 0.96,
        position_group == "Loose Forwards"   ~ 1.12,
        position_group == "Half-Backs" ~ 1.05,
        position_group == "Midfield"   ~ 1.05,
        TRUE                           ~ 1.00
      ),
      hmld = round((if_else(is_match, 560, 320) * hmld_scalar * minute_scalar) *
                     rnorm(n(), 1, 0.18)),
      contacts = if_else(is_match,
                         round(pmax(0, rnorm(n(), 22, 8)) * minute_scalar *
                                 if_else(position_group %in%
                                           c("Front Row","Locks","Loose Forwards"),
                                         1.4, 0.8)), NA_real_),
      # Session max velocity: usually 78-95% of Vmax; occasional true exposure.
      max_vel = round(vmax * pmin(1, rbeta(n(), 8, 1.8)), 2),
      player_load = round(distance * 0.105 * rnorm(n(), 1, 0.06))
    ) |>
    mutate(across(c(distance, hsr_distance, accels, decels, hmld, player_load),
                  ~ pmax(0, .x))) |>
    mutate(opponent = if_else(session_type == "match",
                              "Scrimmage (demo)", NA_character_),
           activity_tag = if_else(session_type == "match", "MD", "Training")) |>
    select(date, athlete_id, athlete_name, position_group, vmax, session_type,
           activity_tag, opponent, duration_min, match_minutes, contacts,
           distance, hsr_distance, hmld, accels, decels, max_vel, player_load)
}

# Daily wellness mirroring the LIVE Form schema (1-10 scales, mixed
# direction, injury-report fields) so demo and live modes are drop-in
# interchangeable. Two athletes deliberately decline late so alert logic
# is visible out of the box.
generate_dummy_wellness <- function(roster, weeks = 10) {
  set.seed(DUMMY_SEED + 2)
  end   <- Sys.Date()
  dates <- seq(end - weeks * 7 + 1, end, by = "day")

  baselines <- roster |>
    transmute(athlete_id, athlete_name,
              base = runif(n(), 6.5, 8.5))   # athlete's normal (1-10 space)

  clamp10 <- function(x) pmin(10, pmax(1, round(x)))

  expand_grid(date = dates, athlete_id = roster$athlete_id) |>
    left_join(baselines, by = "athlete_id") |>
    mutate(
      # Post-match dip: Sunday wellbeing drops (match on Saturday).
      match_dip = if_else(wday(date, week_start = 1) == 7, -1.6, 0),
      # Two athletes trending down over the final 10 days -> demo red flags.
      decline = if_else(athlete_id %in% c("ath_06", "ath_13") &
                          date > max(date) - 10,
                        -as.numeric(date - (max(date) - 10)) * 0.25, 0),
      wellbeing = base + match_dip + decline,
      sleep_quantity = round(pmin(10, pmax(4, rnorm(n(), 7.8, 0.9))), 1),
      sleep_quality  = clamp10(wellbeing * 0.9 + rnorm(n(), 0, 1)),
      energy         = clamp10(wellbeing + rnorm(n(), 0, 1.1)),
      # Higher = WORSE for soreness/stress (matches the live Form).
      soreness       = clamp10(11 - wellbeing + rnorm(n(), 0, 1.2)),
      stress         = clamp10(11 - wellbeing * 0.8 + rnorm(n(), 0, 1) - 2),
      aches = case_when(
        athlete_id == "ath_06" & date > max(date) - 10 ~ "Hamstring",
        athlete_id == "ath_13" & date > max(date) - 10 ~ "Lower Back",
        soreness >= 8 & runif(n()) < 0.5               ~ "General soreness",
        TRUE                                           ~ "N/A"
      ),
      severity  = if_else(aches == "N/A", 0,
                          pmin(3, pmax(1, round(soreness / 3)))),
      treatment = if_else(aches == "N/A", "Nothing",
                          "Following assigned Rehab"),
      atc_notes = ""
    ) |>
    select(date, athlete_id, athlete_name, sleep_quantity, sleep_quality,
           energy, soreness, stress, aches, treatment, atc_notes, severity)
}

# ------------------------------------------------------------------------------
# 4. UNIFIED LOADERS (live if credentialed, else dummy)
# ------------------------------------------------------------------------------
# Source precedence for GPS: Catapult API (if token set) -> GPS Google Sheet
# -> dummy demo squad. Wellness: Google Sheet -> dummy. Capability flags tell
# modules which metrics this source actually provides, so missing metrics are
# hidden rather than rendered as misleading zeros.
load_all_data <- function(weeks = 10) {
  demo_roster <- generate_dummy_roster()
  gps_live <- FALSE

  gps <- if (has_catapult()) {
    tryCatch({
      g <- fetch_catapult_sessions(Sys.Date() - weeks * 7, Sys.Date()) |>
        left_join(demo_roster |> select(athlete_id, position_group, vmax),
                  by = "athlete_id")
      gps_live <- TRUE
      g
    }, error = function(e) {
      warning("Catapult fetch failed, using dummy data: ", conditionMessage(e))
      generate_dummy_gps(demo_roster, weeks)
    })
  } else if (has_gps_sheet()) {
    tryCatch({
      g <- fetch_gps_sheet()
      gps_live <- TRUE
      g
    }, error = function(e) {
      warning("GPS sheet fetch failed, using dummy data: ", conditionMessage(e))
      generate_dummy_gps(demo_roster, weeks)
    })
  } else generate_dummy_gps(demo_roster, weeks)

  gps <- apply_roster_override(gps)

  # Roster derives from whatever GPS source won.
  roster <- gps |> distinct(athlete_id, athlete_name, position_group, vmax)

  wellness_live <- FALSE
  wellness <- if (has_wellness_sheet()) {
    tryCatch({
      w <- fetch_wellness() |>
        left_join(roster |> select(athlete_id, athlete_name),
                  by = "athlete_name") |>
        # Names not (yet) in the roster mapping still need a stable grouping
        # key for within-athlete z-scores.
        mutate(athlete_id = coalesce(
          athlete_id, paste0("ws_", as.integer(factor(athlete_name)))))
      wellness_live <- TRUE
      w
    }, error = function(e) {
      warning("Sheets fetch failed, using dummy data: ", conditionMessage(e))
      generate_dummy_wellness(demo_roster, weeks)
    })
  } else generate_dummy_wellness(demo_roster, weeks)

  testing <- tryCatch(fetch_testing(), error = function(e) {
    warning("Testing sheet fetch failed: ", conditionMessage(e))
    tibble(athlete_name = character(), test_date = as_date(character()),
           metric = character(), value = numeric())
  })

  testing_metrics <- attr(testing, "available_metrics")
  if (is.null(testing_metrics)) testing_metrics <- unique(testing$metric)

  list(
    roster    = roster,
    gps       = gps,
    wellness  = wellness,
    testing   = testing,
    testing_metrics = testing_metrics,
    has_hmld  = any(!is.na(gps$hmld)),
    has_load  = any(!is.na(gps$player_load)),
    live      = c(gps = gps_live, wellness = wellness_live,
                  testing = nrow(testing) > 0)
  )
}
