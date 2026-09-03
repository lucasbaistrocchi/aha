# ==============================================================================
# utils_metrics.R -- Sport science computation engine
# EWMA-ACWR, rolling wellness z-scores, readiness, speed vaccine status.
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. EWMA (Exponentially Weighted Moving Average)
# ------------------------------------------------------------------------------
# Williams et al. (2017, BJSM) argue EWMA beats rolling averages for ACWR
# because it weights recent load exponentially more -- the decay mirrors how
# fitness/fatigue effects actually dissipate.
#
#   lambda = 2 / (N + 1)          (N = chosen time constant in days)
#   EWMA_t = load_t * lambda + EWMA_(t-1) * (1 - lambda)
#
# Acute uses N = 7 (lambda ~= 0.25), chronic N = 28 (lambda ~= 0.069).
# ACWR = EWMA_acute / EWMA_chronic. Interpret as a *screening* flag, not a
# deterministic injury predictor (per the post-2019 ACWR critiques) -- it
# starts a conversation, it doesn't make the decision.
ewma_vec <- function(x, n_days) {
  lambda <- 2 / (n_days + 1)
  out <- numeric(length(x))
  if (length(x) == 0) return(out)
  out[1] <- x[1]
  for (i in seq_along(x)[-1]) {
    out[i] <- x[i] * lambda + out[i - 1] * (1 - lambda)
  }
  out
}

# Daily load series per athlete -> EWMA acute, chronic, ACWR.
# `gps` must have: date, athlete_id, athlete_name, position_group + load col.
compute_acwr <- function(gps, load_col = "player_load") {
  all_days <- seq(min(gps$date), max(gps$date), by = "day")

  gps |>
    group_by(athlete_id, athlete_name, position_group, date) |>
    summarise(daily_load = sum(.data[[load_col]], na.rm = TRUE),
              .groups = "drop") |>
    # Zero-fill non-training days: EWMA must decay through rest days,
    # otherwise chronic load is overestimated.
    complete(nesting(athlete_id, athlete_name, position_group),
             date = all_days, fill = list(daily_load = 0)) |>
    arrange(athlete_id, date) |>
    group_by(athlete_id, athlete_name, position_group) |>
    mutate(
      acute   = ewma_vec(daily_load, 7),
      chronic = ewma_vec(daily_load, 28),
      acwr    = if_else(chronic > 0, acute / chronic, NA_real_)
    ) |>
    ungroup()
}

# Week-over-week % change in total weekly load (pre-season ramp guardrail).
# Progressive overload should live in the 5-15% band; >15% jumps are flagged.
compute_wow_change <- function(gps, load_col = "distance") {
  gps |>
    mutate(week = floor_date(date, "week", week_start = 1)) |>
    group_by(athlete_id, athlete_name, position_group, week) |>
    summarise(weekly_load = sum(.data[[load_col]], na.rm = TRUE),
              .groups = "drop") |>
    arrange(athlete_id, week) |>
    group_by(athlete_id) |>
    mutate(
      wow_pct = (weekly_load - lag(weekly_load)) / lag(weekly_load),
      wow_flag = !is.na(wow_pct) & wow_pct > THRESHOLDS$wow_jump_pct
    ) |>
    ungroup()
}

# ------------------------------------------------------------------------------
# 2. WELLNESS: composite readiness + rolling 21-day z-scores
# ------------------------------------------------------------------------------
# Live Form schema: 1-10 scales with MIXED direction -- sleep quality & energy
# "higher = better"; soreness & stress "higher = worse". Composite readiness
# inverts the negative items (11 - x) so all four contribute in the same
# direction, then rescales to %. Sleep quantity (hours) is displayed but kept
# out of the composite: hours are a behaviour, the 1-10 items are perceptions.
#
# Z-scores are computed WITHIN athlete against their own rolling 21-day
# window: what matters is deviation from the athlete's own normal, not the
# squad's (a stoic prop's 5 may be another athlete's 8).
compute_wellness_scores <- function(wellness, window = 21) {

  roll_z <- function(x, w) {
    # z of today's value vs the athlete's PRIOR w-day mean/sd (today excluded,
    # so a bad day can't dilute its own reference distribution).
    n <- length(x)
    vapply(seq_len(n), function(i) {
      lo <- max(1, i - w); ref <- x[lo:(i - 1)]
      if (i < 8 || sd(ref, na.rm = TRUE) %in% c(0, NA)) return(NA_real_)
      (x[i] - mean(ref, na.rm = TRUE)) / sd(ref, na.rm = TRUE)
    }, numeric(1))
  }

  no_injury <- c("N/A", "n/a", "NA", "None", "none", "")

  wellness |>
    mutate(
      readiness = (sleep_quality + energy +
                     (11 - soreness) + (11 - stress)) / 40 * 100,
      injury_flag = !is.na(aches) & !(trimws(aches) %in% no_injury)
    ) |>
    arrange(athlete_id, date) |>
    group_by(athlete_id) |>
    mutate(
      readiness_z = roll_z(readiness, window),
      z_flag      = !is.na(readiness_z) &
                      readiness_z < THRESHOLDS$wellness_z_flag,
      severe_sore = !is.na(soreness) &
                      soreness >= THRESHOLDS$soreness_severe
    ) |>
    ungroup()
}

# ------------------------------------------------------------------------------
# 3. SPEED VACCINE STATUS
# ------------------------------------------------------------------------------
# For each athlete: days since last session where max_vel >= 90% of Vmax.
# Green <=5 d | Yellow 6-7 d | Red >7 d (or never exposed in window).
compute_speed_vaccine <- function(gps, as_of = max(gps$date)) {
  gps |>
    filter(date <= as_of) |>
    group_by(athlete_id, athlete_name, position_group, vmax) |>
    summarise(
      last_exposure = suppressWarnings(
        max(date[max_vel >= vmax * THRESHOLDS$vmax_pct], na.rm = TRUE)),
      best_recent_pct = max(max_vel / vmax, na.rm = TRUE) * 100,
      exposures_28d = sum(max_vel >= vmax * THRESHOLDS$vmax_pct &
                            date > as_of - 28),
      .groups = "drop"
    ) |>
    mutate(
      last_exposure = as_date(if_else(is.infinite(last_exposure),
                                      NA, last_exposure)),
      days_since = as.numeric(as_of - last_exposure),
      status = case_when(
        is.na(days_since)                        ~ "Red",
        days_since <= THRESHOLDS$vaccine_green   ~ "Green",
        days_since <= THRESHOLDS$vaccine_yellow  ~ "Yellow",
        TRUE                                     ~ "Red"
      ),
      status = factor(status, levels = c("Red", "Yellow", "Green"))
    ) |>
    arrange(status, desc(days_since))
}

# ------------------------------------------------------------------------------
# 3b. POSITIONAL WEEKLY PROGRESS vs PRE-SEASON FORECAST
# ------------------------------------------------------------------------------
# Forecasted weekly load per cohort = Target Match Load x pre-season week
# multiplier (both from the master database workbook; see global.R). Returns
# one row per cohort with accumulated / forecast / remaining / pct for TD,
# HSR, A+D (combined accel+decel efforts), and HMLD. Consumed by Weekly Load
# AND Home briefing so both screens always agree.
# Week windows are strict Monday-Sunday blocks counted from PRESEASON_START
# (Mon 10 Aug 2026). ALL activity tags count toward accumulated volume --
# match day and training alike.
preseason_week_window <- function(week_no) {
  start <- PRESEASON_START + (max(1, week_no) - 1) * 7
  c(start, start + 6)
}

# Attendance for one week: sessions an athlete appears in, over the number of
# session dates the squad ran that week. An athlete who missed half the week
# has a low weekly total for reasons of availability, not prescription --
# leaving them in the cohort aggregate drags the group's apparent completion
# down and makes a well-loaded unit look under-done.
compute_attendance <- function(gps, win, min_share = NULL) {
  # Default pulled at call time, with a literal fallback: a stale global.R on
  # a deployment must not take the whole Weekly Load / Home briefing down.
  if (is.null(min_share))
    min_share <- if (exists("ATTENDANCE_MIN")) ATTENDANCE_MIN else 0.70

  week_gps <- gps |> filter(date >= win[1], date <= win[2])
  n_dates  <- as.integer(n_distinct(week_gps$date))
  roster   <- gps |> distinct(athlete_id, athlete_name, position_group)

  attended <- week_gps |>
    group_by(athlete_id) |>
    summarise(sessions_attended = n_distinct(date), .groups = "drop")

  out <- roster |>
    left_join(attended, by = "athlete_id") |>
    mutate(sessions_attended = coalesce(as.integer(sessions_attended), 0L),
           sessions_possible = n_dates)

  # Plain assignment rather than if_else(): the condition here is a single
  # scalar while the columns are one-per-athlete, and older dplyr refuses to
  # recycle a length-1 condition against a length-n value.
  out$attendance <- if (n_dates > 0) out$sessions_attended / n_dates
                    else NA_real_
  out$meets_attendance <- !is.na(out$attendance) &
    out$attendance > min_share
  out
}

# `include_all = TRUE` ignores the attendance filter (used by views that
# should show the whole squad regardless).
compute_group_progress <- function(gps, week_no = 1, include_all = FALSE) {
  week_no <- max(1, min(week_no, length(WEEK_MULTIPLIERS)))
  mult <- WEEK_MULTIPLIERS[week_no]
  win  <- preseason_week_window(week_no)

  att <- compute_attendance(gps, win)
  n_dates <- if (nrow(att)) att$sessions_possible[1] else 0L

  # A week with no sessions logged yet (the usual state early in the week)
  # would fail every athlete's attendance test and empty the whole table.
  # In that case count everyone, so cohorts still show 0% against forecast.
  keep <- if (include_all || n_dates == 0) att$athlete_id
          else att$athlete_id[att$meets_attendance]

  week_gps <- gps |>
    filter(date >= win[1], date <= win[2], athlete_id %in% keep)

  # Targets scale to the athletes actually counted, so the cohort's
  # forecast and its accumulated load describe the same group of players.
  athlete_counts <- att |>
    filter(athlete_id %in% keep) |>
    count(position_group, name = "n_athletes")

  excluded_counts <- att |>
    filter(!athlete_id %in% keep) |>
    count(position_group, name = "n_excluded")

  week_gps |>
    group_by(position_group) |>
    summarise(acc_distance = sum(distance, na.rm = TRUE),
              acc_hsr      = sum(hsr_distance, na.rm = TRUE),
              acc_ad       = sum(accels, na.rm = TRUE) +
                               sum(decels, na.rm = TRUE),
              acc_hmld     = sum(hmld, na.rm = TRUE),
              .groups = "drop") |>
    right_join(athlete_counts, by = "position_group") |>
    left_join(excluded_counts, by = "position_group") |>
    left_join(MATCH_BENCHMARKS, by = "position_group") |>
    mutate(
      across(starts_with("acc_"), ~ coalesce(.x, 0)),
      n_excluded = coalesce(n_excluded, 0L),
      tg_distance = bm_distance * mult * n_athletes,
      tg_hsr      = bm_hsr      * mult * n_athletes,
      tg_ad       = bm_ad       * mult * n_athletes,
      tg_hmld     = bm_hmld     * mult * n_athletes,
      rem_distance = pmax(0, tg_distance - acc_distance),
      rem_hsr      = pmax(0, tg_hsr - acc_hsr),
      rem_ad       = pmax(0, tg_ad - acc_ad),
      rem_hmld     = pmax(0, tg_hmld - acc_hmld),
      pct_distance = 100 * acc_distance / tg_distance,
      pct_hsr      = 100 * acc_hsr / tg_hsr,
      pct_ad       = 100 * acc_ad / tg_ad,
      pct_hmld     = 100 * acc_hmld / tg_hmld
    ) |>
    mutate(position_group = factor(
      position_group,
      levels = union(POSITION_GROUPS, unique(position_group)))) |>
    arrange(position_group)
}

# ------------------------------------------------------------------------------
# 3b-ii. PER-ATHLETE WEEKLY BREAKDOWN
# ------------------------------------------------------------------------------
# Weekly totals for the load-budget metrics (TD, HSR, A+D, HMLD) plus
# week-over-week % change, for one athlete. Weeks are Monday-Sunday, matching
# the rest of the app. ALL activity tags count -- training and match day.
#
# Change is only computed against the IMMEDIATELY PRECEDING calendar week.
# The season has real gaps (e.g. April matches, then pre-season in August);
# comparing across a three-month break would manufacture a meaningless
# "+400%". Non-adjacent weeks return NA and display as "-".
compute_athlete_weekly <- function(gps, athlete) {
  d <- gps |> filter(athlete_name == athlete)
  if (nrow(d) == 0)
    return(tibble(week = as_date(character()), sessions = integer(),
                  td = numeric(), hsr = numeric(), ad = numeric(),
                  hmld = numeric(), d_td = numeric(), d_hsr = numeric(),
                  d_ad = numeric(), d_hmld = numeric()))

  pct_chg <- function(x, adjacent) {
    prev <- lag(x)
    if_else(adjacent & !is.na(prev) & prev > 0,
            100 * (x - prev) / prev, NA_real_)
  }

  d |>
    mutate(week = floor_date(date, "week", week_start = 1)) |>
    group_by(week) |>
    summarise(
      sessions = n(),
      td   = sum(distance, na.rm = TRUE),
      hsr  = sum(hsr_distance, na.rm = TRUE),
      ad   = sum(accels, na.rm = TRUE) + sum(decels, na.rm = TRUE),
      hmld = sum(hmld, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(week) |>
    mutate(adjacent = !is.na(lag(week)) &
             as.numeric(week - lag(week)) == 7) |>
    mutate(across(c(td, hsr, ad, hmld),
                  ~ pct_chg(.x, adjacent), .names = "d_{.col}")) |>
    select(-adjacent)
}

# ------------------------------------------------------------------------------
# 3b-iii. COHORT VIEWS (same shapes as the individual ones)
# ------------------------------------------------------------------------------
# Cohort figures are PER-ATHLETE AVERAGES, not cohort sums: divided by the
# cohort's roster size (a fixed denominator), so the numbers sit on the same
# scale as an individual athlete's and don't swing with squad availability.
cohort_daily_load <- function(gps, cohort, load_col = "player_load") {
  d <- gps |> filter(position_group == cohort)
  if (nrow(d) == 0) return(NULL)
  n_ath <- n_distinct(d$athlete_id)

  all_days <- seq(min(d$date), max(d$date), by = "day")
  d |>
    group_by(date) |>
    summarise(daily_load = sum(.data[[load_col]], na.rm = TRUE) / n_ath,
              .groups = "drop") |>
    complete(date = all_days, fill = list(daily_load = 0)) |>
    arrange(date)
}

# ACWR for a whole cohort, matching compute_acwr()'s output columns.
compute_cohort_acwr <- function(gps, cohort, load_col = "player_load") {
  series <- cohort_daily_load(gps, cohort, load_col)
  if (is.null(series)) return(NULL)
  series |>
    mutate(
      athlete_name   = cohort,
      position_group = cohort,
      acute   = ewma_vec(daily_load, 7),
      chronic = ewma_vec(daily_load, 28),
      acwr    = if_else(chronic > 0, acute / chronic, NA_real_)
    )
}

# Weekly breakdown for a cohort, same columns as compute_athlete_weekly().
compute_cohort_weekly <- function(gps, cohort) {
  d <- gps |> filter(position_group == cohort)
  if (nrow(d) == 0) return(compute_athlete_weekly(gps, "__none__"))
  n_ath <- n_distinct(d$athlete_id)

  pct_chg <- function(x, adjacent) {
    prev <- lag(x)
    if_else(adjacent & !is.na(prev) & prev > 0,
            100 * (x - prev) / prev, NA_real_)
  }

  d |>
    mutate(week = floor_date(date, "week", week_start = 1)) |>
    group_by(week) |>
    summarise(
      sessions = n_distinct(date),
      td   = sum(distance, na.rm = TRUE) / n_ath,
      hsr  = sum(hsr_distance, na.rm = TRUE) / n_ath,
      ad   = (sum(accels, na.rm = TRUE) + sum(decels, na.rm = TRUE)) / n_ath,
      hmld = sum(hmld, na.rm = TRUE) / n_ath,
      .groups = "drop"
    ) |>
    arrange(week) |>
    mutate(adjacent = !is.na(lag(week)) &
             as.numeric(week - lag(week)) == 7) |>
    mutate(across(c(td, hsr, ad, hmld),
                  ~ pct_chg(.x, adjacent), .names = "d_{.col}")) |>
    select(-adjacent)
}

# ------------------------------------------------------------------------------
# 3c. LONGITUDINAL MATCH EXPOSURE (last N matches)
# ------------------------------------------------------------------------------
# Chronic match exposure separates two very different athletes who look alike
# in a single-game view: 3 x 80 min carries accumulated fatigue + collision
# load (freshness problem); 3 x 30 min carries a match-conditioning deficit
# (fitness problem). Exposure tiers drive opposite interventions.
compute_match_aggregate <- function(gps, n_matches = 3) {
  match_dates <- gps |>
    filter(session_type == "match") |>
    distinct(date) |>
    slice_max(date, n = n_matches) |>
    pull(date)

  gps |>
    filter(session_type == "match", date %in% match_dates) |>
    group_by(athlete_id, athlete_name, position_group) |>
    summarise(
      matches_played = n(),
      total_minutes  = sum(match_minutes, na.rm = TRUE),
      mean_minutes   = mean(match_minutes, na.rm = TRUE),
      total_distance = sum(distance, na.rm = TRUE),
      total_hsr      = sum(hsr_distance, na.rm = TRUE),
      total_hmld     = sum(hmld, na.rm = TRUE),
      total_contacts = sum(contacts, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      available_min = length(match_dates) * THRESHOLDS$match_full_min,
      exposure_pct  = 100 * total_minutes / available_min,
      exposure_tier = case_when(
        exposure_pct >= 75 ~ "High exposure",
        exposure_pct >= 40 ~ "Moderate exposure",
        TRUE               ~ "Low exposure"
      ),
      implication = case_when(
        exposure_pct >= 75 ~
          "Manage freshness: trim mid-week volume, prioritise recovery",
        exposure_pct >= 40 ~
          "Balanced: standard weekly rhythm",
        TRUE ~
          "Match-fitness deficit: add game-intensity conditioning (HMLD/HSR top-ups)"
      )
    ) |>
    arrange(desc(exposure_pct))
}

# ------------------------------------------------------------------------------
# 3d. DAILY TRAINING-LOAD GUIDANCE (Availability tab)
# ------------------------------------------------------------------------------
# Merges the coach-set availability status with objective monitoring data
# (wellness z, soreness, ACWR, speed-vaccine state) into one actionable line
# per athlete. Priority order: medical status > wellness red flags > load
# state > speed exposure. Deliberately terse -- this is sideline language,
# not a report.
AVAILABILITY_STATUSES <- c("Full Participation", "Non-Contact",
                           "Limited Running", "Off-Feet", "Out",
                           "Sick", "Injured")

suggest_training_load <- function(status, readiness, readiness_z, soreness,
                                  aches, vaccine_status, acwr) {
  g <- function(x) if (length(x) == 0 || all(is.na(x))) NA else x[1]
  readiness <- g(readiness); readiness_z <- g(readiness_z)
  soreness <- g(soreness); aches <- g(aches)
  vaccine_status <- g(as.character(vaccine_status)); acwr <- g(acwr)

  if (status %in% c("Out", "Injured"))
    return("No team training — rehab / return-to-play pathway with medical")
  if (status == "Sick")
    return("No training — return once symptom-free 24 h, then graded re-entry")

  parts <- character(0)
  if (status == "Non-Contact")
    parts <- c(parts, "skills + running only, NO contact")
  if (status == "Limited Running")
    parts <- c(parts, "cap running volume, no HSR/sprint work")
  if (status == "Off-Feet")
    parts <- c(parts, "bike/pool conditioning only")

  if (!is.na(readiness_z) && readiness_z < THRESHOLDS$wellness_z_flag)
    parts <- c(parts, "reduce volume 25-30% (wellness z-flag)")
  else if (!is.na(readiness) && readiness < 60)
    parts <- c(parts, "reduce volume ~20% (low readiness)")

  if (!is.na(soreness) && soreness >= THRESHOLDS$soreness_severe)
    parts <- c(parts, "limit eccentric + collision load (severe soreness)")
  if (!is.na(aches) && !(trimws(aches) %in% c("N/A", "n/a", "None", "")))
    parts <- c(parts, paste0("monitor: ", aches))

  if (!is.na(acwr)) {
    if (acwr > THRESHOLDS$acwr_high)
      parts <- c(parts, sprintf("ACWR %.2f high — trim today's load", acwr))
    else if (acwr < THRESHOLDS$acwr_low)
      parts <- c(parts, sprintf("ACWR %.2f low — room to build", acwr))
  }

  if (!is.na(vaccine_status) && vaccine_status == "Red" &&
      !(status %in% c("Limited Running", "Off-Feet")))
    parts <- c(parts, "add ≥90% Vmax exposure in warm-up")

  if (length(parts) == 0) return("Full planned load")
  paste(parts, collapse = " · ")
}

# ------------------------------------------------------------------------------
# 4. MATCH-MINUTE TOP-UP ENGINE
# ------------------------------------------------------------------------------
# Non-starters and early substitutions accumulate a "match-day deficit". The
# top-up closes that gap within ~24 h so the weekly rhythm (and chronic load)
# survives selection decisions. High-minute + high-collision players get the
# opposite: a Tuesday volume reduction to protect recovery.
prescribe_topup <- function(match_df) {
  # Collision/accel flags are RELATIVE (top quartile of this match) rather
  # than absolute counts -- impact/effort scales differ across GPS vendors
  # and threshold configs, but "who took the most punishment" doesn't.
  q75 <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(Inf)   # metric absent -> nobody flagged
    quantile(x, 0.75)
  }
  contact_ref <- q75(match_df$contacts)
  accel_ref   <- q75(match_df$accels)

  match_df |>
    mutate(
      tier = case_when(
        match_minutes < 30                    ~ "High Top-Up",
        match_minutes < 60                    ~ "Moderate Top-Up",
        TRUE                                  ~ "Standard Recovery"
      ),
      reduce_tuesday = match_minutes > 70 &
        (coalesce(contacts, -Inf) >= contact_ref | accels >= accel_ref),
      prescription = case_when(
        tier == "High Top-Up" ~
          "4 x 100 m tempo (70-80% Vmax) + 6 high accels (>2.5 m/s2) + 2 x 40 m HSR builds",
        tier == "Moderate Top-Up" ~
          "2 x 100 m tempo + 4 high accels + 1 x 40 m HSR build",
        reduce_tuesday ~
          "Standard recovery; FLAG: -15% Tuesday volume (high minutes + collision load)",
        TRUE ~ "Standard recovery protocol (pool/bike flush, mobility)"
      )
    )
}
