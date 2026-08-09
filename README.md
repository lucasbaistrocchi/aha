# Rugby Union AMS — Shiny Athlete Management System

Modular R Shiny dashboard for collegiate rugby union integrating **Catapult
OpenField** GPS data and **Google Sheets** daily wellness. Runs out-of-the-box
on a reproducible dummy squad (20 athletes, 10 weeks) when no credentials are
present — the navbar badges show `DEMO` vs `LIVE` per source.

## Quick start

```r
install.packages(c("shiny", "bslib", "tidyverse", "plotly", "reactable",
                   "httr2", "googlesheets4", "lubridate"))
shiny::runApp("rugby-ams")
```

## Data sources

Both team Google Sheets are baked in as defaults, so the app boots LIVE with
no configuration. Precedence for GPS: Catapult API (if token set) → GPS
Google Sheet → demo data. Overrides via `~/.Renviron`:

```
CATAPULT_API_TOKEN=your_openfield_token        # switches GPS to the API
CATAPULT_API_BASE=https://connect-us.catapultsports.com/api/v6
GPS_SHEET_ID=...        # defaults to the team GPS export sheet
WELLNESS_SHEET_ID=...   # defaults to the team wellness Form sheet
```

### GPS sheet mapping notes

The sheet is a match report export (one row per player-match) and provides
nearly the full metric set: Distance, High Speed Distance (HSR), HMLD,
PlayerLoad, real Mins Played, accel/decel efforts, Max Velocity (m/s), and
Impacts (mapped to contacts for the collision-load flag). Vmax per athlete
is recovered from "Max Vel (% Max)" — i.e. the individual max already
configured with the vendor — via `median(max_vel / pct_max)`. The Match Day
tab compares each athlete's per-minute rates against his position-group
peers in that match and against the 80-min benchmarks (peak-demand windows
require an API data stream unavailable on the current subscription, so they
are not part of the app). The sheet is currently **matches-only**, so weekly
budgets and ACWR reflect match load alone until training exports are added
(rows with a blank or "Training" opponent are treated as training
automatically). Match selectors are labelled with opponents. The Tuesday
−15% flag uses top-quartile impact/accel load within each match rather than
absolute counts, so it is robust to vendor threshold configs.

### Official roster & testing numbers

When available, create `data/roster.csv` with columns
`athlete_name, position_group, vmax` (six rugby cohorts; tested Vmax in
m/s). It overrides the sheet's coarse Forwards/Backs grouping and
observed-max Vmax per athlete, unlocking the six-cohort benchmarks.

Wellness reads the team Google Form sheet (tab `WELLNESS`) by default — the
sheet ID is baked in as a fallback, `WELLNESS_SHEET_ID` overrides it. Columns:
`Timestamp | Your Name | Sleep Quantity | Sleep Quality (1-10) |
Energy Levels (1-10) | Soreness (1-10) | Stress (1-10) | Aches/Pains |
Treatment Plan | ATC Notes | severity`. Soreness & stress are
**higher = worse**; the composite readiness score inverts them (`11 − x`).
Latest submission per athlete-day wins; columns are matched by name pattern,
so reordering the Form won't break ingestion. Ache/pain reports (non-"N/A")
drive the Injured/Restricted count and Quick Alerts.

## Project structure

```
rugby-ams/
├── app.R                  # navbar UI + server wiring, shared reactives
└── R/
    ├── global.R           # packages, thresholds, cohorts, MDB benchmarks, theme
    ├── data_sources.R     # Catapult httr2 client, Sheets reader, dummy generators
    ├── utils_metrics.R    # EWMA-ACWR, z-scores, speed vaccine, top-up engine
    └── mod_*.R            # one Shiny module per tab (ui + server pairs)
```

## Operational definitions (edit in `R/global.R`)

| Metric | Threshold |
|---|---|
| HSR | > 5.0 m/s (18 km/h) |
| Accel / Decel | > 2.5 m/s² |
| HMLD | metabolic power > 25.5 W/kg |
| Speed vaccine dose | ≥ 90% individual Vmax |
| Vaccine traffic light | Green ≤ 5 d · Yellow 6–7 d · Red > 7 d |
| Wellness red flag | rolling 21-d z < −1.5 or soreness ≤ 2 |
| WoW jump flag (pre-season) | > 20% |
| ACWR band | 0.8 – 1.5 (EWMA, 7 d / 28 d) |

Match-day 80-min benchmarks per cohort live in `MATCH_BENCHMARKS` — replace
the literature-derived seeds with your own season medians as data accumulates.

## Performance Testing

Reads the testing Google Sheet (40 athletes; anthropometry, CMJ force-plate,
strength, SBJ, IMTP). Pick a test and a testing date — or **All-time best**,
which takes each athlete's best result across sessions. Shows a ranked
athlete bar chart coloured by cohort with the squad mean marked, a cohort
comparison (means plus individual points), and a detail table with values,
delta vs cohort mean, and squad/cohort percentiles. Direction-aware: for
body fat, time-to-takeoff, and time-to-peak-force, *lower* scores rank
higher. `TESTING_SHEET_ID` overrides the baked-in sheet; add a `Date` column
to the sheet and repeat sessions populate the date dropdown automatically.

## Individual Report

One-page athlete summary combining all three data streams: match counts and
minutes, top speed, speed-vaccine status, per-minute match output vs the
cohort's 80-minute benchmark, testing percentile profile, full match log,
and a 21-day readiness trend with injury reports. **Export PDF** produces a
print-ready one-pager (base `pdf()` device — no pandoc or headless browser
needed).

## Availability

The **Availability** tab lists every rostered player with an editable status
(Full Participation, Non-Contact, Limited Running, Off-Feet, Out, Sick,
Injured), a free-text **ATC Notes** field for the athletic trainer, and an
auto-generated daily load-guidance line that folds in wellness z-scores,
soreness, ache reports, distance ACWR, and speed-vaccine state.

Sort the board by sheet order, status (most restricted first or available
first), or name. Sorting is display-only: input fields stay bound to their
sheet rows, so re-sorting can never misalign what gets written back.

The board is populated from the staff **availability Google Sheet** — wide
format, athletes down the rows, session dates across the top, each date
owning a status column plus an unlabelled column beside it for that day's
ATC notes. Pick a date in the tab to load it. With a service account
configured, edits write straight back into those two columns and nothing
else is touched; without one the board is read-only and says so, so staff
edit the sheet directly. Undated headers take the pre-season year.
The PDF export covers all columns for the selected date.

## Positional targets & pre-season forecasts

All positional targets come from the coaches' master database workbook at
`data/2026-2027 LIFE U GPS Master Database.xlsx` ('Pre-Season Load' sheet):

- **Target Match Load** (per position: TD, HMLD, HSR, A+D, BIP m/min) drives
  the match-day benchmarks on the Positional and Match Day tabs. Cohort
  benchmarks are the mean of member positions (e.g. Front Row = Prop +
  Hooker); Forwards/Backs aggregates cover the GPS sheet's coarse grouping.
- **Weekly Progressions** multipliers (Weeks 1–6) drive the Weekly Load tab:
  forecasted weekly load per athlete = Target Match Load × week multiplier.
  Pick the pre-season week; gauges show each position group's distance
  progress vs forecast, with full metric breakdowns per group and athlete.

Weeks run **Monday–Sunday** from pre-season Week 1 = Mon 10 Aug 2026
(`PRESEASON_START` in `global.R`), through Week 6 = Mon 14 Sep 2026. The tab
opens on the week containing today. Cohorts are Front Row (props, hookers),
Locks, Loose Forwards, Half-Backs, Midfield, Back Three.

The roster at `data/roster.csv` (athlete_name, position, position_group,
vmax — generated from the roster workbook) is authoritative for cohort and
tested top speed; regenerate it when the roster changes. Athletes in the GPS
data but off the roster keep their sheet-derived values.

### Activity tags

The GPS sheet's **Activity Tag** column classifies sessions: `MD` = match
day (the only rows the Match Day and Match Minutes tabs analyse); every
other tag (e.g. `Pre-Season Day 0`) is training. **All** tags count toward
weekly volume, ACWR, and longitudinal tracking.

Update the workbook (keep the same layout) and restart the app to refresh
targets. If the file is missing, hardcoded copies of the current values
keep the app running. The Home briefing uses the same forecast via
`compute_group_progress`, so the two screens always agree. A+D = combined
acceleration + deceleration efforts, matching the workbook's definition.

## Longitudinal match exposure

Match Minutes aggregates the last N matches (2–6, default 3):
≥75% of available minutes = high exposure (freshness management, trim
mid-week volume); <40% = low exposure (match-fitness deficit — conditioning
must substitute what selection is not providing).

## Styling

Blackout theme: black canvas, lime green (#AAFF00) signal, gold (#D4AF37)
caution. Fonts: Bebas Neue headings, Rajdhani body (Google Fonts, fetched at
runtime). All colors centralised in `AMS_COLORS` (`global.R`).

## Notes for the practitioner

- **EWMA ACWR** (Williams et al. 2017): acute λ = 2/8, chronic λ = 2/29,
  zero-filled rest days so chronic load decays honestly. Treat excursions as
  conversation starters, not verdicts.
- **Wellness z-scores** compare each athlete to their *own* prior 21-day
  window (today excluded from its own reference).
- **Top-up engine**: <30 min → high top-up; 30–59 → moderate; ≥60 → recovery;
  >70 min + high contact/accel load → flag −15% Tuesday volume.
- The Catapult stats request filters to the exact `params` the dashboard
  consumes (accel/decel band-2+ counts map to the 2.5 m/s² threshold).
