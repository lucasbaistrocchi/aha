# ==============================================================================
# global.R -- Rugby Union Athlete Management System (AMS)
# Collegiate Rugby | Catapult OpenField GPS + Google Sheets Wellness
#
# Sport Science thresholds are defined ONCE here so every module inherits the
# same operational definitions (a CPSS best practice: one source of truth for
# metric definitions prevents "threshold drift" between reports).
# ==============================================================================

# Specific tidyverse packages rather than the tidyverse meta-package: the
# meta-package pulls ~100 dependencies, which on a free hosting tier means
# very slow (and more failure-prone) deploys. These are the ones actually used.
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(readr)
  library(tibble)
  library(plotly)
  library(reactable)
  library(httr2)
  library(googlesheets4)
  library(lubridate)
  library(readxl)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ------------------------------------------------------------------------------
# 1. METRIC THRESHOLDS (operational definitions)
# ------------------------------------------------------------------------------
# HSR      : >5.0 m/s (18 km/h). Absolute band -> forwards vs backs comparable.
# ACC/DEC  : >2.5 m/s^2 (OpenField band 2+).
# HMLD     : High Metabolic Load Distance -- metres covered with metabolic
#            power >25.5 W/kg (Osgnach/di Prampero model). Captures the
#            accel/decel/collision-adjacent work HSR misses: a prop can post
#            trivial HSR but massive HMLD. HSR = speed cost; HMLD = energy cost.
# SPEED VACCINE : exposure >= 90% of individual Vmax (hamstring "vaccination").
THRESHOLDS <- list(
  hsr_ms          = 5.0,    # m/s
  accel_ms2       = 2.5,    # m/s^2
  decel_ms2       = 2.5,    # m/s^2
  hmld_wkg        = 25.5,   # W/kg metabolic power floor defining HMLD
  vmax_pct        = 0.90,   # speed vaccine dose = 90% of individual Vmax
  vaccine_green   = 5,      # <=5 days since exposure  -> Green
  vaccine_yellow  = 7,      # 6-7 days                 -> Yellow, >7 -> Red
  wellness_z_flag = -1.5,   # rolling z below this = red-flag athlete
  soreness_severe = 7,      # >=7 on the Form's 1-10 scale (10 = worst)
  wow_jump_pct    = 0.20,   # >20% week-over-week jump flag (pre-season)
  acwr_high       = 1.5,    # classic "danger" ceiling
  acwr_low        = 0.8,    # under-training floor
  match_full_min  = 80      # rugby union match duration for MD benchmarks
)

# Minimum share of a week's sessions an athlete must attend before their load
# counts toward the COHORT target aggregation. Partial attendance is an
# availability story, not a prescription story; individual breakdowns still
# show everyone.
ATTENDANCE_MIN <- 0.70

# ------------------------------------------------------------------------------
# 2. RUGBY UNION POSITIONAL COHORTS & TARGETS (from the master database xlsx)
# ------------------------------------------------------------------------------
POSITION_GROUPS <- c("Front Row", "Locks", "Loose Forwards",
                     "Half-Backs", "Midfield", "Back Three")

# Pre-season week 1 begins Monday 10 Aug 2026; weeks run Mon-Sun throughout
# the app (floor_date(week_start = 1)). The Weekly Load tab derives the
# current pre-season week from this anchor.
PRESEASON_START <- as.Date("2026-08-10")

# Source of truth: data/2026-2027 LIFE U GPS Master Database.xlsx,
# 'Pre-Season Load' sheet. Two pieces:
#   * Target Match Load (A2:F11): per-position full-match outputs
#     (TD, HMLD, HSR, A+D = combined accel+decel efforts, BIP m/min)
#   * Weekly Progressions (H3:M3): pre-season week multipliers -- forecasted
#     weekly load = Target Match Load x multiplier for that week.
# Drop an updated workbook in data/ and restart to refresh; the hardcoded
# defaults below (a copy of the current workbook values) keep the app
# running if the file is missing or malformed.
FORECAST_XLSX <- "data/2026-2027 LIFE U GPS Master Database.xlsx"

TARGET_MATCH_LOAD_DEFAULT <- tribble(
  ~position,     ~td,  ~hmld, ~hsr, ~a_d, ~bip_mmin,
  "Prop",        5626,  391,   100,  31,   85,
  "Hooker",      5487,  364,   140,  30,   81,
  "Lock",        6309,  563,   284,  53,  103,
  "Back row",    6399,  698,   356,  71,  106,
  "Scrum-half",  6484, 1193,   819, 102,  106,
  "Fly-half",    6056,  661,   269,  87,   95,
  "Center",      6757, 1075,   733, 113,  112,
  "Wing",        7385, 1291,  1034, 105,  130,
  "Fullback",    7130, 1095,   806,  93,  121
)
WEEK_MULTIPLIERS_DEFAULT <- c(1.8, 2.2, 2.6, 2.4, 2.5, 2.6)

TARGET_MATCH_LOAD <- tryCatch({
  raw <- readxl::read_excel(FORECAST_XLSX, sheet = "Pre-Season Load",
                            range = "A2:F11")
  stopifnot(nrow(raw) == 9, ncol(raw) == 6)
  out <- tibble(
    position = as.character(raw[[1]]),
    td       = as.numeric(raw[[2]]),
    hmld     = as.numeric(raw[[3]]),
    hsr      = as.numeric(raw[[4]]),
    a_d      = as.numeric(raw[[5]]),
    bip_mmin = as.numeric(raw[[6]])
  )
  stopifnot(!anyNA(out$td))
  out
}, error = function(e) TARGET_MATCH_LOAD_DEFAULT)

WEEK_MULTIPLIERS <- tryCatch({
  m <- readxl::read_excel(FORECAST_XLSX, sheet = "Pre-Season Load",
                          range = "H3:M3", col_names = FALSE)
  m <- as.numeric(unlist(m))
  stopifnot(length(m) == 6, !anyNA(m))
  m
}, error = function(e) WEEK_MULTIPLIERS_DEFAULT)

# Position -> cohort mapping. Cohort benchmark = mean of member positions.
# Forwards/Backs are the coarse cohorts used while the GPS sheet only labels
# Forwards/Backs (upload data/roster.csv to unlock the six-cohort model).
COHORT_POSITIONS <- list(
  "Front Row"      = c("Prop", "Hooker"),
  "Locks"          = c("Lock"),
  "Loose Forwards" = c("Back row"),
  "Half-Backs"     = c("Scrum-half", "Fly-half"),
  "Midfield"       = c("Center"),
  "Back Three"     = c("Wing", "Fullback"),
  "Forwards"       = c("Prop", "Hooker", "Lock", "Back row"),
  "Backs"          = c("Scrum-half", "Fly-half", "Center", "Wing", "Fullback")
)

MATCH_BENCHMARKS <- purrr::map_dfr(names(COHORT_POSITIONS), function(g) {
  sub <- TARGET_MATCH_LOAD |> filter(position %in% COHORT_POSITIONS[[g]])
  tibble(position_group = g,
         bm_distance = round(mean(sub$td)),
         bm_hmld     = round(mean(sub$hmld)),
         bm_hsr      = round(mean(sub$hsr)),
         bm_ad       = round(mean(sub$a_d)),
         bm_mmin     = round(mean(sub$bip_mmin)))
})

# ------------------------------------------------------------------------------
# 3. THEME (bslib) -- blackout dashboard: black / lime green / gold
# ------------------------------------------------------------------------------
AMS_COLORS <- list(
  bg      = "#0A0A0A",  # near-black canvas
  card    = "#151515",  # card surface
  ink     = "#EDEDED",  # body text
  primary = "#AAFF00",  # lime green (signal / brand)
  gold    = "#D4AF37",  # gold (secondary accent / caution)
  green   = "#7CCF00",  # traffic light GO (lime-leaning)
  yellow  = "#D4AF37",  # traffic light CAUTION = gold
  red     = "#E5484D",  # traffic light STOP
  grey    = "#8A8A8A",
  grid    = "#262626"
)

ams_theme <- bs_theme(
  version      = 5,
  bg           = AMS_COLORS$bg,
  fg           = AMS_COLORS$ink,
  primary      = AMS_COLORS$primary,
  secondary    = AMS_COLORS$gold,
  success      = AMS_COLORS$green,
  warning      = AMS_COLORS$yellow,
  danger       = AMS_COLORS$red,
  base_font    = font_google("Rajdhani"),        # angular, athletic
  heading_font = font_google("Bebas Neue"),      # tall condensed display
  "card-border-radius" = "10px",
  "card-bg"    = AMS_COLORS$card,
  "navbar-bg"  = "#000000",
  # Bootstrap form-control SASS vars -- keeps native inputs on the dark canvas
  "input-bg"           = "#1A1A1A",
  "input-color"        = AMS_COLORS$ink,
  "input-border-color" = "#333333",
  "form-select-bg"     = "#1A1A1A",
  "dropdown-bg"        = "#161616",
  "dropdown-link-color" = AMS_COLORS$ink,
  "dropdown-link-hover-bg" = "#222222"
) |>
  # Shiny widgets (selectize, ionRangeSlider, datepicker) ship their own CSS
  # and ignore Bootstrap vars -- restyle them explicitly so nothing renders
  # as light text on a white control.
  bs_add_rules(sprintf("
    .card { border: 1px solid #232323; }
    .card-header { font-family: 'Bebas Neue'; font-size: 1.25rem;
                   letter-spacing: 0.06em; color: %1$s;
                   border-bottom: 2px solid %1$s; }
    .navbar { border-bottom: 2px solid %1$s; }
    h1,h2,h3,h4,h5 { letter-spacing: 0.05em; }
    .text-muted, .help-block, .shiny-input-container .help-text {
      color: #9A9A9A !important; }

    /* compact value boxes: big centred number, label as a subtitle under it.
       min-height (never max-height -- that clipped the label clean off). */
    .vb-compact { min-height: 104px; }
    .vb-compact .value-box-area {
      display: flex; flex-direction: column-reverse;
      justify-content: center; align-items: center;
      text-align: center; padding: 0.55rem 0.7rem; gap: 1px;
      overflow: visible; }
    .vb-compact .value-box-value {
      font-size: 2.7rem; line-height: 1.05; margin: 0;
      font-family: 'Bebas Neue'; letter-spacing: 0.02em; }
    .vb-compact .value-box-value .shiny-text-output { line-height: 1.05; }
    .vb-compact .value-box-title {
      font-size: 0.76rem; letter-spacing: 0.06em; line-height: 1.15;
      text-transform: uppercase; opacity: 0.9; margin: 0; }

    /* availability rows */
    .avail-row .form-group, .avail-row .shiny-input-container {
      margin-bottom: 0; }
    .avail-row { border-bottom: 1px solid #202020; padding: 5px 0;
      row-gap: 2px; }
    .avail-header { color: %1$s; font-family: 'Bebas Neue';
      letter-spacing: 0.05em; font-size: 1.05rem;
      border-bottom: 2px solid %1$s; padding-bottom: 4px;
      margin-top: 4px; row-gap: 2px; }
    /* guidance can be long -- wrap inside its column instead of pushing
       the row wider and shoving other columns off-screen */
    .sug-text { font-size: 0.85rem; color: #C9C9C9; line-height: 1.35;
      display: inline-block; overflow-wrap: anywhere; }

    /* cards: never let content butt against the edge */
    .card-body { padding-top: 0.6rem; }
    .card-header { padding-top: 0.45rem; padding-bottom: 0.35rem; }
    /* headers hold selectInputs in several tabs -- kill the stray margin
       that pushed the header taller than its text */
    .card-header .form-group,
    .card-header .shiny-input-container { margin-bottom: 0 !important; }

    /* tables: breathing room + wrapped long athlete names */
    .reactable { font-size: 0.88rem; }
    .rt-td, .rt-th { padding-top: 5px !important;
      padding-bottom: 5px !important; }
    .rt-td { overflow-wrap: anywhere; }

    /* plotly containers must not overflow their card */
    .js-plotly-plot, .plot-container { max-width: 100%%; }
    /* the page scrolls; nothing should be squeezed to fit the viewport */
    .container-fluid { padding-left: 0; padding-right: 0; }
    .html-fill-container > .html-fill-item { min-height: auto; }
    .stat-chip { display: inline-block; padding: 4px 14px;
      border-radius: 10px; font-weight: 700; margin-right: 8px; }
    .stat-chip-sm { display: inline-block; padding: 1px 9px;
      border-radius: 8px; font-weight: 700; font-size: 0.75rem;
      margin-right: 4px; }
    .avail-strip { font-size: 0.78rem; color: #9A9A9A;
      padding: 2px 0 8px 0; }

    /* selectize (selectInput) */
    .selectize-input {
      background: #1A1A1A !important; color: %2$s !important;
      border-color: #333 !important; box-shadow: none !important; }
    .selectize-input > input,
    .selectize-input .item { color: %2$s !important; }
    .selectize-dropdown {
      background: #161616 !important; color: %2$s !important;
      border-color: #333 !important; }
    .selectize-dropdown .option.active,
    .selectize-dropdown .option:hover {
      background: %1$s !important; color: #0A0A0A !important; }

    /* ionRangeSlider (sliderInput) */
    .irs--shiny .irs-line { background: #2A2A2A; border: none; }
    .irs--shiny .irs-bar { background: %1$s; border: none; }
    .irs--shiny .irs-handle { background: %1$s; border-color: %1$s;
      box-shadow: none; }
    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single {
      background: %1$s; color: #0A0A0A; font-weight: 700; }
    .irs--shiny .irs-min, .irs--shiny .irs-max {
      background: #222; color: #9A9A9A; }
    .irs--shiny .irs-grid-text { color: #7A7A7A; }

    /* bootstrap-datepicker (dateInput) */
    .datepicker-dropdown { background: #161616; color: %2$s;
      border: 1px solid #333; }
    .datepicker table tr td, .datepicker table tr th { color: %2$s; }
    .datepicker table tr td.day:hover,
    .datepicker table tr td.focused { background: #262626; }
    .datepicker table tr td.active,
    .datepicker table tr td.active:hover {
      background: %1$s !important; color: #0A0A0A !important;
      background-image: none !important; }
    .datepicker table tr td.old, .datepicker table tr td.new {
      color: #5A5A5A; }
    .input-group-text { background: #1A1A1A; color: %2$s;
      border-color: #333; }

    /* checkbox */
    .form-check-input { background-color: #1A1A1A; border-color: #444; }
    .form-check-input:checked { background-color: %1$s; border-color: %1$s; }
  ", AMS_COLORS$primary, AMS_COLORS$ink))

# ------------------------------------------------------------------------------
# 4. SHARED PLOT / TABLE HELPERS (dark-mode aware)
# ------------------------------------------------------------------------------
# hovermode defaults to "closest": "x unified" is only right for time series
# (pass it explicitly there) and produces unwieldy tooltips on grouped bars.
# margin_b: extra bottom room for rotated category labels; margin_l for long
# left-hand tick labels on horizontal bars.
ams_plotly_layout <- function(p, title = NULL, hovermode = "closest",
                              margin_b = 60, margin_l = 60, margin_t = 56) {
  ax <- list(gridcolor = AMS_COLORS$grid, zerolinecolor = AMS_COLORS$grid)
  p |> layout(
    title = list(text = title, x = 0, xanchor = "left", y = 0.98,
                 font = list(family = "Rajdhani", size = 15,
                             color = AMS_COLORS$ink)),
    font = list(family = "Rajdhani", color = AMS_COLORS$ink),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    xaxis = ax, yaxis = ax,
    hovermode = hovermode,
    hoverlabel = list(bgcolor = "#1E1E1E"),
    margin = list(t = margin_t, b = margin_b, l = margin_l, r = 24)
  ) |> config(displaylogo = FALSE, responsive = TRUE)
}

# Pixel height for a horizontal bar chart with one row per athlete, so long
# squads scroll instead of squashing labels into each other.
bar_chart_height <- function(n, px_per_row = 22, min_px = 260, pad = 120) {
  paste0(max(min_px, n * px_per_row + pad), "px")
}

# Shared dark reactable theme -- use in every module.
ams_react_theme <- reactableTheme(
  color = AMS_COLORS$ink,
  backgroundColor = "transparent",
  borderColor = "#232323",
  stripedColor = "#1A1A1A",
  highlightColor = "#202020",
  headerStyle = list(
    fontFamily = "Bebas Neue", letterSpacing = "0.05em",
    fontSize = "0.95rem", color = AMS_COLORS$primary,
    borderBottom = paste("2px solid", AMS_COLORS$primary))
)

# Heatmap cell styler: value = ratio vs benchmark (1.0 = at benchmark).
# Dark canvas -> lime fill ramps up; text flips to black once fill is strong.
heat_style <- function(value, max_ratio = 1.4) {
  if (is.na(value)) return(list(background = "#1A1A1A"))
  ratio <- max(0, min(value, max_ratio)) / max_ratio
  alpha <- round(ratio * 0.9, 2)
  list(background = sprintf("rgba(170, 255, 0, %s)", alpha),
       color = if (alpha > 0.45) "#0A0A0A" else AMS_COLORS$ink,
       fontWeight = 600)
}

# Badge with automatic text contrast (lime/gold need black text).
# Availability status -> colour. `print = TRUE` returns darker, ink-friendly
# variants for the PDF export.
status_colour <- function(s, print = FALSE) {
  if (s %in% c("Out", "Sick", "Injured"))
    return(if (print) "#C0392B" else AMS_COLORS$red)
  if (!identical(s, "Full Participation"))
    return(if (print) "#B7950B" else AMS_COLORS$gold)
  if (print) "#1E8449" else AMS_COLORS$primary
}

# ------------------------------------------------------------------------------
# PDF helpers (base graphics -- no pandoc / headless browser dependency)
# ------------------------------------------------------------------------------
# Base pdf() is ASCII territory: transliterate before drawing.
pdf_ascii <- function(x) {
  x <- gsub("≥", ">=", x); x <- gsub("≤", "<=", x)
  x <- gsub("—", "-", x);  x <- gsub("–", "-", x)
  x <- gsub("·", "|", x);  x <- gsub("Δ", "d", x)
  iconv(x, to = "ASCII//TRANSLIT", sub = "")
}

# Draw a simple table on an open pdf() page, paginating as needed.
# `cols` = named list(label, x, align) ; `rows` = list of character vectors.
# `colour_fn(row_i, col_j)` optionally returns a colour per cell.
pdf_table <- function(title, subtitle, cols, rows, colour_fn = NULL,
                      row_h = 0.019) {
  draw_head <- function() {
    par(mar = c(0.4, 0.6, 0.4, 0.6))
    plot.new(); plot.window(xlim = c(0, 1), ylim = c(0, 1))
    y <- 0.98
    text(0, y, pdf_ascii(title), adj = c(0, 1), cex = 1.3, font = 2)
    y <- y - 0.026
    if (nzchar(subtitle)) {
      text(0, y, pdf_ascii(subtitle), adj = c(0, 1), cex = 0.88,
           col = "#444444")
      y <- y - 0.024
    }
    for (cl in cols)
      text(cl$x, y, cl$label, adj = c(if (identical(cl$align, "right")) 1
                                      else 0, 1),
           cex = 0.72, font = 2, col = "#666666")
    y <- y - 0.010
    segments(0, y, 1, y, col = "#333333", lwd = 1.4)
    y - 0.014
  }

  y <- draw_head()
  for (i in seq_along(rows)) {
    if (y < 0.04) y <- draw_head()
    vals <- rows[[i]]
    for (j in seq_along(cols)) {
      cl <- cols[[j]]
      col <- if (!is.null(colour_fn)) colour_fn(i, j) else "#222222"
      text(cl$x, y, pdf_ascii(vals[j]),
           adj = c(if (identical(cl$align, "right")) 1 else 0, 1),
           cex = 0.76, col = col %||% "#222222")
    }
    y <- y - row_h
    segments(0, y + 0.006, 1, y + 0.006, col = "#DDDDDD", lwd = 0.4)
  }
  text(0, 0.02, pdf_ascii(paste("Life University Rugby AMS  |  generated",
                                format(Sys.Date(), "%b %d, %Y"))),
       adj = c(0, 0), cex = 0.62, col = "#888888")
}

status_badge <- function(color, label) {
  rgb <- grDevices::col2rgb(color)
  lum <- (0.299 * rgb[1] + 0.587 * rgb[2] + 0.114 * rgb[3]) / 255
  span(style = paste0(
    "display:inline-block;padding:2px 10px;border-radius:10px;",
    "font-size:0.78rem;font-weight:700;letter-spacing:0.03em;",
    "color:", if (lum > 0.55) "#0A0A0A" else "#FFFFFF",
    ";background:", color), label)
}
