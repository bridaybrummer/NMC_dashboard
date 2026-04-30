# ──────────────────────────────────────────────────────────────────────────────
# burden_of_disease.r
# Years of Life Lost (YLL), DALYs, and age-standardised mortality/incidence
# rates for the NMC dashboard Burden of Disease page.
#
# Requires: data.table, dplyr
# Optional: epitools (for Poisson CIs), flextable, ggplot2
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(ggplot2)
})

source(file.path(here::here(), "R/plot_themes.r"))


# ══════════════════════════════════════════════════════════════════════════════
# A.  SOUTH AFRICAN LIFE TABLE (Stats SA 2024 mid-year population estimates)
# ══════════════════════════════════════════════════════════════════════════════

#' Reference life expectancy table for YLL calculations.
#'
#' Uses a simplified abridged life table based on Stats SA mid-year
#' population estimates (2024 revision). Life expectancy at each age is
#' the average of male and female combined.
#'
#' Alternative: WHO/GBD standard life table (Coale-Demeny west model,
#' level 26) can be swapped in if desired.
#'
#' @return data.table with columns: age_lower, age_upper, life_expectancy
sa_life_table <- function() {
  # Stats SA 2024 MYPE abridged life expectancy (both sexes combined)
  # Source: Statistical release P0302, Table 6
  data.table(
    age_lower       = c(0,  1,  5, 10, 15, 20, 25, 30, 35, 40,
                         45, 50, 55, 60, 65, 70, 75, 80, 85),
    age_upper       = c(1,  5, 10, 15, 20, 25, 30, 35, 40, 45,
                         50, 55, 60, 65, 70, 75, 80, 85, Inf),
    life_expectancy = c(64.4, 64.9, 61.2, 56.4, 51.5, 46.9, 42.5, 38.2, 34.1, 30.1,
                         26.3, 22.7, 19.3, 16.1, 13.2, 10.5, 8.2, 6.2, 4.5)
  )
}

#' GBD 2019 reference life table (aspirational / global benchmark).
#'
#' These values are the maximum attainable life expectancy used in the
#' Global Burden of Disease Study 2019.
#'
#' @return data.table with columns: age_lower, age_upper, life_expectancy
gbd_reference_life_table <- function() {
  data.table(
    age_lower       = c(0,  1,  5, 10, 15, 20, 25, 30, 35, 40,
                         45, 50, 55, 60, 65, 70, 75, 80, 85),
    age_upper       = c(1,  5, 10, 15, 20, 25, 30, 35, 40, 45,
                         50, 55, 60, 65, 70, 75, 80, 85, Inf),
    life_expectancy = c(88.9, 88.0, 84.0, 79.0, 74.1, 69.1, 64.1, 59.1, 54.2, 49.2,
                         44.3, 39.4, 34.6, 29.8, 25.1, 20.5, 16.2, 12.2, 8.9)
  )
}


# ══════════════════════════════════════════════════════════════════════════════
# B.  GBD 2019 DISABILITY WEIGHTS
# ══════════════════════════════════════════════════════════════════════════════

#' Disability weights and average durations for NMC conditions.
#'
#' These are used to compute Years Lived with Disability (YLD).
#' Source: GBD 2019 disability weight supplement, Salomon et al. (2015).
#'
#' @return data.table with: condition_pattern (regex), disability_weight,
#'         duration_years, source
nmc_disability_weights <- function() {
  data.table(
    condition_pattern = c(
      # Vaccine-preventable
      "(?i)measles",
      "(?i)rubella",
      "(?i)pertussis",
      "(?i)diphtheria",
      "(?i)tetanus",
      "(?i)hepatitis\\s*b",
      "(?i)haemophilus",
      "(?i)congenital\\s*rubella",
      "(?i)polio|(?i)flaccid\\s*paralysis",
      "(?i)yellow\\s*fever",
      # Enteric / waterborne
      "(?i)cholera",
      "(?i)enteric\\s*fever|(?i)typhoid",
      "(?i)hepatitis\\s*a",
      "(?i)hepatitis\\s*c",
      "(?i)hepatitis\\s*e",
      "(?i)shigellosis",
      "(?i)salmonellosis",
      "(?i)listeriosis",
      "(?i)food\\s*borne",
      # Respiratory / high priority
      "(?i)covid",
      "(?i)tuberculosis.*pulmonary",
      "(?i)tuberculosis.*extra",
      "(?i)tuberculosis.*mdr|(?i)tuberculosis.*xdr",
      "(?i)meningococcal",
      # Vector-borne
      "(?i)malaria",
      "(?i)rift\\s*valley",
      # Other
      "(?i)rabies",
      "(?i)mpox",
      "(?i)anthrax",
      "(?i)brucellosis",
      "(?i)leprosy",
      "(?i)bilharzia|(?i)schistosomiasis",
      "(?i)congenital\\s*syphilis",
      "(?i)lead\\s*poisoning",
      "(?i)mercury\\s*poisoning",
      "(?i)maternal\\s*death"
    ),
    disability_weight = c(
      0.051,   # Measles – moderate infectious disease
      0.012,   # Rubella – mild
      0.027,   # Pertussis – moderate
      0.187,   # Diphtheria – severe pharyngitis
      0.247,   # Tetanus – severe
      0.051,   # Hepatitis B acute
      0.051,   # Hib meningitis
      0.187,   # CRS – moderate developmental disability
      0.369,   # Polio/AFP – motor impairment
      0.133,   # Yellow fever – moderate
      0.105,   # Cholera – moderate diarrhoeal
      0.051,   # Enteric/typhoid fever
      0.051,   # Hepatitis A
      0.051,   # Hepatitis C acute
      0.051,   # Hepatitis E acute
      0.105,   # Shigellosis – moderate diarrhoeal
      0.061,   # Non-typhoidal salmonellosis
      0.133,   # Listeriosis – severe infectious
      0.061,   # Foodborne illness
      0.051,   # COVID-19 moderate
      0.333,   # TB pulmonary
      0.333,   # TB extra-pulmonary
      0.408,   # MDR/XDR-TB
      0.615,   # Meningococcal – severe meningitis
      0.051,   # Malaria – moderate episode
      0.133,   # Rift Valley Fever
      0.615,   # Rabies – always fatal, prodrome severe
      0.051,   # Mpox
      0.133,   # Anthrax
      0.051,   # Brucellosis
      0.011,   # Leprosy – disfigurement grade 1
      0.027,   # Bilharzia / schistosomiasis
      0.187,   # Congenital syphilis
      0.012,   # Lead poisoning
      0.012,   # Mercury poisoning
      0.000    # Maternal death (mortality only, no disability duration)
    ),
    duration_years = c(
      14/365,  # Measles
      14/365,  # Rubella
      56/365,  # Pertussis
      21/365,  # Diphtheria
      30/365,  # Tetanus
      42/365,  # Hep B acute
      14/365,  # Hib
      1.000,   # CRS – lifelong, annualised
      1.000,   # Polio/AFP – lifelong
      14/365,  # Yellow fever
      7/365,   # Cholera
      21/365,  # Enteric/typhoid
      28/365,  # Hep A
      42/365,  # Hep C acute
      28/365,  # Hep E
      7/365,   # Shigellosis
      7/365,   # NTS
      14/365,  # Listeriosis
      3/365,   # Foodborne
      14/365,  # COVID
      180/365, # TB pulmonary
      180/365, # TB extra-pulmonary
      365/365, # MDR/XDR-TB
      14/365,  # Meningococcal
      7/365,   # Malaria
      14/365,  # RVF
      5/365,   # Rabies (prodrome until death)
      21/365,  # Mpox
      14/365,  # Anthrax
      42/365,  # Brucellosis
      1.000,   # Leprosy – chronic
      1.000,   # Bilharzia – chronic
      1.000,   # Congenital syphilis – first year
      1.000,   # Lead poisoning – chronic
      1.000,   # Mercury poisoning – chronic
      0.000    # Maternal death
    ),
    source = "GBD 2019; Salomon et al. Lancet 2015"
  )
}


#' Match condition names to their disability weight entry.
#'
#' @param conditions character vector of condition names
#' @return data.table: condition, disability_weight, duration_years
match_disability_weights <- function(conditions) {
  dw <- nmc_disability_weights()
  out <- data.table(
    condition         = conditions,
    disability_weight = NA_real_,
    duration_years    = NA_real_
  )
  for (i in seq_len(nrow(dw))) {
    idx <- grepl(dw$condition_pattern[i], conditions, perl = TRUE)
    out[idx & is.na(disability_weight), `:=`(
      disability_weight = dw$disability_weight[i],
      duration_years    = dw$duration_years[i]
    )]
  }
  out
}


# ══════════════════════════════════════════════════════════════════════════════
# C.  YLL CALCULATION
# ══════════════════════════════════════════════════════════════════════════════

#' Compute YLL for individual deaths.
#'
#' For each death at age X, YLL = remaining life expectancy at age X
#' from the reference life table.
#'
#' @param age_at_death numeric vector of ages (years)
#' @param life_table   data.table from sa_life_table() or gbd_reference_life_table()
#' @return numeric vector of YLL values
compute_yll_individual <- function(age_at_death, life_table = sa_life_table()) {
  lt <- as.data.table(life_table)
  vapply(age_at_death, function(a) {
    if (is.na(a) || a < 0) return(NA_real_)
    row <- lt[a >= age_lower & a < age_upper]
    if (nrow(row) == 0) row <- lt[.N]
    row$life_expectancy[1]
  }, FUN.VALUE = numeric(1))
}


#' Aggregate YLL by condition (and optionally other grouping variables).
#'
#' @param deaths_dt   data.table of death records with at minimum:
#'                    condition, age_at_death columns
#' @param by_cols     character vector of additional grouping columns
#'                    e.g. c("prov_", "year")
#' @param life_table  reference life table (default SA)
#' @return data.table: condition (+ by_cols), deaths, total_yll,
#'         mean_yll, median_age_at_death
compute_yll <- function(deaths_dt,
                        by_cols    = NULL,
                        life_table = sa_life_table()) {

  dt <- as.data.table(copy(deaths_dt))
  dt[, yll := compute_yll_individual(age_at_death, life_table)]

 grp <- c("condition", by_cols)

  dt[!is.na(yll), .(
    deaths            = .N,
    total_yll         = round(sum(yll, na.rm = TRUE), 1),
    mean_yll          = round(mean(yll, na.rm = TRUE), 1),
    median_age_death  = round(median(age_at_death, na.rm = TRUE), 1)
  ), by = grp][order(-total_yll)]
}


#' Compute Potential Years of Productive Life Lost (PYPLL).
#'
#' For each death at age X where X < upper_age:
#'   PYPLL(X) = upper_age - X
#' Only deaths in the productive age window [lower_age, upper_age) contribute.
#'
#' @param deaths_dt  data.table of death records with: condition, age_at_death
#' @param by_cols    character vector of additional grouping columns
#' @param lower_age  lower bound of productive years (default 15)
#' @param upper_age  upper bound of productive years (default 65)
#' @return data.table: condition (+ by_cols), deaths_productive, total_pypll,
#'         mean_pypll, median_age_death
compute_pypll <- function(deaths_dt,
                          by_cols   = NULL,
                          lower_age = 15,
                          upper_age = 65) {

  dt <- as.data.table(copy(deaths_dt))
  dt <- dt[age_at_death >= lower_age & age_at_death < upper_age]
  dt[, pypll := upper_age - age_at_death]

  grp <- c("condition", by_cols)

  dt[, .(
    deaths_productive = .N,
    total_pypll       = round(sum(pypll, na.rm = TRUE), 1),
    mean_pypll        = round(mean(pypll, na.rm = TRUE), 1),
    median_age_death  = round(median(age_at_death, na.rm = TRUE), 1)
  ), by = grp][order(-total_pypll)]
}


#' Compute Case Fatality Rate (CFR) by condition (and optional groups).
#'
#' CFR = deaths / cases × 100
#'
#' @param dt         data.table of full case-level data with: condition,
#'                   patient_vital_status, patientoutcome
#' @param by_cols    character vector of additional grouping columns
#'                   e.g. c("prov_", "year", "gender", "age_group")
#' @return data.table: condition (+ by_cols), cases, deaths, cfr_pct, cfr_95ci
compute_cfr <- function(dt, by_cols = NULL) {

  dt <- as.data.table(copy(dt))

  # Identify deaths
  dt[, is_death := (tolower(patient_vital_status) == "deceased") |
       (tolower(patientoutcome) %in% c("died", "died (non-covid)"))]

  grp <- c("condition", by_cols)

  out <- dt[, .(
    cases  = .N,
    deaths = sum(is_death, na.rm = TRUE)
  ), by = grp]

  out[, cfr_pct := round(100 * deaths / cases, 2)]

  # Wilson score 95% CI for proportions (vectorised)
  z <- 1.96
  p_hat <- out$deaths / out$cases
  n <- out$cases
  denom <- 1 + z^2 / n
  centre <- (p_hat + z^2 / (2 * n)) / denom
  offset <- z * sqrt((p_hat * (1 - p_hat) + z^2 / (4 * n)) / n) / denom

  out[, cfr_lower := round(100 * pmax(0, centre - offset), 2)]
  out[, cfr_upper := round(100 * pmin(1, centre + offset), 2)]
  # Handle n=0 edge case
  out[cases == 0, `:=`(cfr_pct = 0, cfr_lower = 0, cfr_upper = 0)]

  out[, cfr_95ci := paste0(cfr_lower, "–", cfr_upper)]

  setorder(out, -cfr_pct)
  out
}


# ══════════════════════════════════════════════════════════════════════════════
# D.  YLD CALCULATION
# ══════════════════════════════════════════════════════════════════════════════

#' Compute YLD for each case based on disability weight and duration.
#'
#' YLD = cases × disability_weight × duration_years
#'
#' @param cases_dt    data.table with at minimum: condition, cases (count)
#' @param by_cols     character vector of additional grouping columns
#' @return data.table: condition (+ by_cols), cases, disability_weight,
#'         duration_years, total_yld
compute_yld <- function(cases_dt, by_cols = NULL) {

  dt <- as.data.table(copy(cases_dt))

  # Match disability weights
  dw <- match_disability_weights(unique(dt$condition))
  dt <- merge(dt, dw, by = "condition", all.x = TRUE)

  # Default for unmatched: generic mild (DW = 0.01, duration 14 days)
  dt[is.na(disability_weight), disability_weight := 0.01]
  dt[is.na(duration_years), duration_years := 14 / 365]

  dt[, yld := cases * disability_weight * duration_years]

  grp <- c("condition", by_cols)
  dt[, .(
    cases             = sum(cases, na.rm = TRUE),
    disability_weight = first(disability_weight),
    duration_years    = first(duration_years),
    total_yld         = round(sum(yld, na.rm = TRUE), 2)
  ), by = grp][order(-total_yld)]
}


# ══════════════════════════════════════════════════════════════════════════════
# E.  DALY CALCULATION
# ══════════════════════════════════════════════════════════════════════════════

#' Combine YLL and YLD into DALYs.
#'
#' DALY = YLL + YLD
#'
#' @param yll_dt  data.table from compute_yll() with at minimum: condition, total_yll
#' @param yld_dt  data.table from compute_yld() with at minimum: condition, total_yld
#' @param by_cols character vector of additional grouping columns
#' @return data.table: condition, deaths, cases, total_yll, total_yld, total_daly,
#'         pct_yll, pct_yld
compute_daly <- function(yll_dt, yld_dt, by_cols = NULL) {

  yll <- as.data.table(copy(yll_dt))
  yld <- as.data.table(copy(yld_dt))

  merge_cols <- c("condition", by_cols)

  out <- merge(
    yll[, c(merge_cols, "deaths", "total_yll", "mean_yll", "median_age_death"), with = FALSE],
    yld[, c(merge_cols, "cases", "total_yld", "disability_weight", "duration_years"), with = FALSE],
    by = merge_cols, all = TRUE
  )

  # Fill NAs with 0 for merging
  setnafill(out, fill = 0, cols = c("total_yll", "total_yld", "deaths", "cases"))

  out[, total_daly := round(total_yll + total_yld, 1)]
  out[, pct_yll := round(100 * total_yll / total_daly, 1)]
  out[, pct_yld := round(100 * total_yld / total_daly, 1)]

  # Fix NaN for conditions with 0 DALY
 out[is.nan(pct_yll), pct_yll := 0]
  out[is.nan(pct_yld), pct_yld := 0]

  setorder(out, -total_daly)
  out
}


# ══════════════════════════════════════════════════════════════════════════════
# F.  DATA PREPARATION HELPERS
# ══════════════════════════════════════════════════════════════════════════════

#' Extract death records from raw NMC data.
#'
#' Identifies deaths using patient_vital_status and patientoutcome columns.
#'
#' @param dt data.table of raw NMC data
#' @return data.table subset containing only death records, with
#'         standardised columns: condition, age_at_death, prov_, year, date
extract_deaths <- function(dt) {
  dt <- as.data.table(copy(dt))

  deaths <- dt[
    (tolower(patient_vital_status) == "deceased") |
    (tolower(patientoutcome) %in% c("died", "died (non-covid)"))
  ]

  # Standardise age column
  if ("Age_years" %in% names(deaths)) {
    deaths[, age_at_death := as.numeric(Age_years)]
  } else if ("age" %in% names(deaths)) {
    deaths[, age_at_death := as.numeric(age)]
  }

  # Standardise year
  if (!"year" %in% names(deaths) && "date" %in% names(deaths)) {
    deaths[, year := year(as.Date(date))]
  }

  deaths[age_at_death >= 0 & !is.na(age_at_death)]
}


#' Aggregate total cases per condition (and optionally other groups).
#'
#' @param dt       data.table of raw NMC data
#' @param by_cols  additional grouping columns (e.g. "prov_", "year")
#' @return data.table: condition (+ by_cols), cases
aggregate_cases <- function(dt, by_cols = NULL) {
  dt <- as.data.table(copy(dt))
  grp <- c("condition", by_cols)
  dt[, .(cases = .N), by = grp]
}


# ══════════════════════════════════════════════════════════════════════════════
# G.  VISUALISATION HELPERS
# ══════════════════════════════════════════════════════════════════════════════

#' Traffic-light colour for DALY contribution
#' @param daly_values numeric vector of DALY values
#' @param breaks numeric vector of break points (default: 0, 100, 1000, Inf)
#' @return character vector of hex colours
daly_colour <- function(daly_values,
                        breaks = c(0, 100, 1000, 5000, Inf),
                        colours = c("#16a34a", "#f59e0b", "#ea580c", "#dc2626")) {
  cut_result <- cut(daly_values, breaks = breaks, labels = colours,
                    include.lowest = TRUE, right = FALSE)
  as.character(cut_result)
}


#' Create a ranked horizontal bar chart of DALYs by condition.
#'
#' @param daly_dt  data.table from compute_daly() with: condition, total_daly,
#'                 total_yll, total_yld
#' @param top_n    integer, show top N conditions (default 20)
#' @param title    plot title
#' @return ggplot object
plot_daly_ranking <- function(daly_dt,
                              top_n = 20,
                              title = "DALY Ranking by Condition") {

  dt <- as.data.table(copy(daly_dt))
  dt <- head(dt[order(-total_daly)], top_n)

  # Pivot longer for stacked bar
  dt_long <- melt(
    dt[, .(condition, YLL = total_yll, YLD = total_yld)],
    id.vars = "condition",
    variable.name = "component",
    value.name = "value"
  )
  dt_long[, condition := factor(condition, levels = rev(dt$condition))]

  ggplot(dt_long, aes(x = condition, y = value, fill = component)) +
    geom_col(width = 0.7) +
    coord_flip() +
    scale_fill_manual(
      values = c(YLL = "#dc2626", YLD = "#3b82f6"),
      labels = c(YLL = "Years of Life Lost", YLD = "Years Lived with Disability")
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       labels = scales::comma) +
    labs(
      title    = title,
      subtitle = "DALY = YLL + YLD | ranked by total burden",
      x = NULL, y = "DALYs", fill = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(colour = "grey40"),
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}


#' Create a YLL summary bar chart.
#'
#' @param yll_dt  data.table from compute_yll()
#' @param top_n   integer
#' @param title   plot title
#' @return ggplot object
plot_yll_ranking <- function(yll_dt,
                             top_n = 20,
                             title = "Top Conditions by Years of Life Lost") {

  dt <- as.data.table(copy(yll_dt))
  dt <- head(dt[order(-total_yll)], top_n)
  dt[, condition := factor(condition, levels = rev(condition))]

  ggplot(dt, aes(x = condition, y = total_yll)) +
    geom_col(fill = "#dc2626", alpha = 0.85, width = 0.7) +
    geom_text(aes(label = deaths), hjust = -0.2, size = 3, colour = "grey30") +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                       labels = scales::comma) +
    labs(
      title    = title,
      subtitle = "Red bars = YLL; numbers = death count",
      x = NULL, y = "Years of Life Lost"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(colour = "grey40"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}


#' Province dot-plot comparing DALYs or YLL per 100,000.
#'
#' @param daly_prov data.table with: prov_, metric (e.g. daly_rate or yll_rate)
#' @param metric_col column name for the rate
#' @param title plot title
#' @return ggplot object
plot_province_comparison <- function(daly_prov,
                                     metric_col = "daly_rate_100k",
                                     title = "DALYs per 100,000 by Province") {

  dt <- as.data.table(copy(daly_prov))
  setnames(dt, metric_col, "metric", skip_absent = TRUE)
  dt[, prov_ := factor(prov_, levels = dt[order(metric)]$prov_)]

  ggplot(dt, aes(x = prov_, y = metric, colour = prov_)) +
    geom_point(size = 4) +
    geom_segment(aes(xend = prov_, y = 0, yend = metric), linewidth = 1) +
    scale_colour_manual(values = province_colours, guide = "none") +
    scale_x_discrete(labels = province_names) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)),
                       labels = scales::comma) +
    labs(
      title    = title,
      subtitle = "Rate per 100,000 population",
      x = NULL, y = "Rate per 100,000"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(colour = "grey40"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank()
    )
}


#' Temporal trend plot for BoD indicators by year.
#'
#' @param trend_dt data.table with: year, condition, metric
#' @param metric_col column name
#' @param title plot title
#' @param top_n show top N conditions
#' @return ggplot object
plot_bod_trend <- function(trend_dt,
                           metric_col = "total_daly",
                           title      = "DALY Trends Over Time",
                           top_n      = 10) {

  dt <- as.data.table(copy(trend_dt))
  setnames(dt, metric_col, "metric", skip_absent = TRUE)

  # Ensure year is numeric
  if ("year" %in% names(dt)) dt[, year := as.numeric(year)]

  # Identify top conditions by overall total
  top_conds <- dt[, .(total = sum(metric, na.rm = TRUE)), by = condition][
    order(-total)][1:min(top_n, .N)]$condition
  dt <- dt[condition %in% top_conds]
  dt[, condition := factor(condition, levels = top_conds)]

  ggplot(dt, aes(x = year, y = metric, colour = condition)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = seq(min(dt$year), max(dt$year))) +
    scale_y_continuous(labels = scales::comma,
                       expand = expansion(mult = c(0, 0.05))) +
    labs(
      title    = title,
      subtitle = paste("Top", min(top_n, length(top_conds)), "conditions by total burden"),
      x = "Year", y = format_col_title(metric_col), colour = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(colour = "grey40"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    guides(colour = guide_legend(nrow = 2))
}
