# ──────────────────────────────────────────────────────────────────────────────
# epinow2_functions.r
# Reusable EpiNow2 forecasting helpers for vaccine-preventable NMC conditions
#
# Designed to work with the aggregated data produced by prepare_dashboard_data.r
# and the NMCleaner / time_series_functions already in the project.
#
# Conditions of interest (vaccine-preventable / respiratory):
#   • Fever-Rash  (Measles + Rubella combined)
#   • Measles     (standalone)
#   • Rubella     (standalone)
#   • Pertussis
#   • Respiratory viruses (Influenza, RSV, etc.)
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(EpiNow2)
  library(data.table)
  library(dplyr)
  library(lubridate)
})

# ── 1.  Disease parameter lookup table ────────────────────────────────────────
#
# Each entry returns a *named list* with:
#   generation_time   – EpiNow2 distribution (serial interval / generation time)
#   incubation_period – EpiNow2 distribution
#   rt_prior          – list(mean, sd) for Rt prior
#   reference         – citation string for the parameter source
#
# Parameters are specified using the mean/sd Gamma parameterisation where
# possible for readability.  EpiNow2 internally converts to shape/rate.
#
# HOW TO ADJUST:
#   1. Find the condition entry below (lowercase name in switch()).
#   2. Change the mean / sd / max values to match your preferred source.
#   3. Update the `reference` string to cite the new source.
#   4. Re-run:  Rscript R/run_forecasts.r
#   5. Re-render: quarto render by-forecasts/index.qmd
#
# Literature conventions:
#   generation_time  ≈ serial interval when direct measurement unavailable
#   incubation_period = symptom onset from exposure
#   rt_prior          = weakly informative prior; the model learns from data

disease_params <- function(condition) {
  condition <- tolower(condition)

  params <- switch(condition,

    # ── Vaccine-preventable ──────────────────────────────────────────────────

    "fever-rash" = , "measles" = list(
      # Generation time (serial interval): mean 11.7 d (sd 2.0)
      #   Source: Lessler et al. (2009) Lancet Infect Dis 9(5):291-300
      # Incubation period: mean 12.5 d (sd 2.3)
      #   Source: Lessler et al. (2009) – same systematic review
      # R0 ~12-18; Rt prior weakly informative
      generation_time   = EpiNow2::Gamma(mean = 11.7, sd = 2.0, max = 20),
      incubation_period = EpiNow2::Gamma(mean = 12.5, sd = 2.3, max = 21),
      rt_prior          = list(mean = 2, sd = 1),
      reference         = "Lessler et al. (2009) Lancet Infect Dis 9(5):291-300"
    ),

    "rubella" = list(
      # Generation time (serial interval): mean 18.3 d (sd 3.5)
      #   Source: Vink et al. (2014) Am J Epidemiol 180(9):865-875
      # Incubation period: mean 16.5 d (sd 2.5)
      #   Source: Lessler et al. (2009) Lancet Infect Dis 9(5):291-300
      generation_time   = EpiNow2::Gamma(mean = 18.3, sd = 3.5, max = 30),
      incubation_period = EpiNow2::Gamma(mean = 16.5, sd = 2.5, max = 25),
      rt_prior          = list(mean = 2, sd = 1),
      reference         = "Vink et al. (2014) Am J Epidemiol 180(9):865-875; Lessler et al. (2009)"
    ),

    "pertussis" = list(
      # Generation time (serial interval): mean 22.8 d (sd 6.3)
      #   Source: Vink et al. (2014) Am J Epidemiol 180(9):865-875
      # Incubation period: mean 9.4 d (sd 2.7)
      #   Source: Wearing & Rohani (2009) Emerg Infect Dis 15(8):1248-1255
      generation_time   = EpiNow2::Gamma(mean = 22.8, sd = 6.3, max = 42),
      incubation_period = EpiNow2::Gamma(mean = 9.4, sd = 2.7, max = 21),
      rt_prior          = list(mean = 2, sd = 1),
      reference         = "Vink et al. (2014); Wearing & Rohani (2009) Emerg Infect Dis 15(8):1248"
    ),

    "diphtheria" = list(
      # Generation time (serial interval): mean 8 d (sd 3)
      #   Source: Truelove et al. (2020) Emerg Infect Dis 26(10):2396
      # Incubation period: mean 3.3 d (sd 1.5), range 2-5 d
      #   Source: AAP Red Book (2024); WHO position paper
      generation_time   = EpiNow2::Gamma(mean = 8.0, sd = 3.0, max = 18),
      incubation_period = EpiNow2::Gamma(mean = 3.3, sd = 1.5, max = 10),
      rt_prior          = list(mean = 2, sd = 1),
      reference         = "Truelove et al. (2020) Emerg Infect Dis 26(10):2396; AAP Red Book"
    ),

    # ── Category 1 – immediate notification ──────────────────────────────────

    "cholera" = list(
      # Generation time (serial interval): mean 5.0 d (sd 2.0)
      #   Source: Azman et al. (2013) J Infect Dis 207(11):1698-1706
      # Incubation period: mean 1.4 d (sd 0.6), range 0.5-5 d
      #   Source: WHO Cholera Fact Sheet; Azman et al. (2013)
      generation_time   = EpiNow2::Gamma(mean = 5.0, sd = 2.0, max = 14),
      incubation_period = EpiNow2::Gamma(mean = 1.4, sd = 0.6, max = 5),
      rt_prior          = list(mean = 2, sd = 1),
      reference         = "Azman et al. (2013) J Infect Dis 207(11):1698-1706"
    ),

    "malaria" = list(
      # Generation time: mean 30 d (sd 8) – P. falciparum intrinsic + extrinsic
      #   Source: Huber et al. (2016) Malar J 15:18 (modelling serial interval)
      # Incubation period: mean 12 d (sd 3), range 7-30 d for P. falciparum
      #   Source: WHO World Malaria Report (2023); Bartoloni & Zammarchi (2012) Mediterr J Hematol Infect Dis
      # Rt prior lower (vector-borne, Rt typically 1-3 in endemic settings)
      generation_time   = EpiNow2::Gamma(mean = 30.0, sd = 8.0, max = 50),
      incubation_period = EpiNow2::Gamma(mean = 12.0, sd = 3.0, max = 30),
      rt_prior          = list(mean = 1.5, sd = 0.5),
      reference         = "Huber et al. (2016) Malar J 15:18; WHO World Malaria Report (2023)"
    ),

    "covid-19" = list(
      # Generation time (serial interval): mean 3.3 d (sd 1.3) – Omicron era
      #   Source: Park et al. (2023) BMC Infect Dis 23:295 (Omicron meta-analysis)
      # Incubation period: mean 3.4 d (sd 1.4) – Omicron
      #   Source: Wu et al. (2022) JAMA Netw Open 5(8):e2228008
      generation_time   = EpiNow2::Gamma(mean = 3.3, sd = 1.3, max = 10),
      incubation_period = EpiNow2::Gamma(mean = 3.4, sd = 1.4, max = 10),
      rt_prior          = list(mean = 1.5, sd = 0.5),
      reference         = "Park et al. (2023) BMC Infect Dis 23:295; Wu et al. (2022) JAMA Netw Open 5(8):e2228008"
    ),

    "enteric fever (typhoid or paratyphoid fever)" = , "enteric fever" = list(
      # Generation time (serial interval): mean 14 d (sd 4) for typhoid
      #   Source: Pitzer et al. (2014) PLoS Negl Trop Dis 8(6):e2891
      # Incubation period: mean 10.2 d (sd 3.5), range 5-21 d
      #   Source: Crump et al. (2015) Clin Microbiol Rev 28(4):901-937
      generation_time   = EpiNow2::Gamma(mean = 14.0, sd = 4.0, max = 30),
      incubation_period = EpiNow2::Gamma(mean = 10.2, sd = 3.5, max = 21),
      rt_prior          = list(mean = 1.5, sd = 0.5),
      reference         = "Pitzer et al. (2014) PLoS Negl Trop Dis 8(6):e2891; Crump et al. (2015) Clin Microbiol Rev 28(4):901"
    ),

    "meningococcal disease" = list(
      # Generation time (serial interval): mean 7.0 d (sd 2.5)
      #   Source: Trotter et al. (2005) Emerg Infect Dis 11(4):563-570
      # Incubation period: mean 3.7 d (sd 1.3), range 2-10 d
      #   Source: Rosenstein et al. (2001) N Engl J Med 344(18):1378-1388
      generation_time   = EpiNow2::Gamma(mean = 7.0, sd = 2.5, max = 14),
      incubation_period = EpiNow2::Gamma(mean = 3.7, sd = 1.3, max = 10),
      rt_prior          = list(mean = 1.5, sd = 0.5),
      reference         = "Trotter et al. (2005) Emerg Infect Dis 11(4):563; Rosenstein et al. (2001) NEJM 344(18):1378"
    ),

    "listeriosis" = list(
      # Generation time: mean 21 d (sd 10) – foodborne, long tail
      #   Source: Goulet et al. (2013) Clin Infect Dis 56(10):1411-1419
      # Incubation period: median 8 d for GI, up to 67 d for invasive; mean 21 d (sd 12)
      #   Source: Goulet et al. (2013); European Centre for Disease Prevention (ECDC) rapid risk assessment
      generation_time   = EpiNow2::Gamma(mean = 21.0, sd = 10.0, max = 70),
      incubation_period = EpiNow2::Gamma(mean = 21.0, sd = 12.0, max = 70),
      rt_prior          = list(mean = 1.2, sd = 0.5),
      reference         = "Goulet et al. (2013) Clin Infect Dis 56(10):1411-1419"
    ),

    "hepatitis a" = list(
      # Generation time (serial interval): mean 24 d (sd 6)
      #   Source: Jacobsen & Wiersma (2010) Clin Liver Dis 14(4):591-602
      # Incubation period: mean 28.5 d (sd 5), range 15-50 d
      #   Source: Lemon et al. (2018) Lancet 391(10120):1587-1598
      generation_time   = EpiNow2::Gamma(mean = 24.0, sd = 6.0, max = 50),
      incubation_period = EpiNow2::Gamma(mean = 28.5, sd = 5.0, max = 50),
      rt_prior          = list(mean = 1.5, sd = 0.5),
      reference         = "Jacobsen & Wiersma (2010) Clin Liver Dis 14(4):591; Lemon et al. (2018) Lancet 391:1587"
    ),

    "mpox" = list(
      # Generation time (serial interval): mean 12.5 d (sd 4.3)
      #   Source: Miura et al. (2022) Euro Surveill 27(44):2200806
      # Incubation period: mean 8.5 d (sd 2.6), range 5-21 d
      #   Source: Thornhill et al. (2022) N Engl J Med 387(8):679-691
      generation_time   = EpiNow2::Gamma(mean = 12.5, sd = 4.3, max = 25),
      incubation_period = EpiNow2::Gamma(mean = 8.5, sd = 2.6, max = 21),
      rt_prior          = list(mean = 1.5, sd = 0.5),
      reference         = "Miura et al. (2022) Euro Surveill 27(44):2200806; Thornhill et al. (2022) NEJM 387(8):679"
    ),

    # ── Additional vaccine-preventable diseases ───────────────────────────────

    "tetanus" = list(
      # Tetanus is non-communicable (environmental Clostridium tetani),
      # so generation time is not epidemiologically meaningful.
      # We use a nominal serial interval for modelling purposes only.
      # Incubation period: mean 8 d (sd 3), range 3-21 d
      #   Source: WHO position paper (2017); Roper et al. (2018) Lancet 391:1657
      generation_time   = EpiNow2::Gamma(mean = 14.0, sd = 5.0, max = 30),
      incubation_period = EpiNow2::Gamma(mean = 8.0, sd = 3.0, max = 21),
      rt_prior          = list(mean = 1.0, sd = 0.5),
      reference         = "WHO Tetanus Position Paper (2017); Roper et al. (2018) Lancet 391:1657"
    ),

    "hepatitis b" = list(
      # Generation time: mean 60 d (sd 20) – long infectious period
      #   Source: Edmunds et al. (1993) Epidemiol Infect 110(3):569-578
      # Incubation period: mean 75 d (sd 20), range 45-180 d
      #   Source: Liang (2009) Hepatology 49(5 Suppl):S13-21
      generation_time   = EpiNow2::Gamma(mean = 60.0, sd = 20.0, max = 120),
      incubation_period = EpiNow2::Gamma(mean = 75.0, sd = 20.0, max = 180),
      rt_prior          = list(mean = 1.5, sd = 0.5),
      reference         = "Edmunds et al. (1993) Epidemiol Infect 110(3):569; Liang (2009) Hepatology 49(S5):S13"
    ),

    "haemophilus influenzae type b" = list(
      # Generation time: mean 3 d (sd 1.5) – short respiratory transmission
      #   Source: Peltola (2000) Clin Microbiol Rev 13(2):302-317
      # Incubation period: mean 2.5 d (sd 1.0), range 2-4 d
      #   Source: AAP Red Book (2024); Peltola (2000)
      generation_time   = EpiNow2::Gamma(mean = 3.0, sd = 1.5, max = 10),
      incubation_period = EpiNow2::Gamma(mean = 2.5, sd = 1.0, max = 7),
      rt_prior          = list(mean = 1.5, sd = 0.5),
      reference         = "Peltola (2000) Clin Microbiol Rev 13(2):302-317; AAP Red Book (2024)"
    ),

    "congenital rubella syndrome" = list(
      # Uses rubella parameters (vertical transmission via maternal infection)
      # Generation time: mean 18.3 d (sd 3.5) – same as rubella
      # Incubation: mean 16.5 d (sd 2.5)
      #   Source: Vink et al. (2014); Lessler et al. (2009)
      generation_time   = EpiNow2::Gamma(mean = 18.3, sd = 3.5, max = 30),
      incubation_period = EpiNow2::Gamma(mean = 16.5, sd = 2.5, max = 25),
      rt_prior          = list(mean = 1.5, sd = 0.5),
      reference         = "Vink et al. (2014); Lessler et al. (2009) – rubella parameters"
    ),

    "poliomyelitis" = , "acute flaccid paralysis" = list(
      # Generation time (serial interval): mean 15 d (sd 5)
      #   Source: Fine & Carneiro (1999) Am J Epidemiol 150(12):1332-1346
      # Incubation period: mean 10 d (sd 4), range 3-35 d
      #   Source: Nathanson & Kew (2010) Am J Epidemiol 172(11):1213-1229
      generation_time   = EpiNow2::Gamma(mean = 15.0, sd = 5.0, max = 35),
      incubation_period = EpiNow2::Gamma(mean = 10.0, sd = 4.0, max = 35),
      rt_prior          = list(mean = 2, sd = 1),
      reference         = "Fine & Carneiro (1999) Am J Epidemiol 150(12):1332; Nathanson & Kew (2010) Am J Epidemiol 172(11):1213"
    ),

    "yellow fever" = list(
      # Generation time: mean 12 d (sd 3) – human-mosquito-human cycle
      #   Source: Johansson et al. (2010) Am J Trop Med Hyg 82(5):930-936
      # Incubation period: mean 4.5 d (sd 1.5), range 3-6 d
      #   Source: Monath & Vasconcelos (2015) J Clin Virol 64:160-173
      generation_time   = EpiNow2::Gamma(mean = 12.0, sd = 3.0, max = 25),
      incubation_period = EpiNow2::Gamma(mean = 4.5, sd = 1.5, max = 10),
      rt_prior          = list(mean = 1.5, sd = 0.5),
      reference         = "Johansson et al. (2010) Am J Trop Med Hyg 82(5):930; Monath & Vasconcelos (2015) J Clin Virol 64:160"
    ),

    # Default / fallback – conservative generic respiratory pathogen
    list(
      generation_time   = EpiNow2::Gamma(mean = 7.0, sd = 3.0, max = 20),
      incubation_period = EpiNow2::Gamma(mean = 5.0, sd = 2.5, max = 14),
      rt_prior          = list(mean = 2, sd = 1),
      reference         = "Default: generic respiratory pathogen (no condition-specific source)"
    )
  )

  return(params)
}


# ── 2. Estimate reporting delay from surveillance data ────────────────────────
#
# Mirrors the approach in Disease_epinow.qmd: compute symptom→notification
# delay from the raw case-level data, then fit with EpiNow2::estimate_delay().
#
# @param dt          data.table / data.frame with `notification_date` and `symptom_date`
# @param max_sample  max observations to use (for speed)
# @param max_value   truncation for the fitted delay distribution
# @return            an EpiNow2 delay distribution object

estimate_reporting_delay <- function(dt,
                                     max_sample = 100,
                                     max_value  = 15,
                                     bootstraps = 1) {

  delays <- as.numeric(difftime(
    as.Date(dt$notification_date),
    as.Date(dt$symptom_date),
    units = "days"
  ))

  # Drop negatives and NAs

  delays <- delays[!is.na(delays) & delays >= 0]

  if (length(delays) == 0) {
    message("No valid delays; returning fixed LogNormal(1.5, 0.5).")
    return(EpiNow2::LogNormal(mean = 1.5, sd = 0.5, max = max_value))
  }

  if (length(delays) > max_sample) {
    delays <- sample(delays, max_sample, replace = FALSE)
  }

  EpiNow2::estimate_delay(delays, max_value = max_value, bootstraps = bootstraps)
}


# ── 3. Run EpiNow2 forecast for one condition ────────────────────────────────
#
# @param case_ts       data.frame with columns `date` (Date) and `confirm` (int)
# @param condition     character: one of the supported disease names
# @param reporting_delay  optional pre-computed delay; if NULL a LogNormal default is used
# @param horizon       forecast horizon in days (default 21)
# @param lookback_days how many days of history to feed the model (default 360)
# @param CrIs          credible-interval widths (default 0.5, 0.9)
# @return              the full EpiNow2::epinow() result object

run_epinow_forecast <- function(case_ts,
                                condition,
                                reporting_delay = NULL,
                                horizon       = 21,
                                lookback_days = 360,
                                CrIs          = c(0.5, 0.9)) {

  # Disease-specific parameters

  params <- disease_params(condition)

  # Trim to the lookback window
  end_date   <- max(case_ts$date)
  start_date <- end_date - days(lookback_days)
  case_ts    <- case_ts[case_ts$date >= start_date & case_ts$date <= end_date, ]

  # Fall back to a sensible fixed delay if none supplied

  if (is.null(reporting_delay)) {
    reporting_delay <- EpiNow2::LogNormal(mean = 3, sd = 2, max = 15)
  }

  estimates <- EpiNow2::epinow(
    data            = case_ts,
    generation_time = EpiNow2::generation_time_opts(params$generation_time),
    delays          = EpiNow2::delay_opts(params$incubation_period + reporting_delay),
    obs             = EpiNow2::obs_opts(week_effect = TRUE),
    forecast        = EpiNow2::forecast_opts(horizon = horizon),
    CrIs            = CrIs
  )

  return(estimates)
}


# ── 4. Extract tidy summary from EpiNow2 result ──────────────────────────────

extract_forecast_summary <- function(epinow_result, condition_label = "Unknown") {
  # EpiNow2 >= 1.8.0: use summary() instead of deprecated $estimates$summarised
  summ <- tryCatch(
    summary(epinow_result, type = "parameters"),
    error = function(e) epinow_result$estimates$summarised
  )
  summ$condition <- condition_label
  summ
}


# ── 5. Rolling forecast ("rewind") for outbreak animation ─────────────────────
#
# Runs EpiNow2 repeatedly, each time with data up to (today − i) days,
# producing a family of forecast curves that can be animated to show how
# the outlook evolved over time.
#
# @param case_ts         data.frame: date + confirm
# @param condition       character
# @param reporting_delay EpiNow2 delay (or NULL for default)
# @param n_rewinds       how many past snapshots to compute (default 8)
# @param rewind_step     days between each snapshot (default 7 ≈ weekly)
# @param lookback_days   history fed to each run
# @param horizon         forecast days
# @return data.frame with all forecast summaries, column `snapshot_date`

rolling_forecast <- function(case_ts,
                             condition,
                             reporting_delay = NULL,
                             n_rewinds     = 8,
                             rewind_step   = 7,
                             lookback_days = 180,
                             horizon       = 21) {

  all_results <- list()


  for (i in seq(0, (n_rewinds - 1) * rewind_step, by = rewind_step)) {
    snap_date <- max(case_ts$date) - days(i)
    ts_sub    <- case_ts[case_ts$date <= snap_date, ]

    if (nrow(ts_sub) < 30) next

    tryCatch({
      est <- run_epinow_forecast(
        case_ts        = ts_sub,
        condition      = condition,
        reporting_delay = reporting_delay,
        horizon        = horizon,
        lookback_days  = lookback_days,
        CrIs           = c(0.5, 0.9)
      )

      summ <- extract_forecast_summary(est, condition)
      summ$snapshot_date <- snap_date
      summ$snapshot_label <- format(snap_date, "%d %b %Y")
      all_results[[length(all_results) + 1]] <- summ
    }, error = function(e) {
      message("Snapshot ", snap_date, " failed: ", e$message)
    })
  }

  bind_rows(all_results)
}


# ── 6. Prepare time-series for a condition from aggregated data ───────────────
#
# Works with the agg_national / agg_province objects from prepare_dashboard_data.r

prepare_condition_ts <- function(agg_dt,
                                 target_condition,
                                 region_filter = NULL) {

  dt <- data.table::copy(agg_dt)

  if (!is.null(region_filter)) {
    dt <- dt[region == region_filter]
  }

  dt <- dt[condition == target_condition,
           .(confirm = sum(confirm, na.rm = TRUE)),
           keyby = .(date)]

  # Ensure continuous daily series
  all_dates <- data.table(date = seq(min(dt$date), max(dt$date), by = "day"))
  dt <- dt[all_dates, on = "date"][is.na(confirm), confirm := 0]

  as.data.frame(dt)
}


# ── 7. Combine Fever-Rash conditions (mirror make_fever_rash.R) ──────────────

combine_fever_rash <- function(agg_dt) {
  dt <- data.table::copy(agg_dt)
  dt[condition %in% c("Measles", "Rubella"), condition := "Fever-Rash"]

  # Re-aggregate after renaming so there are no duplicate (condition, date) rows
  num_cols <- intersect(names(dt), c("count", "confirm"))
  key_cols <- setdiff(names(dt), c(num_cols, "incidence_100k"))

  if (length(num_cols) > 0 && "date" %in% names(dt)) {
    dt <- dt[, lapply(.SD, sum, na.rm = TRUE), by = key_cols, .SDcols = num_cols]
  }

  dt
}


# ── 8. Vaccine-preventable condition list ─────────────────────────────────────

vp_conditions <- function() {
  c("Fever-Rash", "Measles", "Rubella", "Pertussis", "Diphtheria")
}

# ── 8b. Broader set of conditions for EpiNow2 forecasting ────────────────────
#  Includes key Category 1 (and selected Category 2) conditions with
#  sufficient case volume for meaningful Rt estimation.

forecast_conditions <- function() {
  c(
    # Vaccine-preventable
    "Fever-Rash", "Measles", "Rubella", "Pertussis", "Diphtheria",
    "Tetanus", "Hepatitis B", "Haemophilus influenzae type B",
    "Congenital rubella syndrome",
    "Poliomyelitis", "Acute flaccid paralysis", "Yellow fever",
    # Category 1 – high public-health priority (non-VPD)
    "Cholera", "Malaria", "Covid-19",
    "Enteric fever (typhoid or paratyphoid fever)",
    "Meningococcal disease", "Listeriosis", "Mpox",
    # Category 2 – high volume
    "Hepatitis A"
  )
}

# ── 8c. Filter data to all forecasting-eligible conditions ────────────────────

filter_forecast_conditions <- function(agg_dt, include_fever_rash = TRUE) {
  dt <- data.table::copy(agg_dt)
  if (include_fever_rash) {
    dt <- combine_fever_rash(dt)
  }
  targets <- forecast_conditions()
  dt[condition %in% targets]
}

# ── 9. Human-readable parameter summary table ─────────────────────────────────
#
# Returns a data.frame with one row per condition, showing the key assumptions
# passed to EpiNow2. Used by the dashboard to display a transparency table.

disease_params_table <- function(conditions = forecast_conditions()) {
  rows <- lapply(conditions, function(cond) {
    p <- disease_params(cond)

    # --- Generation time ---------------------------------------------------
    gt <- p$generation_time
    gt_txt <- tryCatch({
      pars <- gt$parameters
      if (!is.null(pars$mean)) {
        sprintf("Gamma(mean=%.1f, sd=%.1f, max=%s)",
                pars$mean,
                if (!is.null(pars$sd)) pars$sd else NA,
                if (!is.null(gt$max)) gt$max else "--")
      } else if (!is.null(pars$shape) && !is.null(pars$rate)) {
        shape_m <- if (is.list(pars$shape)) pars$shape$parameters$mean else pars$shape
        rate_m  <- if (is.list(pars$rate))  pars$rate$parameters$mean  else pars$rate
        sprintf("Gamma(shape~%.0f, rate~%.1f, max=%s)", shape_m, rate_m,
                if (!is.null(gt$max)) gt$max else "--")
      } else {
        "Gamma (see source)"
      }
    }, error = function(e) "Gamma (see source)")

    # --- Incubation period -------------------------------------------------
    ip <- p$incubation_period
    ip_txt <- tryCatch({
      pars <- ip$parameters
      if (!is.null(pars$mean)) {
        sprintf("Gamma(mean=%.1f, sd=%.1f, max=%s)",
                pars$mean,
                if (!is.null(pars$sd)) pars$sd else NA,
                if (!is.null(ip$max)) ip$max else "--")
      } else {
        "Gamma (see source)"
      }
    }, error = function(e) "Gamma (see source)")

    # --- Rt prior ----------------------------------------------------------
    rt <- p$rt_prior
    rt_txt <- sprintf("Normal(mean=%.1f, sd=%.1f)", rt$mean, rt$sd)

    # --- Reporting delay (default used in run_epinow_forecast) -------------
    delay_txt <- "LogNormal(mean=3, sd=2, max=15)"

    # --- Reference ---------------------------------------------------------
    ref_txt <- if (!is.null(p$reference)) p$reference else ""

    # --- Plain-language explanation ----------------------------------------
    # Extract generation time mean for the explanation
    gt_mean <- tryCatch({
      pm <- p$generation_time$parameters
      if (!is.null(pm$mean)) round(pm$mean) else NA
    }, error = function(e) NA)
    ip_mean <- tryCatch({
      pm <- p$incubation_period$parameters
      if (!is.null(pm$mean)) round(pm$mean) else NA
    }, error = function(e) NA)
    rt_mean <- p$rt_prior$mean

    plain <- sprintf(
      paste0(
        "On average, an infected person can spread %s to others about %s days after being infected themselves. ",
        "Symptoms typically appear ~%s days after exposure. ",
        "The model starts by assuming each case infects about %.0f other people on average, ",
        "then adjusts this up or down based on the actual reported data. ",
        "A ~3-day delay is assumed between someone falling ill and the case being officially reported."
      ),
      cond,
      if (!is.na(gt_mean)) gt_mean else "several",
      if (!is.na(ip_mean)) ip_mean else "several",
      rt_mean
    )

    data.frame(
      Condition           = cond,
      Generation_Time     = gt_txt,
      Incubation_Period   = ip_txt,
      Rt_Prior            = rt_txt,
      Reporting_Delay     = delay_txt,
      In_Plain_Language   = plain,
      Reference           = ref_txt,
      stringsAsFactors    = FALSE
    )
  })

  do.call(rbind, rows)
}


#' Filter aggregated data to vaccine-preventable / respiratory conditions
#' Also combines Measles + Rubella → Fever-Rash if requested
filter_vp_conditions <- function(agg_dt, include_fever_rash = TRUE) {
  dt <- data.table::copy(agg_dt)
  if (include_fever_rash) {
    dt <- combine_fever_rash(dt)
  }
  targets <- vp_conditions()
  dt[condition %in% targets]
}
