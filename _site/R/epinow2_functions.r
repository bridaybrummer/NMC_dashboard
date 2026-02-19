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
#   generation_time  – EpiNow2 distribution
#   incubation_period – EpiNow2 distribution
#   rt_prior          – list(mean, sd) for Rt prior
#
# Values are drawn from WHO / CDC / literature defaults; adjust as needed.

disease_params <- function(condition) {
  condition <- tolower(condition)

  params <- switch(condition,

    "fever-rash" = , "measles" = list(
      generation_time   = EpiNow2::Gamma(shape = Normal(16, 3),
                                          rate  = Normal(2, 1.4), max = 20),
      incubation_period = EpiNow2::Gamma(mean = 12, sd = 3, max = 21),
      rt_prior          = list(mean = 2, sd = 1)
    ),

    "rubella" = list(
      generation_time   = EpiNow2::Gamma(shape = Normal(18, 3),
                                          rate  = Normal(1.5, 1), max = 25),
      incubation_period = EpiNow2::Gamma(mean = 16, sd = 3, max = 23),
      rt_prior          = list(mean = 2, sd = 1)
    ),

    "pertussis" = list(
      generation_time   = EpiNow2::Gamma(shape = Normal(22, 4),
                                          rate  = Normal(2, 1.2), max = 30),
      incubation_period = EpiNow2::Gamma(mean = 9, sd = 3, max = 21),
      rt_prior          = list(mean = 2, sd = 1)
    ),

    "diphtheria" = list(
      generation_time   = EpiNow2::Gamma(shape = Normal(10, 3),
                                          rate  = Normal(1.5, 0.8), max = 21),
      incubation_period = EpiNow2::Gamma(mean = 3, sd = 1.5, max = 10),
      rt_prior          = list(mean = 2, sd = 1)
    ),

    # Default / fallback
    list(
      generation_time   = EpiNow2::Gamma(shape = Normal(10, 3),
                                          rate  = Normal(2, 1.2), max = 20),
      incubation_period = EpiNow2::Gamma(mean = 7, sd = 3, max = 14),
      rt_prior          = list(mean = 2, sd = 1)
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
