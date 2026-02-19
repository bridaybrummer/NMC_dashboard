# ──────────────────────────────────────────────────────────────────────────────
# quality_functions.r
# Data quality and duplicate-checking functions for the NMC dashboard
#
# Adapted from:
#   - NMC_reports_scripts/scripts_and_functions/Checking_Duplicates.R
#   - NMC_reports_scripts/scripts_and_functions/Data_completeness.r
#
# These support the "Data Quality" dashboard page.
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})


# ── 1. Duplicate detection ──────────────────────────────────────────────────
#
#' Tag potential duplicate notifications based on case identifiers.
#' Adapted from NMC_reports_scripts/scripts_and_functions/Checking_Duplicates.R
#'
#' @param dt           data.table with case-level data
#' @param id_col       column name for case ID (default "case_id_2")
#' @param date_col     column for sorting duplicates (default "notification_date")
#' @return dt with added columns: is_duplicate (logical), dup_rank (integer)

tag_duplicates <- function(dt,
                           id_col   = "case_id_2",
                           date_col = "notification_date") {
  dt <- as.data.table(copy(dt))

  # Ensure id column exists
  if (!id_col %in% names(dt)) {
    warning("Column '", id_col, "' not found. Skipping duplicate detection.")
    dt[, is_duplicate := FALSE]
    dt[, dup_rank := 1L]
    return(dt)
  }

  setorderv(dt, c(id_col, date_col))

  dt[, dup_count := .N, by = c(id_col)]
  dt[, dup_rank  := seq_len(.N), by = c(id_col)]
  dt[, is_duplicate := dup_count > 1L]

  return(dt)
}


#' Summarise duplicates by condition (for dashboard table).
#' @param dt  data.table already processed by tag_duplicates()
#' @return  data.table: condition, total_notifications, n_unique, n_duplicate, pct_duplicate

summarise_duplicates <- function(dt) {
  dt <- as.data.table(dt)
  if (!"is_duplicate" %in% names(dt)) {
    dt <- tag_duplicates(dt)
  }

  dt[, .(
    total_notifications = .N,
    n_unique     = sum(!is_duplicate),
    n_duplicate  = sum(is_duplicate),
    pct_duplicate = round(sum(is_duplicate) / .N * 100, 1)
  ), by = condition][order(-n_duplicate)]
}


# ── 2. Reporting timeliness ─────────────────────────────────────────────────
#
#' Calculate median notification delay (symptom_date → notification_date).
#'
#' @param dt      data.table with symptom_date and notification_date
#' @param by_col  grouping column (e.g. "prov_", "condition")
#' @return  data.table: by_col, median_delay_days, p25, p75, n_with_dates

calc_timeliness <- function(dt, by_col = "prov_") {
  dt <- as.data.table(copy(dt))

  # Compute delay
  dt[, delay := as.numeric(difftime(
    as.Date(notification_date),
    as.Date(symptom_date),
    units = "days"
  ))]

  # Filter valid delays
  valid <- dt[!is.na(delay) & delay >= 0 & delay < 365]

  valid[, .(
    median_delay_days = median(delay, na.rm = TRUE),
    p25               = quantile(delay, 0.25, na.rm = TRUE),
    p75               = quantile(delay, 0.75, na.rm = TRUE),
    n_with_dates      = .N
  ), by = by_col]
}


# ── 3. Submission volume heatmap data ────────────────────────────────────────
#
#' Prepare data for a submission volume heatmap (day-of-week × epiweek).
#' Useful for the Data Quality page to spot reporting patterns.
#'
#' @param dt        data.table with notification_date
#' @param date_col  column name (default "notification_date")
#' @return  data.table: epiweek, dow (day of week), n

calc_submission_heatmap <- function(dt, date_col = "notification_date") {
  dt <- as.data.table(copy(dt))
  dt[, notif_date := as.Date(get(date_col))]
  dt[, epiweek := floor_date(notif_date, unit = "week")]
  dt[, dow := wday(notif_date, label = TRUE, abbr = TRUE)]

  dt[, .N, by = .(epiweek, dow)]
}


# ── 4. Completeness trend (over time) ───────────────────────────────────────
#
#' Track completeness of a variable over monthly periods.
#'
#' @param dt       data.table with raw case-level data
#' @param var_name variable to assess
#' @param date_col date column for periodisation
#' @return  data.table: month, n_total, n_complete, pct_complete

completeness_trend <- function(dt,
                               var_name = "symptom_date",
                               date_col = "notification_date") {
  dt <- as.data.table(copy(dt))
  if (!var_name %in% names(dt)) {
    warning("Variable '", var_name, "' not found.")
    return(data.table())
  }
  dt[, month := floor_date(as.Date(get(date_col)), "month")]
  dt[, .(
    n_total      = .N,
    n_complete   = sum(!is.na(get(var_name)) &
                        !grepl("unknown", get(var_name), ignore.case = TRUE)),
    pct_complete = round(
      sum(!is.na(get(var_name)) &
           !grepl("unknown", get(var_name), ignore.case = TRUE)) / .N * 100, 1
    )
  ), by = month][order(month)]
}
