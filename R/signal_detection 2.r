# ──────────────────────────────────────────────────────────────────────────────
# signal_detection.r
# EARS and CUSUM signal detection for the NMC dashboard
#
# Adapted from: NMC_reports_scripts/scripts_and_functions/time_series_functions.r
#
# Provides both lightweight per-series functions and a sliding-window CUSUM
# suitable for the Trending & Signals dashboard page.
#
# Key improvements over legacy:
#   - Uses data.table throughout for speed
#   - Returns tidy output columns suitable for plotly
#   - Parameterised thresholds
#   - No progress-bar side effects (quiet by default)
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

# ── 1. EARS-C2 detector (rolling z-score) ────────────────────────────────────
#
#' Apply EARS-C2 style signal detection on a time series.
#'
#' @param dt         data.table with at least `date` and `n` (daily counts)
#' @param baseline   integer; number of lagged days for the moving baseline (default 14)
#' @param z_thresh   z-score threshold for flagging (default 3)
#' @return  dt with added columns: ears_mean, ears_sd, ears_z, ears_threshold,
#'          is_ears_signal (logical)

ears_detect <- function(dt, baseline = 14L, z_thresh = 3) {
  dt <- as.data.table(copy(dt))
  setorder(dt, date)

  # Rolling mean and sd over the baseline window (right-aligned = lagged)
  dt[, ears_mean := frollmean(shift(n, 1L), n = baseline, align = "right", na.rm = TRUE)]
  dt[, ears_sd   := frollapply(shift(n, 1L), n = baseline, FUN = sd, na.rm = TRUE, align = "right")]

  # Guard against zero sd

  dt[ears_sd < 0.001, ears_sd := 0.001]

  dt[, ears_z         := (n - ears_mean) / ears_sd]
  dt[, ears_threshold := ears_mean + z_thresh * ears_sd]
  dt[, is_ears_signal := !is.na(ears_z) & ears_z > z_thresh]

  return(dt)
}


# ── 2. CUSUM detector ────────────────────────────────────────────────────────
#
#' One-sided upper CUSUM on a daily time series.
#'
#' @param dt              data.table with `date` and `n`
#' @param k               slack parameter (default 0.5)
#' @param cusum_thresh    CUSUM threshold for signalling (default 5)
#' @return  dt with added: cusum_value, is_cusum_signal

cusum_detect <- function(dt, k = 0.5, cusum_thresh = 5) {
  dt <- as.data.table(copy(dt))
  setorder(dt, date)

  y_hat <- dt[, mean(n, na.rm = TRUE)]
  s     <- dt[, sd(n, na.rm = TRUE)]
  if (is.na(s) || s < 0.001) s <- 0.001

  dt[, z_cusum := (n - y_hat) / s]

  # Sequential CUSUM

  n_rows <- nrow(dt)
  cusums <- numeric(n_rows)
  c_prev <- 0
  for (i in seq_len(n_rows)) {
    val <- dt$z_cusum[i]
    if (is.na(val)) val <- 0
    c_prev <- max(0, c_prev + val - k)
    cusums[i] <- c_prev
  }

  dt[, cusum_value     := cusums]
  dt[, is_cusum_signal := cusum_value > cusum_thresh]

  return(dt)
}


# ── 3. Combined EARS + CUSUM detector ────────────────────────────────────────
#
#' Run both EARS and CUSUM on a single time series, return tidy output.
#'
#' @param dt   data.table with `date` and `n`
#' @param ...  passed to ears_detect() and cusum_detect()
#' @return     dt with all signal columns

detect_signals <- function(dt,
                           baseline     = 14L,
                           z_thresh     = 3,
                           k            = 0.5,
                           cusum_thresh = 5) {
  dt <- ears_detect(dt, baseline = baseline, z_thresh = z_thresh)
  dt <- cusum_detect(dt, k = k, cusum_thresh = cusum_thresh)

  # Unified signal flag: either method triggered
  dt[, is_signal := is_ears_signal | is_cusum_signal]

  return(dt)
}


# ── 4. Sliding-window CUSUM (for long series) ────────────────────────────────
#
#' Applies CUSUM over a sliding window so that distant history does not
#' dominate the baseline. This mirrors the cusum_window_start() from the
#' legacy scripts but is faster and returns a clean data.table.
#'
#' @param dt           data.table with `date`, `n`, and optionally `condition`
#' @param window       window size in days (default 180)
#' @param k            CUSUM slack parameter
#' @param cusum_thresh CUSUM threshold
#' @param start_date   optional; only analyse dates >= start_date
#' @param quiet        suppress messages (default TRUE)
#' @return  data.table with one row per date, columns:
#'          date, n, condition (if present), cusum_value, is_cusum_signal

cusum_sliding <- function(dt,
                          window       = 180L,
                          k            = 0.5,
                          cusum_thresh = 5,
                          start_date   = NULL,
                          quiet        = TRUE) {

  dt <- as.data.table(copy(dt))
  setorder(dt, date)

  # Filter if start_date given (keep window-worth of lookback)
  if (!is.null(start_date)) {
    start_date <- as.Date(start_date)
    dt <- dt[as.Date(date) >= (start_date - window)]
  }

  if (nrow(dt) < window) {
    warning("Not enough data for the specified window (", nrow(dt), " < ", window, ")")
    return(data.table())
  }

  total_iter <- nrow(dt) - window
  results <- vector("list", total_iter)

  for (i in seq_len(total_iter)) {
    win <- dt[i:(i + window - 1L)]

    # CUSUM on window
    y_hat <- mean(win$n, na.rm = TRUE)
    s     <- sd(win$n, na.rm = TRUE)
    if (is.na(s) || s < 0.001) s <- 0.001

    z <- (win$n - y_hat) / s
    cusums <- numeric(window)
    c_prev <- 0
    for (j in seq_len(window)) {
      val <- z[j]
      if (is.na(val)) val <- 0
      c_prev <- max(0, c_prev + val - k)
      cusums[j] <- c_prev
    }

    latest <- win[.N]
    results[[i]] <- data.table(
      date             = latest$date,
      n                = latest$n,
      condition        = if ("condition" %in% names(dt)) latest$condition else NA_character_,
      cusum_value      = cusums[window],
      is_cusum_signal  = cusums[window] > cusum_thresh
    )
  }

  rbindlist(results)
}


# ── 5. Batch signal detection across conditions / provinces ──────────────────
#
#' Run detect_signals() for every group in a dataset.
#'
#' @param dt         data.table with date, n, and group columns
#' @param group_cols character vector of grouping columns (e.g. c("condition", "prov_"))
#' @param ...        passed to detect_signals()
#' @return           data.table with all signal columns, grouped

detect_signals_batch <- function(dt, group_cols = "condition", ...) {
  dt <- as.data.table(copy(dt))
  dt[, {
    tryCatch(
      detect_signals(.SD, ...),
      error = function(e) .SD
    )
  }, by = group_cols]
}
