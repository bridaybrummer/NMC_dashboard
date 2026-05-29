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

# ── 0. Internal: SD-floor strategy ────────────────────────────────────────────
#
# Stabilises the standard deviation used as the denominator of a z-score
# when the empirical SD is degenerate (zero, NA, or implausibly small for the
# observed mean). See the `sd_floor` argument documentation on `ears_detect()`
# for the methodology rationale.
.apply_sd_floor <- function(s, mu, sd_floor = "poisson") {
  if (is.character(sd_floor)) {
    if (identical(sd_floor, "fixed")) {
      if (is.na(s) || s < 0.001) return(0.001)
      return(s)
    }
    # poisson: at least sqrt(max(mu, 1))
    floor_val <- sqrt(max(mu, 1, na.rm = TRUE))
    if (is.na(s)) return(floor_val)
    return(max(s, floor_val, 0.001))
  }
  # numeric floor
  fv <- as.numeric(sd_floor)
  if (is.na(s) || s < fv) return(fv)
  return(s)
}

# ── 1. EARS-C2 detector (rolling z-score) ────────────────────────────────────
#
#' Apply EARS-C2 style signal detection on a time series.
#'
#' @param dt          data.table with at least `date` and `n` (daily counts)
#' @param baseline    integer; number of lagged days for the moving baseline (default 14)
#' @param z_thresh    z-score threshold for flagging (default 3)
#' @param sd_floor    Strategy for stabilising the rolling SD when the
#'                    baseline is degenerate (all-zero or near-constant):
#'                    * `"poisson"` (default) — `pmax(sd, sqrt(pmax(mean, 1)))`,
#'                      i.e. assume at least Poisson noise. Prevents tiny SDs
#'                      from inflating z-scores for rare conditions.
#'                    * `"fixed"` — clamp at `1e-3` (legacy behaviour;
#'                      reproduces the original NICD scripts).
#'                    * a positive numeric — clamp SD at that absolute value.
#' @param min_baseline_mean  Suppress EARS signals when the baseline mean is
#'                    below this value (default 0.5). Rationale: for rare
#'                    diseases whose 14-day baseline averages <1 case/day, a
#'                    single new case produces a z-score of `+Inf`/very large
#'                    that is not a meaningful aberration. Such rows still get
#'                    `ears_z` computed, but `is_ears_signal = FALSE` and
#'                    `is_low_baseline = TRUE`.
#' @return  dt with added columns: ears_mean, ears_sd, ears_sd_raw, ears_z,
#'          ears_threshold, is_low_baseline, is_ears_signal (logical)
#'
#' @section Methodology note:
#'   The standard EARS-C2 (Hutwagner 2003) uses a sample SD over the lagged
#'   baseline. For rare/sparse counts the sample SD can collapse to 0 (all
#'   zeros) or to a tiny non-zero value (one outlier), producing implausibly
#'   large z-scores. We replace the SD with the larger of the empirical SD and
#'   the Poisson-implied SD `sqrt(max(mean, 1))`; this is equivalent to
#'   assuming at least Poisson process noise and is consistent with the
#'   Farrington-style treatment used by ECDC and NICD. Combined with the
#'   `min_baseline_mean` gate, this prevents single-case alerts on rare
#'   diseases without dampening genuine outbreaks.

ears_detect <- function(dt,
                        baseline          = 14L,
                        z_thresh          = 3,
                        sd_floor          = c("poisson", "fixed"),
                        min_baseline_mean = 0.5) {
  if (is.character(sd_floor)) sd_floor <- match.arg(sd_floor)
  dt <- as.data.table(copy(dt))
  setorder(dt, date)

  # Rolling mean and sd over the baseline window (right-aligned = lagged)
  dt[, ears_mean   := frollmean(shift(n, 1L), n = baseline, align = "right", na.rm = TRUE)]
  dt[, ears_sd_raw := frollapply(shift(n, 1L), n = baseline, FUN = sd, na.rm = TRUE, align = "right")]

  # Apply selected SD floor
  if (identical(sd_floor, "fixed")) {
    dt[, ears_sd := pmax(ears_sd_raw, 0.001, na.rm = TRUE)]
  } else if (is.numeric(sd_floor)) {
    floor_val <- as.numeric(sd_floor)
    dt[, ears_sd := pmax(ears_sd_raw, floor_val, na.rm = TRUE)]
  } else {
    # "poisson": at least sqrt(max(mean, 1)) so a baseline mean of 0 still
    # implies SD = 1 (one expected case worth of noise).
    dt[, ears_sd := pmax(ears_sd_raw, sqrt(pmax(ears_mean, 1, na.rm = TRUE)), 0.001,
                         na.rm = TRUE)]
  }

  dt[, ears_z          := (n - ears_mean) / ears_sd]
  dt[, ears_threshold  := ears_mean + z_thresh * ears_sd]
  dt[, is_low_baseline := !is.na(ears_mean) & ears_mean < min_baseline_mean]
  dt[, is_ears_signal  := !is.na(ears_z) & ears_z > z_thresh & !is_low_baseline]

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

cusum_detect <- function(dt, k = 0.5, cusum_thresh = 5,
                         sd_floor = c("poisson", "fixed")) {
  if (is.character(sd_floor)) sd_floor <- match.arg(sd_floor)
  dt <- as.data.table(copy(dt))
  setorder(dt, date)

  y_hat <- dt[, mean(n, na.rm = TRUE)]
  s     <- dt[, sd(n, na.rm = TRUE)]
  s <- .apply_sd_floor(s, y_hat, sd_floor)

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
                           baseline          = 14L,
                           z_thresh          = 3,
                           k                 = 0.5,
                           cusum_thresh      = 5,
                           sd_floor          = c("poisson", "fixed"),
                           min_baseline_mean = 0.5) {
  if (is.character(sd_floor)) sd_floor <- match.arg(sd_floor)
  dt <- ears_detect(dt, baseline = baseline, z_thresh = z_thresh,
                    sd_floor = sd_floor,
                    min_baseline_mean = min_baseline_mean)
  dt <- cusum_detect(dt, k = k, cusum_thresh = cusum_thresh,
                     sd_floor = sd_floor)

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
                          sd_floor     = c("poisson", "fixed"),
                          quiet        = TRUE) {
  if (is.character(sd_floor)) sd_floor <- match.arg(sd_floor)

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
    s     <- .apply_sd_floor(s, y_hat, sd_floor)

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


# ── 6. Build a tidy alert-log table from a batch detection run ───────────────
#
#' From the per-row signal output of detect_signals_batch(), produce one row
#' per (group × method × first signal date) so analysts get a triage list.
#'
#' @param dt          output of detect_signals_batch() with a `date` column
#' @param group_cols  same group_cols passed to detect_signals_batch()
#' @param recent_days only flag signals within the last N days (default 28)
#' @return  data.table: <group_cols>, method, first_signal, last_signal,
#'          n_signal_days, peak_value, latest_n

build_alert_log <- function(dt, group_cols = "condition", recent_days = 28L) {
  dt <- as.data.table(copy(dt))
  if (!"date" %in% names(dt)) stop("`dt` must contain a `date` column.")
  dt[, date := as.Date(date)]
  cutoff <- max(dt$date, na.rm = TRUE) - recent_days

  out <- list()

  if ("is_ears_signal" %in% names(dt)) {
    ears <- dt[is_ears_signal == TRUE & date >= cutoff,
               .(method        = "EARS-C2",
                 first_signal  = min(date),
                 last_signal   = max(date),
                 n_signal_days = .N,
                 peak_value    = max(ears_z, na.rm = TRUE),
                 latest_n      = n[which.max(date)]),
               by = group_cols]
    out[[length(out) + 1L]] <- ears
  }
  if ("is_cusum_signal" %in% names(dt)) {
    cs <- dt[is_cusum_signal == TRUE & date >= cutoff,
             .(method        = "CUSUM",
               first_signal  = min(date),
               last_signal   = max(date),
               n_signal_days = .N,
               peak_value    = max(cusum_value, na.rm = TRUE),
               latest_n      = n[which.max(date)]),
             by = group_cols]
    out[[length(out) + 1L]] <- cs
  }

  res <- rbindlist(out, use.names = TRUE, fill = TRUE)
  if (nrow(res) == 0L) return(res)
  setorder(res, -last_signal, -peak_value)
  res
}


# ── 7. Persist a rolling alert log to disk ───────────────────────────────────
#
#' Append today's alerts to data/processed/alerts.rds. De-duplicates on
#' (group_cols × method × first_signal). Safe to call repeatedly.
#'
#' @param alert_dt   output of build_alert_log()
#' @param group_cols grouping columns used in detection
#' @param path       RDS file path
#' @return data.table written to disk

persist_alerts <- function(alert_dt,
                           group_cols = "condition",
                           path = "data/processed/alerts.rds") {

  if (is.null(alert_dt) || nrow(alert_dt) == 0L) return(invisible(NULL))
  alert_dt <- as.data.table(copy(alert_dt))
  alert_dt[, run_date := Sys.Date()]

  prev <- if (file.exists(path)) {
    tryCatch(as.data.table(readRDS(path)), error = function(e) data.table())
  } else data.table()

  combined <- rbindlist(list(prev, alert_dt), use.names = TRUE, fill = TRUE)

  key_cols <- c(group_cols, "method", "first_signal")
  key_cols <- key_cols[key_cols %in% names(combined)]
  if (length(key_cols) > 0L) {
    setorderv(combined, c(key_cols, "run_date"))
    combined <- unique(combined, by = key_cols, fromLast = TRUE)
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(combined, path)
  invisible(combined)
}
