# ──────────────────────────────────────────────────────────────────────────────
# run_forecasts.r
# Batch runner: produces cached forecast .rds files consumed by the dashboard
#
# USAGE (from project root):
#   Rscript R/run_forecasts.r
#
# Or source interactively in RStudio / Positron:
#   source("R/run_forecasts.r")
#
# Output: data/processed/forecasts_<condition>.rds  (one per condition)
#         data/processed/forecasts_rolling_<condition>.rds
#         data/processed/forecasts_meta.rds  (run timestamp + conditions)
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(lubridate)
  library(purrr)
  library(EpiNow2)
})

# ── Paths ────────────────────────────────────────────────────────────────────
project_root <- here::here()
source(file.path(project_root, "R/epinow2_functions.r"))

out_dir  <- file.path(project_root, "data/processed")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ── Load aggregated data (created by prepare_dashboard_data.r) ───────────────
agg_national <- readRDS(file.path(out_dir, "agg_national.rds"))
setDT(agg_national)

# ── Combine Measles + Rubella → Fever-Rash ───────────────────────────────────
agg_vp <- filter_vp_conditions(agg_national, include_fever_rash = TRUE)

# Which conditions actually have data?
available <- unique(agg_vp$condition)
cat("Conditions with data:", paste(available, collapse = ", "), "\n")

# ── Configure run ────────────────────────────────────────────────────────────
HORIZON       <- 21L    # forecast days ahead
LOOKBACK      <- 360L   # history window
N_REWINDS     <- 8L     # rolling snapshots for animation
REWIND_STEP   <- 7L     # days between snapshots

# ── Single-point forecasts ───────────────────────────────────────────────────
cat("\n──── Running single-point forecasts ────\n")

walk(available, function(cond) {
  cat("  ▸ ", cond, " ... ")

  ts <- prepare_condition_ts(agg_vp, cond)

  if (nrow(ts) < 60) {
    cat("skipped (< 60 days of data)\n")
    return(invisible(NULL))
  }

  tryCatch({
    est <- run_epinow_forecast(
      case_ts       = ts,
      condition     = cond,
      horizon       = HORIZON,
      lookback_days = LOOKBACK
    )
    summ <- extract_forecast_summary(est, cond)
    fname <- file.path(out_dir, paste0("forecasts_", gsub("[- ]", "_", tolower(cond)), ".rds"))
    saveRDS(summ, fname)
    cat("saved →", basename(fname), "\n")
  }, error = function(e) {
    cat("FAILED:", e$message, "\n")
  })
})

# ── Rolling forecasts (for animated outlook) ─────────────────────────────────
cat("\n──── Running rolling forecasts (animation data) ────\n")

walk(available, function(cond) {
  cat("  ▸ ", cond, " ... ")

  ts <- prepare_condition_ts(agg_vp, cond)

  if (nrow(ts) < 60) {
    cat("skipped\n")
    return(invisible(NULL))
  }

  tryCatch({
    roll <- rolling_forecast(
      case_ts       = ts,
      condition     = cond,
      n_rewinds     = N_REWINDS,
      rewind_step   = REWIND_STEP,
      lookback_days = min(LOOKBACK, 180),
      horizon       = HORIZON
    )
    fname <- file.path(out_dir, paste0("forecasts_rolling_", gsub("[- ]", "_", tolower(cond)), ".rds"))
    saveRDS(roll, fname)
    cat("saved →", basename(fname), "\n")
  }, error = function(e) {
    cat("FAILED:", e$message, "\n")
  })
})

# ── Metadata ─────────────────────────────────────────────────────────────────
meta <- list(
  run_time   = Sys.time(),
  conditions = available,
  horizon    = HORIZON,
  lookback   = LOOKBACK,
  n_rewinds  = N_REWINDS,
  rewind_step = REWIND_STEP
)
saveRDS(meta, file.path(out_dir, "forecasts_meta.rds"))

cat("\n✔ All forecasts complete. Metadata saved to forecasts_meta.rds\n")
cat("  Run time:", format(meta$run_time), "\n")
