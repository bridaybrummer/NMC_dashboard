# ──────────────────────────────────────────────────────────────────────────────
# prepare_asr_data.r
# Age-Standardised Rate (ASR) data pipeline for the NMC dashboard
#
# Reads the master feather file, runs the asr_pipeline() for:
#   1. All NMC conditions combined (national overview)
#   2. Each of the top N conditions individually
#   3. Province-level ASR for all conditions combined
#
# Outputs cached .rds files in data/processed/
#
# Usage:
#   cd /Users/briday/Desktop/SAFETP/CLA/NMC_website/NMC_dashboard
#   Rscript R/prepare_asr_data.r
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table)
  library(dplyr)
  library(arrow)
  library(here)
})

project_root <- here::here()
source(file.path(project_root, "R/asr_functions.r"))

# ── Configuration ────────────────────────────────────────────────────────────
MASTER_PATH <- normalizePath(
  file.path("~", "Desktop", "SAFETP", "CLA", "NMC_database", "master", "new_master.feather"),
  mustWork = FALSE
)
OUT_DIR     <- file.path(project_root, "data", "processed")
YEARS       <- 2020:2024
TOP_N       <- 20   # number of top conditions to run individual pipelines for

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# ── Load master data ─────────────────────────────────────────────────────────
message("Loading master feather: ", MASTER_PATH)
if (!file.exists(MASTER_PATH)) stop("Master feather not found at: ", MASTER_PATH)

master <- as.data.table(arrow::read_feather(MASTER_PATH))
message("Loaded ", format(nrow(master), big.mark = ","), " rows")

# ── Standardise columns for ASR pipeline ─────────────────────────────────────
# The asr_pipeline expects: age (numeric years), sex, year/date
# Master has: Age_years, gender, year, date

# Keep only relevant columns to save memory
keep_cols <- intersect(
  c("Age_years", "gender", "year", "date", "condition", "prov_",
    "notification_date", "symptom_date"),
  names(master)
)
dt <- master[, ..keep_cols]
rm(master); gc()

# Filter to valid years
dt[, year := as.integer(as.character(year))]
dt <- dt[year %in% YEARS]
message("After year filter (", paste(range(YEARS), collapse = "-"), "): ",
        format(nrow(dt), big.mark = ","), " rows")

# Filter to valid age and sex
dt <- dt[!is.na(Age_years) & Age_years >= 0]
dt <- dt[gender %in% c("Male", "Female")]
message("After age/sex filter: ", format(nrow(dt), big.mark = ","), " rows")

# ══════════════════════════════════════════════════════════════════════════════
# 1. NATIONAL ASR — ALL CONDITIONS COMBINED
# ══════════════════════════════════════════════════════════════════════════════
message("\n", strrep("═", 60))
message("1. National ASR — all NMC conditions combined")
message(strrep("═", 60))

national_result <- tryCatch(
  asr_pipeline(
    case_level_dt  = dt,
    years          = YEARS,
    condition_label = "All NMC Conditions"
  ),
  error = function(e) {
    message("ERROR in national ASR: ", e$message)
    NULL
  }
)

if (!is.null(national_result)) {
  saveRDS(national_result$data,        file.path(OUT_DIR, "asr_national.rds"))
  saveRDS(national_result$data_wide,   file.path(OUT_DIR, "asr_national_wide.rds"))
  saveRDS(national_result$summary,     file.path(OUT_DIR, "asr_national_summary.rds"))
  saveRDS(national_result$data_by_year, file.path(OUT_DIR, "asr_national_by_year.rds"))
  message("  National ASR saved successfully")
  message("  Female cases: ", format(sum(national_result$data[Sex == "Female"]$cases), big.mark = ","),
          " | Male cases: ", format(sum(national_result$data[Sex == "Male"]$cases), big.mark = ","))
}

# ══════════════════════════════════════════════════════════════════════════════
# 2. PER-CONDITION ASR — TOP N CONDITIONS
# ══════════════════════════════════════════════════════════════════════════════
message("\n", strrep("═", 60))
message("2. Per-condition ASR — top ", TOP_N, " conditions")
message(strrep("═", 60))

# Identify top conditions by total case count
cond_counts <- dt[, .(cases = .N), by = condition][order(-cases)]
top_conditions <- head(cond_counts$condition, TOP_N)

message("Top ", TOP_N, " conditions: ", paste(head(top_conditions, 5), collapse = ", "), ", ...")

condition_results <- list()

for (cond in top_conditions) {
  message("  Processing: ", cond, " (", format(cond_counts[condition == cond, cases], big.mark = ","), " cases)")

  cond_dt <- dt[condition == cond]

  result <- tryCatch(
    asr_pipeline(
      case_level_dt   = cond_dt,
      years           = YEARS,
      condition_label = cond
    ),
    error = function(e) {
      message("    WARN: ", e$message)
      NULL
    }
  )

  if (!is.null(result)) {
    # Store the key data (not the flextable/ggplot objects — not serialisable reliably)
    condition_results[[cond]] <- list(
      data        = result$data,
      data_wide   = result$data_wide,
      summary     = result$summary,
      data_by_year = result$data_by_year
    )
  }
}

if (length(condition_results) > 0) {
  saveRDS(condition_results, file.path(OUT_DIR, "asr_by_condition.rds"))
  message("  Saved ASR for ", length(condition_results), " conditions")
}

# ══════════════════════════════════════════════════════════════════════════════
# 3. PROVINCE-LEVEL ASR — ALL CONDITIONS COMBINED
# ══════════════════════════════════════════════════════════════════════════════
message("\n", strrep("═", 60))
message("3. Province-level ASR — all conditions combined")
message(strrep("═", 60))

province_results <- list()
provinces <- unique(dt$prov_[!is.na(dt$prov_)])

for (prov in provinces) {
  prov_dt <- dt[prov_ == prov]
  n_cases <- nrow(prov_dt)

  if (n_cases < 50) {
    message("  Skipping ", prov, " (", n_cases, " cases — too few)")
    next
  }

  message("  Processing: ", prov, " (", format(n_cases, big.mark = ","), " cases)")

  result <- tryCatch(
    asr_pipeline(
      case_level_dt   = prov_dt,
      years           = YEARS,
      condition_label = paste0("All NMC — ", prov)
    ),
    error = function(e) {
      message("    WARN: ", e$message)
      NULL
    }
  )

  if (!is.null(result)) {
    province_results[[prov]] <- list(
      data     = result$data,
      data_wide = result$data_wide,
      summary  = result$summary
    )
  }
}

if (length(province_results) > 0) {
  saveRDS(province_results, file.path(OUT_DIR, "asr_by_province.rds"))
  message("  Saved ASR for ", length(province_results), " provinces")
}

# ══════════════════════════════════════════════════════════════════════════════
# 4. BUILD ASR SUMMARY TABLE (ALL CONDITIONS × SEX)
# ══════════════════════════════════════════════════════════════════════════════
message("\n", strrep("═", 60))
message("4. Building cross-condition ASR summary")
message(strrep("═", 60))

# Combine per-condition ASR summaries into a single comparison table
if (length(condition_results) > 0) {
  asr_comparison <- rbindlist(lapply(names(condition_results), function(cond) {
    summ <- condition_results[[cond]]$summary
    if (!is.null(summ) && nrow(summ) > 0) {
      summ[, condition := cond]
      return(summ)
    }
    NULL
  }), fill = TRUE)

  if (nrow(asr_comparison) > 0) {
    saveRDS(asr_comparison, file.path(OUT_DIR, "asr_comparison.rds"))
    message("  Saved cross-condition ASR comparison (", nrow(asr_comparison), " rows)")
  }
}

# ══════════════════════════════════════════════════════════════════════════════
# 5. METADATA
# ══════════════════════════════════════════════════════════════════════════════
asr_meta <- list(
  run_time        = Sys.time(),
  years           = YEARS,
  n_cases_total   = nrow(dt),
  n_conditions    = length(condition_results),
  n_provinces     = length(province_results),
  top_conditions  = top_conditions,
  population_source = "NMCleaner::pop (Stats SA MYPE)"
)
saveRDS(asr_meta, file.path(OUT_DIR, "asr_meta.rds"))

message("\n", strrep("═", 60))
message("ASR pipeline complete!")
message("  Total cases: ", format(nrow(dt), big.mark = ","))
message("  Conditions:  ", length(condition_results))
message("  Provinces:   ", length(province_results))
message("  Output dir:  ", OUT_DIR)
message(strrep("═", 60))
