# ──────────────────────────────────────────────────────────────────────────────
# prepare_bod_data.r
# Pre-compute Burden of Disease indicators (YLL, YLD, DALY) and cache as .rds
#
# Run from project root:
#   Rscript R/prepare_bod_data.r
#
# Reads:
#   ~/Desktop/SAFETP/CLA/NMC_database/master/new_master.feather
#
# Writes to data/processed/:
#   bod_deaths.rds        — cleaned death records
#   bod_yll.rds           — YLL by condition (national)
#   bod_yll_province.rds  — YLL by condition × province
#   bod_yll_year.rds      — YLL by condition × year
#   bod_yld.rds           — YLD by condition (national)
#   bod_yld_year.rds      — YLD by condition × year
#   bod_daly.rds          — DALY by condition (national)
#   bod_daly_province.rds — DALY by condition × province
#   bod_daly_year.rds     — DALY by condition × year
#   bod_meta.rds          — run metadata
# ──────────────────────────────────────────────────────────────────────────────

library(data.table)
library(arrow)
library(lubridate)
library(here)

log_msg <- function(...) cat(paste0("[BoD] ", ..., "\n"), file = stdout())

# ── Source BoD functions ────────────────────────────────────────────────────
source(here::here("R", "burden_of_disease.r"))

# ── Output directory ────────────────────────────────────────────────────────
out_dir <- here::here("data", "processed")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# ── 1. Load raw data ───────────────────────────────────────────────────────
feather_path <- file.path(
  Sys.getenv("HOME"),
  "Desktop/SAFETP/CLA/NMC_database/master/new_master.feather"
)
if (!file.exists(feather_path)) stop("Master data not found: ", feather_path)

log_msg("Loading master data...")
dt <- as.data.table(arrow::read_feather(feather_path))
log_msg("Loaded ", nrow(dt), " rows")

# ── 2. Basic cleaning ──────────────────────────────────────────────────────
dt[, date := as.Date(date)]
dt <- dt[!is.na(condition) & condition != "" & !is.na(date)]

# Standardise province codes
valid_provs <- c("EC", "FS", "GP", "KZN", "LP", "MP", "NC", "NW", "WC")
dt <- dt[prov_ %chin% valid_provs]

# Ensure year column exists
if (!"year" %in% names(dt)) dt[, year := year(date)]

log_msg("After cleaning: ", nrow(dt), " rows, ",
        uniqueN(dt$condition), " conditions")

# ── 3. Province populations (for per-capita rates) ─────────────────────────
pop_province <- data.table(
  prov_ = c("EC",      "FS",      "GP",       "KZN",      "LP",
             "MP",      "NC",      "NW",       "WC"),
  pop   = c(6734001,   2928903,   16098571,   11538813,   5941439,
             4743584,   1303047,   4141774,    7187004)
)
pop_total <- sum(pop_province$pop)

# ── 4. Extract deaths ──────────────────────────────────────────────────────
log_msg("Extracting death records...")
deaths <- extract_deaths(dt)
log_msg("Deaths found: ", nrow(deaths))

saveRDS(deaths, file.path(out_dir, "bod_deaths.rds"))

# ── 5. YLL — National ──────────────────────────────────────────────────────
log_msg("Computing YLL (national)...")
yll_national <- compute_yll(deaths)
saveRDS(yll_national, file.path(out_dir, "bod_yll.rds"))
log_msg("  Top condition: ", yll_national$condition[1],
        " (", yll_national$total_yll[1], " YLL)")

# ── 6. YLL — by Province ───────────────────────────────────────────────────
log_msg("Computing YLL by province...")
yll_prov <- compute_yll(deaths, by_cols = "prov_")
saveRDS(yll_prov, file.path(out_dir, "bod_yll_province.rds"))

# ── 7. YLL — by Year ───────────────────────────────────────────────────────
log_msg("Computing YLL by year...")
yll_year <- compute_yll(deaths, by_cols = "year")
saveRDS(yll_year, file.path(out_dir, "bod_yll_year.rds"))

# ── 8. YLD — National ──────────────────────────────────────────────────────
log_msg("Computing YLD (national)...")
cases_national <- aggregate_cases(dt)
yld_national <- compute_yld(cases_national)
saveRDS(yld_national, file.path(out_dir, "bod_yld.rds"))

# ── 9. YLD — by Year ───────────────────────────────────────────────────────
log_msg("Computing YLD by year...")
cases_year <- aggregate_cases(dt, by_cols = "year")
yld_year <- compute_yld(cases_year, by_cols = "year")
saveRDS(yld_year, file.path(out_dir, "bod_yld_year.rds"))

# ── 10. DALY — National ────────────────────────────────────────────────────
log_msg("Computing DALYs (national)...")
daly_national <- compute_daly(yll_national, yld_national)
saveRDS(daly_national, file.path(out_dir, "bod_daly.rds"))

# ── 11. DALY — by Province ─────────────────────────────────────────────────
log_msg("Computing DALYs by province...")
# YLD by province
cases_prov <- aggregate_cases(dt, by_cols = "prov_")
yld_prov <- compute_yld(cases_prov, by_cols = "prov_")

daly_prov <- compute_daly(yll_prov, yld_prov, by_cols = "prov_")

# Add population rates
daly_prov <- merge(daly_prov, pop_province, by = "prov_", all.x = TRUE)
daly_prov[, daly_rate_100k := round(total_daly / pop * 1e5, 2)]
daly_prov[, yll_rate_100k  := round(total_yll / pop * 1e5, 2)]

saveRDS(daly_prov, file.path(out_dir, "bod_daly_province.rds"))

# ── 12. DALY — by Year ─────────────────────────────────────────────────────
log_msg("Computing DALYs by year...")
daly_year <- compute_daly(yll_year, yld_year, by_cols = "year")
saveRDS(daly_year, file.path(out_dir, "bod_daly_year.rds"))

# ── 13. Province-level summary (all conditions aggregated) ──────────────────
prov_summary <- daly_prov[, .(
  total_daly = sum(total_daly, na.rm = TRUE),
  total_yll  = sum(total_yll, na.rm = TRUE),
  total_yld  = sum(total_yld, na.rm = TRUE),
  deaths     = sum(deaths, na.rm = TRUE),
  cases      = sum(cases, na.rm = TRUE)
), by = prov_]
prov_summary <- merge(prov_summary, pop_province, by = "prov_", all.x = TRUE)
prov_summary[, daly_rate_100k := round(total_daly / pop * 1e5, 2)]
prov_summary[, yll_rate_100k  := round(total_yll / pop * 1e5, 2)]
saveRDS(prov_summary, file.path(out_dir, "bod_province_summary.rds"))

# ── 14. Metadata ───────────────────────────────────────────────────────────
meta <- list(
  run_time         = Sys.time(),
  n_deaths         = nrow(deaths),
  n_conditions     = uniqueN(dt$condition),
  date_range       = range(dt$date, na.rm = TRUE),
  year_range       = range(dt$year, na.rm = TRUE),
  life_table       = "Stats SA 2024 MYPE",
  disability_src   = "GBD 2019; Salomon et al. Lancet 2015"
)
saveRDS(meta, file.path(out_dir, "bod_meta.rds"))

# ── 15. Case Fatality Rate ─────────────────────────────────────────────────
log_msg("Computing CFR by condition...")
cfr_national <- compute_cfr(dt)
saveRDS(cfr_national, file.path(out_dir, "bod_cfr.rds"))
log_msg("  CFR computed for ", nrow(cfr_national), " conditions")

log_msg("Computing CFR by condition × province...")
cfr_prov <- compute_cfr(dt, by_cols = "prov_")
saveRDS(cfr_prov, file.path(out_dir, "bod_cfr_province.rds"))

log_msg("Computing CFR by condition × year...")
cfr_year <- compute_cfr(dt, by_cols = "year")
saveRDS(cfr_year, file.path(out_dir, "bod_cfr_year.rds"))

# Age-group for CFR stratification
dt[, age_group := cut(as.numeric(Age_years),
  breaks = c(0, 1, 5, seq(10, 65, 5), Inf),
  labels = c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
             "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
             "60-64", "65+"),
  right = FALSE
)]

log_msg("Computing CFR by condition × age_group × gender...")
cfr_age_sex <- compute_cfr(dt, by_cols = c("age_group", "gender"))
saveRDS(cfr_age_sex, file.path(out_dir, "bod_cfr_age_sex.rds"))

# ── 16. PYPLL (Potential Years of Productive Life Lost) ────────────────────
log_msg("Computing PYPLL (national)...")
pypll_national <- compute_pypll(deaths)
saveRDS(pypll_national, file.path(out_dir, "bod_pypll.rds"))
log_msg("  Top condition: ", pypll_national$condition[1],
        " (", pypll_national$total_pypll[1], " PYPLL)")

log_msg("Computing PYPLL by year...")
pypll_year <- compute_pypll(deaths, by_cols = "year")
saveRDS(pypll_year, file.path(out_dir, "bod_pypll_year.rds"))

log_msg("Computing PYPLL by province...")
pypll_prov <- compute_pypll(deaths, by_cols = "prov_")
saveRDS(pypll_prov, file.path(out_dir, "bod_pypll_province.rds"))

# ── 17. Age-sex case counts for pyramids ───────────────────────────────────
log_msg("Building age-sex pyramid data...")
dt_pyramid <- dt[gender %in% c("Male", "Female") & !is.na(Age_years)]
dt_pyramid[, age_band := cut(as.numeric(Age_years),
  breaks = c(0, 1, 5, seq(10, 65, 5), Inf),
  labels = c("0-1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
             "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
             "60-64", "65+"),
  right = FALSE
)]

pyramid_national <- dt_pyramid[, .(cases = .N), by = .(age_band, gender)]
saveRDS(pyramid_national, file.path(out_dir, "bod_pyramid.rds"))

# Per top-condition pyramids
top_cond_names <- head(daly_national[order(-total_daly)]$condition, 10)
pyramid_by_condition <- dt_pyramid[condition %in% top_cond_names,
  .(cases = .N), by = .(condition, age_band, gender)]
saveRDS(pyramid_by_condition, file.path(out_dir, "bod_pyramid_condition.rds"))

# ── 18. DALY rank by year (for bump chart) ─────────────────────────────────
log_msg("Building DALY rank-by-year data...")
daly_year_loaded <- daly_year  # already computed above
rank_dt <- daly_year_loaded[, .(condition, year, total_daly)]
rank_dt[, rank := frank(-total_daly), by = year]
saveRDS(rank_dt, file.path(out_dir, "bod_rank_year.rds"))

log_msg("Done. Files saved to: ", out_dir)
log_msg("Summary: ", nrow(deaths), " deaths → ",
        round(sum(yll_national$total_yll)), " total YLL, ",
        round(sum(daly_national$total_daly)), " total DALYs across ",
        nrow(daly_national), " conditions")
