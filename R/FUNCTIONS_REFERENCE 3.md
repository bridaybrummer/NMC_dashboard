# NMC Dashboard — R Function Reference

> Auto-generated reference for all R modules in the `R/` directory.  
> Source everything via `source("R/_load_all.r")` from the project root.

---

## Table of Contents

1. [plot_themes.r](#plot_themesr) — Themes, palettes, scale helpers
2. [epicurve_functions.r](#epicurve_functionsr) — Epidemic curve plots
3. [signal_detection.r](#signal_detectionr) — EARS & CUSUM signal detection
4. [incidence_functions.r](#incidence_functionsr) — Incidence rates & age-standardisation
5. [asr_functions.r](#asr_functionsr) — Full age-standardised rate pipeline (ASR)
6. [map_functions.r](#map_functionsr) — Choropleth maps (province & district)
7. [visualisation_extras.r](#visualisation_extrasr) — Ridge plots, bubble matrix, context table, sparklines
8. [quality_functions.r](#quality_functionsr) — Duplicates, timeliness, completeness
9. [tabulate_functions.r](#tabulate_functionsr) — Summary tables
10. [epinow2_functions.r](#epinow2_functionsr) — EpiNow2 forecasting
11. [prepare_dashboard_data.r](#prepare_dashboard_datar) — Data pipeline (feather → RDS)
12. [aggregator_national.r](#aggregator_nationalr) — National aggregation
13. [data_import.r](#data_importr) — Raw feather import
14. [run_forecasts.r](#run_forecastsr) — Batch forecast runner

---

## plot_themes.r

Dashboard-wide ggplot2 themes and colour palettes.

### Constants

| Name | Type | Description |
|------|------|-------------|
| `nmc_palette` | character(18) | Extended Paired + custom colours for 18+ conditions |
| `province_colours` | named character(9) | Fixed colour per province code (EC, FS, GP, …) |
| `province_names` | named character(9) | Province code → full name lookup |

### Functions

#### `theme_nmc(base_size = 11)`
Light, clean ggplot2 theme for dashboard panels.  
**Returns:** ggplot2 theme object.

#### `theme_nmc_map(base_size = 11)`
Map variant — removes axes, ticks, gridlines.  
**Returns:** ggplot2 theme object.

#### `scale_fill_nmc(...)`
`scale_fill_manual()` using `nmc_palette`.

#### `scale_colour_nmc(...)`
`scale_colour_manual()` using `nmc_palette`.

#### `scale_fill_province(...)`
`scale_fill_manual()` using `province_colours` (9 fixed).

#### `scale_colour_province(...)`
`scale_colour_manual()` using `province_colours`.

---

## epicurve_functions.r

Epidemic curve plotting. All functions accept aggregated data.tables from `prepare_dashboard_data.r`.

### Functions

#### `plot_epicurve(dt, date_col, count_col, fill_col, date_breaks, date_labels, title, subtitle)`

Weekly epidemic curve (static ggplot). Aggregates daily data to epiweek internally.

| Param | Default | Description |
|-------|---------|-------------|
| `dt` | — | data.table with date, count columns |
| `date_col` | `"date"` | Column name for date |
| `count_col` | `"count"` | Column name for counts |
| `fill_col` | `NULL` | Optional: column for stacked fill (e.g. `"condition"`) |
| `date_breaks` | `"1 month"` | x-axis date breaks |
| `date_labels` | `"%b %Y"` | x-axis date format |

**Returns:** ggplot object.

#### `plotly_epicurve(dt, ..., height = 400)`

Interactive plotly wrapper around `plot_epicurve()`. Adds hover tooltips and horizontal legend.

**Returns:** plotly object.

#### `plot_monthly_bars(dt, date_col, count_col, top_n, title)`

Stacked monthly bars by condition. Groups lesser conditions into "Other".

| Param | Default | Description |
|-------|---------|-------------|
| `top_n` | `10` | Show only top N conditions |

**Returns:** ggplot object.

#### `plot_epicurve_faceted(dt, date_col, count_col, facet_col, fill_col, date_breaks, ncol, title)`

Small-multiple epicurves, one facet per group (typically province).

| Param | Default | Description |
|-------|---------|-------------|
| `facet_col` | `"prov_"` | Faceting column |
| `fill_col` | `"condition"` | Fill column |
| `ncol` | `3` | Facet grid columns |

**Returns:** ggplot object.

---

## signal_detection.r

EARS-C2 and CUSUM signal detection for the Trending & Signals dashboard page.
All functions are data.table-based with no side effects.

### Functions

#### `ears_detect(dt, baseline = 14L, z_thresh = 3)`

EARS-C2 rolling z-score detector.

| Param | Default | Description |
|-------|---------|-------------|
| `dt` | — | data.table with `date` (Date) and `n` (daily counts) |
| `baseline` | `14L` | Lag window (days) |
| `z_thresh` | `3` | z-score threshold |

**Returns:** `dt` with added columns: `ears_mean`, `ears_sd`, `ears_z`, `ears_threshold`, `is_ears_signal`.

#### `cusum_detect(dt, k = 0.5, cusum_thresh = 5)`

One-sided upper CUSUM on daily time series.

**Returns:** `dt` with: `z_cusum`, `cusum_value`, `is_cusum_signal`.

#### `detect_signals(dt, baseline, z_thresh, k, cusum_thresh)`

Runs both EARS + CUSUM and adds unified `is_signal` flag.

**Returns:** `dt` with all signal columns.

#### `cusum_sliding(dt, window = 180L, k, cusum_thresh, start_date, quiet)`

Sliding-window CUSUM so distant history doesn't dominate baseline.

**Returns:** data.table: `date`, `n`, `condition`, `cusum_value`, `is_cusum_signal`.

#### `detect_signals_batch(dt, group_cols = "condition", ...)`

Runs `detect_signals()` for every group (e.g. each condition or condition × province).

**Returns:** data.table with all signal columns, grouped.

---

## incidence_functions.r

Crude incidence rate calculations by province and age group, and a simple ASR calculator.

### Constants

| Name | Type | Description |
|------|------|-------------|
| `age_levels_default` | character(14) | `"0-4", "5-9", …, "60-64", "65+"` |
| `who_standard_pop` | named numeric(14) | WHO standard weights (Ahmad et al. 2001) per age band (×100 000 scale) |

### Functions

#### `calc_incidence_province(agg_dt, pop_dt = NULL, per = 1e5)`

Province-level incidence per 100 000. Uses built-in 2024 MYPE populations when `pop_dt` is NULL.

| Param | Default | Description |
|-------|---------|-------------|
| `agg_dt` | — | data.table: `condition`, `prov_`, `count` |
| `pop_dt` | built-in | data.table: `prov_`, `pop` |

**Returns:** data.table: `condition`, `prov_`, `cases`, `pop`, `incidence`.

#### `calc_incidence_age(case_dt, pop_dt, age_col = "agecategory", per = 1e5)`

Age-stratified incidence from aggregated case/population data.

**Returns:** data.table: `condition`, `Age`, `cases`, `pop`, `incidence`.

#### `plot_incidence_province(inci_dt, top_n = 12, title)`

Stacked bar chart of incidence by province, coloured by condition.

**Returns:** ggplot object.

#### `plot_incidence_age(inci_dt, top_n = 12, title)`

Stacked bar chart of incidence by age group.

**Returns:** ggplot object.

#### `calc_asr(inci_dt, std_pop = who_standard_pop, per = 1e5)`

Simple age-standardised rate (direct method). Returns a single numeric value.  
For the full ASR pipeline with CIs, sex stratification, tables and plots, use `asr_functions.r` instead.

| Param | Default | Description |
|-------|---------|-------------|
| `inci_dt` | — | data.table with `Age`, `cases`, `pop` |
| `std_pop` | `who_standard_pop` | Named weight vector |

**Returns:** numeric — ASR per 100 000.

---

## asr_functions.r

Full age-standardised rate (ASR) pipeline — the complete toolkit adapted from the legacy `Age_Standardise_Rates.r`. Provides Poisson CIs, sex-stratified rates, IRR comparisons, publication-quality tables and plots.

### Constants

| Name | Type | Description |
|------|------|-------------|
| `asr_age_levels` | character(15) | `"0-1", "1-4", "5-9", …, "60-64", "65+"` (split 0-4 into 0-1 / 1-4) |
| `who_std_weights` | data.table | `Age`, `weight`, `weight_pct` — WHO standard pop with 0-1/1-4 split and 65+ collapse |
| `sa_province_pops_2024` | named numeric(9) | SA province populations (2024 MYPE) |

### Functions

#### `assign_age_band(age_years, levels = asr_age_levels)`

Map continuous age in years to the 15-band factor with 0-1 / 1-4 split.

| Param | Description |
|-------|-------------|
| `age_years` | Numeric vector of ages |

**Returns:** factor vector with levels = `asr_age_levels`.

#### `build_population_by_age_sex(pop_dt, years, split_0_1 = 1/5)`

Build population denominator table by Age × Sex × Year from NMCleaner `pop` data, splitting 0-4 into 0-1 and 1-4, and collapsing 65+ bands.

| Param | Default | Description |
|-------|---------|-------------|
| `pop_dt` | — | data.table with `Age_group`, `Sex`, `Year`, `Population` (from `NMCleaner::pop`) |
| `years` | — | integer vector of years to include |
| `split_0_1` | `1/5` | Proportion of 0-4 pop assigned to 0-1 |

**Returns:** data.table: `Age`, `Sex`, `Year`, `pop`, `weight`, `weight_pct`.

#### `calc_asr_by_group(case_dt, pop_dt, group_cols = "Sex", per = 1e5)`

Calculate age-specific rates and direct ASR for arbitrary groupings (sex, province, year, condition, etc.). Attaches Poisson exact 95% CIs via `epitools::pois.exact()`.

| Param | Default | Description |
|-------|---------|-------------|
| `case_dt` | — | data.table: `Age`, group_cols columns, `cases` |
| `pop_dt` | — | data.table: `Age`, group_cols, `pop`, `weight` |
| `group_cols` | `"Sex"` | character vector of grouping columns |
| `per` | `1e5` | Rate denominator |

**Returns:** data.table with: `Age`, group columns, `cases`, `pop`, `weight`, `rate_per_100k`, `asr_contribution`, `ci_lower`, `ci_upper`.

#### `calc_sex_irr(asr_dt)`

Compute incidence rate ratios (Female vs Male) with Poisson exact test per age band.

| Param | Description |
|-------|-------------|
| `asr_dt` | data.table from `calc_asr_by_group()` containing both Sex levels |

**Returns:** wide data.table with: `Age`, rates for each sex, `irr`, `irr_lower`, `irr_upper`, `p_value`.

#### `asr_summary(asr_dt, group_cols = "Sex")`

Collapse age-specific rows into a single ASR value per group with Poisson CI.

**Returns:** data.table: group columns, `asr`, `asr_lower`, `asr_upper`, `total_cases`, `total_pop`.

#### `asr_table(asr_wide_dt, condition_label = "")`

Publication-quality flextable of age-specific + age-standardised rates by sex, with IRR and p-values. Two-row header (Gender / Measure), WHO footnote, Poisson footnote.

| Param | Description |
|-------|-------------|
| `asr_wide_dt` | Output from `calc_sex_irr()` |
| `condition_label` | Title / caption text |

**Returns:** flextable object.

#### `asr_line_plot(asr_dt, condition_label = "", ci = TRUE)`

Publication-quality line + ribbon plot of age-standardised rates by sex. Uses NMC theme with direct end-labels (no legend).

| Param | Default | Description |
|-------|---------|-------------|
| `asr_dt` | — | data.table from `calc_asr_by_group()` |
| `condition_label` | `""` | Plot title text |
| `ci` | `TRUE` | Show confidence ribbons |

**Returns:** ggplot object.

#### `asr_pipeline(case_level_dt, pop_dt = NULL, years = 2020:2024, condition_label = "", split_0_1 = 1/5)`

End-to-end convenience wrapper. Takes case-level data, builds population, computes ASR by sex, returns a list of all outputs.

| Param | Default | Description |
|-------|---------|-------------|
| `case_level_dt` | — | data.table with `age_years` (or `Age_years`), `sex` (or `gender`/`patient_gender`/`Sex`), and optionally `year` |
| `pop_dt` | `NULL` → uses `NMCleaner::pop` | Population data |
| `years` | `2020:2024` | Years to include |
| `condition_label` | `""` | Label for tables/plots |
| `split_0_1` | `1/5` | 0-1 age band proportion |

**Returns:** named list:
- `$table` — flextable
- `$plot` — ggplot (line + ribbon)
- `$data` — full data.table with rates, CIs, weights
- `$data_by_year` — year-stratified version
- `$summary` — ASR summary by sex

---

## map_functions.r

Choropleth maps using `sf` + `ggplot2` + `plotly`. Shapefiles from `NMCleaner::shape_files`.

### Helpers

#### `get_province_shape()`
Returns province-level sf object (9 polygons). Merges district shapes from NMCleaner.

#### `get_district_shape()`
Returns district-level sf object from NMCleaner.

#### `standardise_district(x)`
Lowercases and strips "city", "metro", "of", "district" for matching.

### Functions

#### `plot_province_map(cases_dt, value_col = "cases", title, fill_lab, interactive = TRUE)`

Province choropleth with text labels.

| Param | Default | Description |
|-------|---------|-------------|
| `cases_dt` | — | data.table: `prov_`, value column |
| `interactive` | `TRUE` | Return plotly or ggplot |

**Returns:** plotly or ggplot object.

#### `plot_province_incidence_map(inci_dt, condition_name, interactive = TRUE)`

Convenience wrapper for incidence maps (fills `"incidence"` column).

#### `plot_district_map(cases_dt, value_col, title, fill_lab, top_n_label = 10, interactive = TRUE)`

District-level choropleth. Labels top N districts by value.

**Returns:** plotly or ggplot object.

---

## visualisation_extras.r

Speciality visualisation functions.

### Functions

#### `make_fever_rash(dt, label = "Fever-Rash")`

Recodes Measles / Rubella → "Fever-Rash" in the `condition` column.

**Returns:** data.table.

#### `plot_condition_ridges(dt, conditions, jittered = TRUE, scale_factor = 1.5, title)`

Ridge-line density plot of notification dates by condition. Requires `ggridges`.

| Param | Description |
|-------|-------------|
| `dt` | data.table with `notification_date`, `condition` |
| `conditions` | character vector; `NULL` → auto-order by frequency |

**Returns:** ggplot object.

#### `plot_bubble_matrix(dt, count_col = "cases", max_size = 18, title)`

Bubble-matrix: condition (y) × province (x), size = count.

**Returns:** ggplot object.

#### `build_context_table(dt, n_weeks = 12L, conditions = NULL)`

Wide table of weekly counts (conditions × epiweeks) with historical weekly average.

| Param | Default | Description |
|-------|---------|-------------|
| `dt` | — | data.table: `date`, `condition`, `count` |
| `n_weeks` | `12` | Number of recent epiweeks |

**Returns:** wide data.table (suitable for `DT::datatable()`).

#### `calc_completeness(dt, vars, by_col = "prov_")`

Percentage of non-missing / non-"unknown" values for each variable.

| Param | Default | Description |
|-------|---------|-------------|
| `vars` | `c("folder_no", "patient_name", ...)` | Variables to assess |
| `by_col` | `"prov_"` | Grouping column |

**Returns:** data.table: `by_col`, `variable`, `n_total`, `n_complete`, `pct_complete`.

#### `plot_completeness_heatmap(comp_dt, by_col = "prov_", title)`

Tile heatmap of completeness percentages (red→yellow→green).

**Returns:** ggplot object.

#### `sparkline_svg(x, width = 80, height = 20, colour = "#1d4ed8")`

Inline SVG sparkline for embedding in DT tables.

**Returns:** HTML string.

---

## quality_functions.r

Data quality functions for the Data Quality dashboard page.

### Functions

#### `tag_duplicates(dt, id_col = "case_id_2", date_col = "notification_date")`

Tags potential duplicate notifications.

**Returns:** `dt` with `is_duplicate` (logical), `dup_rank` (integer), `dup_count`.

#### `summarise_duplicates(dt)`

Summary of duplicates by condition.

**Returns:** data.table: `condition`, `total_notifications`, `n_unique`, `n_duplicate`, `pct_duplicate`.

#### `calc_timeliness(dt, by_col = "prov_")`

Median notification delay (symptom_date → notification_date) with IQR.

**Returns:** data.table: `by_col`, `median_delay_days`, `p25`, `p75`, `n_with_dates`.

#### `calc_submission_heatmap(dt, date_col = "notification_date")`

Submission volume data by epiweek × day-of-week.

**Returns:** data.table: `epiweek`, `dow`, `N`.

#### `completeness_trend(dt, var_name = "symptom_date", date_col = "notification_date")`

Monthly completeness trend for a single variable.

**Returns:** data.table: `month`, `n_total`, `n_complete`, `pct_complete`.

---

## tabulate_functions.r

Summary tables for dashboard pages. **Note:** these depend on a global `agg_dt` object.

### Functions

#### `tabulate_condition(province)`

Summary table for a given province showing counts by condition.

#### `tabulate_province(condition)`

Summary table for a given condition showing counts by province.

---

## epinow2_functions.r

EpiNow2-based forecasting for vaccine-preventable / respiratory NMC conditions.

### Functions

#### `disease_params(condition)`

Lookup table of generation time, incubation period, and Rt prior for:
`"fever-rash"`, `"measles"`, `"rubella"`, `"pertussis"`, `"influenza"`, `"rsv"` (+ default fallback).

**Returns:** named list: `$generation_time`, `$incubation_period`, `$rt_prior`.

#### `estimate_reporting_delay(dt, max_sample = 100, max_value = 15, bootstraps = 1)`

Fits symptom→notification delay from case-level data.

**Returns:** EpiNow2 delay distribution.

#### `run_epinow_forecast(case_ts, condition, reporting_delay, horizon = 21, lookback_days = 360, CrIs)`

Full EpiNow2 forecast for one condition.

| Param | Default | Description |
|-------|---------|-------------|
| `case_ts` | — | data.frame: `date` (Date), `confirm` (int) |
| `condition` | — | Disease name (for parameter lookup) |
| `horizon` | `21` | Forecast days |
| `lookback_days` | `360` | History window |

**Returns:** EpiNow2 `epinow()` result object.

#### `extract_forecast_summary(epinow_result, condition_label = "Unknown")`

Tidy summary from EpiNow2 result.

**Returns:** data.frame with forecast summaries.

#### `rolling_forecast(case_ts, condition, reporting_delay, n_rewinds = 8, rewind_step = 7, lookback_days = 180, horizon = 21)`

Repeated forecasts from sequential snapshot dates (for animation/evaluation).

**Returns:** data.frame with `snapshot_date` column.

#### `prepare_condition_ts(agg_dt, target_condition, region_filter = NULL)`

Extracts a continuous daily time series (`date`, `confirm`) from aggregated dashboard data.

#### `combine_fever_rash(agg_dt)`

Merges Measles + Rubella → "Fever-Rash" and re-aggregates.

#### `vp_conditions()`

Returns character vector: `"Fever-Rash", "Measles", "Rubella", "Pertussis", "Influenza", "RSV"`.

#### `filter_vp_conditions(agg_dt, include_fever_rash = TRUE)`

Filters + optionally combines to vaccine-preventable conditions only.

---

## prepare_dashboard_data.r

Data import and aggregation pipeline. Loads `new_master.feather` from `~/Desktop/SAFETP/CLA/NMC_database/master/`, cleans, and saves aggregated RDS files to `data/processed/`.

### Key outputs

| File | Description |
|------|-------------|
| `data/processed/agg_national.rds` | National daily counts by condition |
| `data/processed/agg_province.rds` | Province daily counts by condition × prov_ |
| `data/processed/agg_district.rds` | District daily counts by condition × district |

### Internal function

#### `log_msg(msg)`

Timestamped message logger.

---

## aggregator_national.r

Simple script that aggregates `new_master` into `agg_dt` with columns: `prov_`, `condition`, `date`, `year`, `n`.

---

## data_import.r

Loads the raw feather file:
```r
new_master <- arrow::read_feather("~/Desktop/SAFETP/CLA/NMC_database/master/new_master.feather")
```

---

## run_forecasts.r

Batch forecast runner. Configuration constants:

| Constant | Value | Description |
|----------|-------|-------------|
| `HORIZON` | `21` | Forecast days |
| `LOOKBACK` | `360` | History window |
| `N_REWINDS` | `8` | Snapshot count |
| `REWIND_STEP` | `7` | Days between snapshots |

Saves per-condition forecast `.rds` files.

---

## Usage cheat-sheet

```r
# Load everything
source("R/_load_all.r")

# Quick epicurve
plot_epicurve(agg_national, fill_col = "condition")

# Signal detection on one condition
measles_ts <- agg_national[condition == "Measles", .(date, n = count)]
signals <- detect_signals(measles_ts)

# Province incidence
inci <- calc_incidence_province(agg_province)
plot_province_incidence_map(inci[condition == "Measles"])

# Full ASR pipeline (requires asr_functions.r)
result <- asr_pipeline(case_level_data, condition_label = "Acute Viral Hepatitis")
result$table   # flextable
result$plot    # ggplot

# Context table
ctx <- build_context_table(agg_national, n_weeks = 8)
DT::datatable(ctx)

# Data quality
dups <- tag_duplicates(raw_dt) |> summarise_duplicates()
timeliness <- calc_timeliness(raw_dt, by_col = "condition")
```

---

*Last updated: 2025*
