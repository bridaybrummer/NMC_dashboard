# Predictive Modelling — Update & Maintenance Guide

This document describes how to run, refresh, and extend the **Forecasts** section of the NMC Dashboard.

---

## Architecture overview

```
R/
├── epinow2_functions.r        ← Reusable functions: disease params, EpiNow2 wrappers
├── run_forecasts.r            ← Batch script that produces cached .rds forecast files
├── prepare_dashboard_data.r   ← Aggregates raw NMC data → agg_national / province / district
└── data_import.r              ← Reads the master feather file

by-forecasts/
├── index.qmd                  ← Dashboard page (reads cached .rds, renders plots)
└── UPDATE_INSTRUCTIONS.md     ← This file

data/processed/
├── agg_national.rds           ← Aggregated counts by condition × date
├── agg_province.rds           ← By province
├── agg_district.rds           ← By district
├── forecasts_<condition>.rds  ← Single-point EpiNow2 summaries
├── forecasts_rolling_<condition>.rds  ← Rolling snapshots for animation
└── forecasts_meta.rds         ← Run timestamp + parameters
```

---

## Prerequisites

| Requirement | How to install |
|---|---|
| **R ≥ 4.3** | <https://cran.r-project.org/> |
| **EpiNow2** | `install.packages("EpiNow2")` |
| **CmdStan** | `install.packages("cmdstanr"); cmdstanr::install_cmdstan()` |
| **here** | `install.packages("here")` |
| **data.table, dplyr, lubridate, purrr** | `install.packages(c("data.table","dplyr","lubridate","purrr"))` |
| **gganimate + gifski** (for animated GIF) | `install.packages(c("gganimate","gifski"))` |
| **Quarto ≥ 1.4** | <https://quarto.org/docs/get-started/> |

On **macOS** you also need:

- Xcode command-line tools: `xcode-select --install`
- Optionally Homebrew + `brew install pandoc`

---

## Quick-start: full refresh

```bash
cd /Users/briday/Desktop/SAFETP/CLA/NMC_website/NMC_dashboard

# 1. Aggregate latest NMC data
Rscript R/prepare_dashboard_data.r

# 2. Run EpiNow2 forecasts (takes ~5-15 min depending on # conditions)
Rscript R/run_forecasts.r

# 3. Re-render the forecasts page (or full site)
quarto render by-forecasts/index.qmd
# quarto render   # ← full site
```

---

## Step-by-step details

### 1. Update surveillance data

`R/prepare_dashboard_data.r` loads the NMC master dataset (`.feather`) and produces
three aggregated `.rds` files. It expects the master data at:

```
~/Desktop/SAFETP/CLA/NMC_database/master/new_master.feather
```

If the master file moves, update the path in `prepare_dashboard_data.r` line ~18.

### 2. Run forecasts

`R/run_forecasts.r` reads `agg_national.rds`, filters to vaccine-preventable
conditions, and runs EpiNow2 for each. It produces:

- **Single-point forecasts** – one `.rds` per condition with Rt, cases, infections
- **Rolling forecasts** – 8 weekly snapshots per condition (for animation)
- **Metadata** – run timestamp, horizon, conditions list

**Tuning parameters** (edit at the top of `run_forecasts.r`):

| Variable | Default | Effect |
|---|---|---|
| `HORIZON` | 21 | Days forecasted ahead |
| `LOOKBACK` | 360 | Days of surveillance history used |
| `N_REWINDS` | 8 | Number of rolling snapshots |
| `REWIND_STEP` | 7 | Days between rolling snapshots |

### 3. Render

The QMD page reads cached `.rds` files — it does **not** run EpiNow2 live.
This keeps rendering fast (~30 s) while the model runs stay separate.

---

## Adding a new condition

1. **Data**: ensure the condition name appears in the `condition` column of `agg_national.rds`.
   If it's a new NMC category, update `prepare_dashboard_data.r` to include it.

2. **Disease parameters**: in `R/epinow2_functions.r`, add a new entry to the
   `disease_params()` switch statement:

   ```r
   "my_new_condition" = list(
     generation_time   = EpiNow2::Gamma(shape = Normal(...), rate = Normal(...), max = ...),
     incubation_period = EpiNow2::Gamma(mean = ..., sd = ..., max = ...),
     rt_prior          = list(mean = ..., sd = ...)
   ),
   ```

3. **Condition list**: add the name to the `vp_conditions()` function in the same file.

4. **Colour**: add a colour entry in `by-forecasts/index.qmd` → `condition_colours` vector.

5. **Re-run** `Rscript R/run_forecasts.r` then `quarto render by-forecasts/index.qmd`.

---

## Synergy with existing scripts

| Existing script | How forecasts integrate |
|---|---|
| `NMC_reports_scripts/Disease_epinow.qmd` | Original EpiNow2 prototype. The `R/epinow2_functions.r` module refactors its logic into reusable functions (`disease_params()`, `run_epinow_forecast()`, `rolling_forecast()`). |
| `scripts_and_functions/time_series_functions.r` | CUSUM / EARS signal detection. The by-trending page feeds signals here; the forecast page builds on those signals with predictive models. |
| `scripts_and_functions/make_fever_rash.R` | Combines Measles + Rubella. Mirrored by `combine_fever_rash()` in `epinow2_functions.r`. |
| `scripts_and_functions/GGAnimate_FeverRash.r` | Map animation. The forecast rolling animation uses a similar `gganimate` approach for time-series. |
| `R/prepare_dashboard_data.r` | Produces the aggregated `.rds` files that both the condition page and forecasts page consume. |

---

## Troubleshooting

| Problem | Solution |
|---|---|
| `CmdStan not found` | Run `cmdstanr::install_cmdstan()` in R |
| EpiNow2 fails with < 60 days of data | The condition doesn't have enough history; it's auto-skipped |
| Forecast `.rds` files not found | Run `Rscript R/run_forecasts.r` first |
| Animation GIF not rendering | Install `gifski`: `install.packages("gifski")` |
| Aggregated data path wrong | Check `data_dir` in `by-forecasts/index.qmd` setup chunk |

---

## Scheduled automation (optional)

For production, consider a cron job or GitHub Actions workflow:

```bash
# crontab -e  (runs every Monday at 06:00)
0 6 * * 1 cd /path/to/NMC_dashboard && Rscript R/prepare_dashboard_data.r && Rscript R/run_forecasts.r && quarto render
```

Or via GitHub Actions: trigger on push to `main`, run the three commands, and deploy to GitHub Pages.
