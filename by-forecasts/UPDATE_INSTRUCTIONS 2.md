# Predictive Modelling — Update & Maintenance Guide

This document describes how to run, refresh, extend, and **adjust model parameters** for the **Forecasts** section of the NMC Dashboard.

---

## Architecture overview

```
R/
├── epinow2_functions.r        ← Disease params + EpiNow2 wrappers (★ edit here for assumptions)
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
├── forecasts_<condition>.rds  ← Single-point EpiNow2 summaries (Rt, cases, infections)
├── forecasts_obj_<condition>.rds  ← Full EpiNow2 result objects (for native plot())
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
| **patchwork** | `install.packages("patchwork")` — for EpiNow2 native plot layout |
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

`R/run_forecasts.r` reads `agg_national.rds`, filters to forecast-eligible
conditions, and runs EpiNow2 for each. It produces:

- **Summary forecasts** – `forecasts_<condition>.rds` with Rt, cases, infections
- **Full result objects** – `forecasts_obj_<condition>.rds` for native EpiNow2 `plot()`
- **Rolling forecasts** – weekly snapshots per condition (for animation)
- **Metadata** – run timestamp, horizon, conditions list

**Tuning parameters** (edit at the top of `run_forecasts.r`):

| Variable | Default | Effect |
|---|---|---|
| `HORIZON` | 28 | Days forecasted ahead (4 weeks) |
| `LOOKBACK` | 360 | Days of surveillance history used |
| `N_REWINDS` | 8 | Number of rolling snapshots |
| `REWIND_STEP` | 7 | Days between rolling snapshots |

### 3. Render

The QMD page reads cached `.rds` files — it does **not** run EpiNow2 live.
This keeps rendering fast (~30 s) while the model runs stay separate.

---

## Adjusting Disease Parameters (Model Assumptions)

All disease-specific assumptions are in **one place**:
`R/epinow2_functions.r` → the `disease_params()` function.

Each condition returns a named list with:

| Field | Type | Description |
|---|---|---|
| `generation_time` | `EpiNow2::Gamma(...)` | Serial interval / generation time distribution |
| `incubation_period` | `EpiNow2::Gamma(...)` | Time from exposure to symptom onset |
| `rt_prior` | `list(mean, sd)` | Prior for the reproduction number |
| `reference` | `character` | Literature citation for the values |

### How to change an existing condition's parameters

1. Open `R/epinow2_functions.r` in any text editor
2. Search for the condition name (lowercase), e.g. `"cholera"`
3. Modify `mean`, `sd`, or `max` inside the `EpiNow2::Gamma()` call
4. Update the `reference` string to cite your new source
5. Save the file
6. Re-run: `Rscript R/run_forecasts.r`
7. Re-render: `quarto render by-forecasts/index.qmd`

**Example** – updating cholera generation time from a new study:

```r
# BEFORE:
"cholera" = list(
  generation_time   = EpiNow2::Gamma(mean = 5.0, sd = 2.0, max = 14),
  incubation_period = EpiNow2::Gamma(mean = 1.4, sd = 0.6, max = 5),
  rt_prior          = list(mean = 2, sd = 1),
  reference         = "Azman et al. (2013) J Infect Dis 207(11):1698-1706"
),

# AFTER:
"cholera" = list(
  generation_time   = EpiNow2::Gamma(mean = 4.5, sd = 1.8, max = 12),
  #                                   ↑ updated based on new study
  incubation_period = EpiNow2::Gamma(mean = 1.4, sd = 0.6, max = 5),
  rt_prior          = list(mean = 2, sd = 1),
  reference         = "New Author (2026) Journal Name DOI:10.xxxx/xxxxx"
),
```

### Understanding the Gamma distribution parameters

EpiNow2 uses `EpiNow2::Gamma()` to define delay distributions. The mean/sd parameterisation:

- `mean`: average duration in **days**
- `sd`: standard deviation in **days** (captures uncertainty / variability)
- `max`: hard upper truncation in **days** (prevents extreme tail values)

**Where to find values in the literature:**

- Check systematic reviews for "serial interval" or "generation time"
- Look for "incubation period" meta-analyses
- Common sources: Lessler et al. (2009), Vink et al. (2014), WHO disease fact sheets
- For newer diseases: real-time estimates from outbreak reports (e.g., Eurosurveillance, MMWR)

### Understanding the Rt prior

```r
rt_prior = list(mean = 2, sd = 1)   # weakly informative: allows Rt from ~0 to ~4
rt_prior = list(mean = 1.5, sd = 0.5) # tighter: for endemic / vector-borne diseases
```

The prior is `Normal(mean, sd)` and is **weakly informative** — the model quickly
overrides it with data. Use `mean=2, sd=1` for directly-transmitted pathogens with
high R0 (measles, pertussis). Use `mean=1.5, sd=0.5` for vector-borne (malaria)
or lower-R0 conditions (listeriosis).

### Understanding the reporting delay

A default `LogNormal(mean=3, sd=2, max=15)` is used unless empirical delay data
is available. This represents the typical NMC notification delay from symptom onset.
To change it globally, edit `run_epinow_forecast()` in `R/epinow2_functions.r`.

---

## Adding a new condition

1. **Data**: ensure the condition name appears in the `condition` column of `agg_national.rds`.
   If it's a new NMC category, update `prepare_dashboard_data.r` to include it.

2. **Disease parameters**: in `R/epinow2_functions.r`, add a new entry to the
   `disease_params()` switch statement:

   ```r
   "my new condition" = list(
     generation_time   = EpiNow2::Gamma(mean = ..., sd = ..., max = ...),
     incubation_period = EpiNow2::Gamma(mean = ..., sd = ..., max = ...),
     rt_prior          = list(mean = ..., sd = ...),
     reference         = "Author (Year) Journal Vol:Pages"
   ),
   ```

   Place the entry **before** the `# Default / fallback` comment.

3. **Condition list**: add the name to `forecast_conditions()` in the same file.

4. **Colour**: add a colour entry in `by-forecasts/index.qmd` → `condition_colours` vector.

5. **Re-run** `Rscript R/run_forecasts.r` then `quarto render by-forecasts/index.qmd`.

---

## Current parameter summary

| Condition | Generation Time (mean d) | Incubation (mean d) | Rt Prior | Primary Source |
|---|---|---|---|---|
| Measles / Fever-Rash | 11.7 | 12.5 | N(2,1) | Lessler et al. (2009) Lancet Infect Dis |
| Rubella | 18.3 | 16.5 | N(2,1) | Vink et al. (2014) Am J Epidemiol; Lessler et al. (2009) |
| Pertussis | 22.8 | 9.4 | N(2,1) | Vink et al. (2014); Wearing & Rohani (2009) |
| Diphtheria | 8.0 | 3.3 | N(2,1) | Truelove et al. (2020) Emerg Infect Dis; AAP Red Book |
| Cholera | 5.0 | 1.4 | N(2,1) | Azman et al. (2013) J Infect Dis |
| Malaria | 30.0 | 12.0 | N(1.5,0.5) | Huber et al. (2016) Malar J; WHO Malaria Report |
| COVID-19 (Omicron) | 3.3 | 3.4 | N(1.5,0.5) | Park et al. (2023) BMC Infect Dis; Wu et al. (2022) JAMA Netw Open |
| Enteric fever | 14.0 | 10.2 | N(1.5,0.5) | Pitzer et al. (2014) PLoS NTD; Crump et al. (2015) |
| Meningococcal | 7.0 | 3.7 | N(1.5,0.5) | Trotter et al. (2005) Emerg Infect Dis; Rosenstein et al. (2001) NEJM |
| Listeriosis | 21.0 | 21.0 | N(1.2,0.5) | Goulet et al. (2013) Clin Infect Dis |
| Hepatitis A | 24.0 | 28.5 | N(1.5,0.5) | Jacobsen & Wiersma (2010) Clin Liver Dis; Lemon et al. (2018) Lancet |
| Mpox | 12.5 | 8.5 | N(1.5,0.5) | Miura et al. (2022) Euro Surveill; Thornhill et al. (2022) NEJM |

---

## GitHub contribution workflow

If you're updating parameters via a pull request on GitHub:

### 1. Create a branch

```bash
git checkout -b update/cholera-params
```

### 2. Edit the parameters

Open `R/epinow2_functions.r` and modify the relevant entry in `disease_params()`.
Always update the `reference` field with the new citation.

### 3. Validate locally (recommended)

```bash
# Run a quick test for just the changed condition
Rscript -e '
  source("R/epinow2_functions.r")
  p <- disease_params("cholera")
  cat("Generation time mean:", p$generation_time$parameters$mean, "\n")
  cat("Incubation mean:", p$incubation_period$parameters$mean, "\n")
  cat("Reference:", p$reference, "\n")
'
```

### 4. Commit with a descriptive message

```bash
git add R/epinow2_functions.r
git commit -m "Update cholera generation time to 4.5d (New Author 2026)

- Source: New Author (2026) Journal Name DOI:10.xxxx
- Changed generation_time mean from 5.0 to 4.5
- Updated reference string"
```

### 5. Push and create a PR

```bash
git push origin update/cholera-params
# Then create a pull request on GitHub
```

### PR checklist

- [ ] Parameter values sourced from peer-reviewed literature
- [ ] `reference` field updated with citation
- [ ] No R syntax errors (test with `Rscript -e 'source("R/epinow2_functions.r")'`)
- [ ] Optionally: re-ran `Rscript R/run_forecasts.r` and confirmed output

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
| "object 'forecasts_obj_...' not found" | Re-run `R/run_forecasts.r` — older runs may not have saved full objects |

---

## Scheduled automation (optional)

For production, consider a cron job or GitHub Actions workflow:

```bash
# crontab -e  (runs every Monday at 06:00)
0 6 * * 1 cd /path/to/NMC_dashboard && Rscript R/prepare_dashboard_data.r && Rscript R/run_forecasts.r && quarto render
```

Or via GitHub Actions: trigger on push to `main`, run the three commands, and deploy to GitHub Pages.
