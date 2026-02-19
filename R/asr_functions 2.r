# ──────────────────────────────────────────────────────────────────────────────
# asr_functions.r
# Full age-standardised rate (ASR) pipeline for the NMC dashboard
#
# Adapted from: NMC_reports_scripts/Age_Standardise_Rates.r
#
# Features:
#   - WHO World Standard Population (Ahmad et al. 2001) with 0-1/1-4 split
#   - Direct age-standardisation
#   - Poisson exact 95% CIs (via epitools)
#   - Sex-stratified rates with incidence rate ratio (IRR)
#   - Publication-quality flextable + ggplot output
#   - End-to-end pipeline: case-level data → table + plot
#
# Requires: data.table, ggplot2, epitools, flextable, gtsummary, dplyr
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(dplyr)
})

source(file.path(here::here(), "R/plot_themes.r"))

# ══════════════════════════════════════════════════════════════════════════════
# A. CONSTANTS
# ══════════════════════════════════════════════════════════════════════════════

#' 15 age bands: splits WHO 0-4 into 0-1 / 1-4, collapses 65-69…85+ into 65+
asr_age_levels <- c(
  "0-1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29",
  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
  "60-64", "65+"
)

#' WHO World Standard Population weights (Ahmad et al. 2001, Segi modified)
#' with 0-1/1-4 split (1/5 : 4/5 of original 0-4 weight) and 65+ collapsed.
.build_who_std <- function(split_0_1 = 1 / 5) {
  # Original 18-band WHO weights
  who_18 <- data.table(
    Age    = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
               "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
               "60-64", "65-69", "70-74", "75-79", "80-84", "85+"),
    weight = c(0.0886, 0.0869, 0.0860, 0.0847, 0.0822, 0.0793,
               0.0761, 0.0715, 0.0659, 0.0604, 0.0537, 0.0455,
               0.0372, 0.0296, 0.0221, 0.0152, 0.0091, 0.0063)
  )

  w04  <- who_18[Age == "0-4", weight]
  w65p <- who_18[Age %in% c("65-69", "70-74", "75-79", "80-84", "85+"),
                 sum(weight)]

  # Remove original 0-4 and the 65+ detail bands
  out <- who_18[!Age %in% c("0-4", "65-69", "70-74", "75-79", "80-84", "85+")]

  # Add split 0-1 / 1-4 and collapsed 65+
  out <- rbindlist(list(
    data.table(Age = "0-1", weight = w04 * split_0_1),
    data.table(Age = "1-4", weight = w04 * (1 - split_0_1)),
    out,
    data.table(Age = "65+", weight = w65p)
  ))

  out[, Age := factor(Age, levels = asr_age_levels)]
  setorder(out, Age)
  out[, weight_pct := round(weight * 100, 2)]
  return(out)
}

who_std_weights <- .build_who_std()

#' Province populations (2024 MYPE, Stats SA)
sa_province_pops_2024 <- c(
  EC = 6734001, FS = 2928903, GP = 16098571, KZN = 11538813,
  LP = 5941439, MP = 4743584, NC = 1303047,  NW  = 4141774,
  WC = 7187004
)


# ══════════════════════════════════════════════════════════════════════════════
# B. HELPERS
# ══════════════════════════════════════════════════════════════════════════════

#' Map continuous age (years) to the 15-band ASR factor
#'
#' @param age_years numeric vector
#' @param levels    character vector of factor levels (default asr_age_levels)
#' @return factor vector
assign_age_band <- function(age_years, levels = asr_age_levels) {
  band <- fcase(
    age_years < 1,              "0-1",
    age_years >= 1  & age_years < 5,  "1-4",
    age_years >= 5  & age_years < 10, "5-9",
    age_years >= 10 & age_years < 15, "10-14",
    age_years >= 15 & age_years < 20, "15-19",
    age_years >= 20 & age_years < 25, "20-24",
    age_years >= 25 & age_years < 30, "25-29",
    age_years >= 30 & age_years < 35, "30-34",
    age_years >= 35 & age_years < 40, "35-39",
    age_years >= 40 & age_years < 45, "40-44",
    age_years >= 45 & age_years < 50, "45-49",
    age_years >= 50 & age_years < 55, "50-54",
    age_years >= 55 & age_years < 60, "55-59",
    age_years >= 60 & age_years < 65, "60-64",
    age_years >= 65,                   "65+",
    default = NA_character_
  )
  factor(band, levels = levels)
}


#' Detect age column in a dataset (tries common variants)
.find_age_col <- function(dt) {
  candidates <- c("age_years", "Age_years", "age_tested_years",
                   "patient_age_years", "age")
  for (col in candidates) {
    if (col %in% names(dt)) return(col)
  }
  NULL
}

#' Detect sex column in a dataset (tries common variants)
.find_sex_col <- function(dt) {
  candidates <- c("Sex", "sex", "gender", "patient_gender")
  for (col in candidates) {
    if (col %in% names(dt)) return(col)
  }
  NULL
}


# ══════════════════════════════════════════════════════════════════════════════
# C. POPULATION BUILDER
# ══════════════════════════════════════════════════════════════════════════════

#' Build population denominator table (Age × Sex × Year) from NMCleaner pop data.
#'
#' Splits the 0-4 band into 0-1 / 1-4 using a fixed proportion (default 1:4)
#' and collapses 65-69 … 85+ into 65+.
#'
#' @param pop_dt      data.table from NMCleaner::pop (cols: Age_group, Sex, Year, Population)
#'                    If NULL, attempts to load NMCleaner::pop
#' @param years       integer vector of years to include
#' @param split_0_1   proportion of 0-4 population assigned to 0-1 (default 1/5)
#' @return data.table: Age, Sex, Year, pop, weight, weight_pct

build_population_by_age_sex <- function(pop_dt    = NULL,
                                        years     = 2020:2024,
                                        split_0_1 = 1 / 5) {

  if (is.null(pop_dt)) {
    if (requireNamespace("NMCleaner", quietly = TRUE)) {
      pop_dt <- as.data.table(NMCleaner::pop)
    } else {
      stop("Provide pop_dt or install NMCleaner: devtools::install_github('bridaybrummer/NMCleaner')")
    }
  } else {
    pop_dt <- as.data.table(copy(pop_dt))
  }

  # Standardise column names
  if ("Age_group" %in% names(pop_dt)) setnames(pop_dt, "Age_group", "Age")
  if ("Year" %in% names(pop_dt)) pop_dt[, Year := as.integer(Year)]
  pop_dt <- pop_dt[Year %in% years]

  # Map existing age bands to our 15-level scheme
  # Source data typically uses 0-4, 5-9, …, 80-84, 85+
  # Step 1: Split 0-4 into 0-1 and 1-4
  dt_04 <- pop_dt[Age == "0-4"]
  if (nrow(dt_04) > 0) {
    dt_01 <- copy(dt_04)[, `:=`(Age = "0-1", Population = Population * split_0_1)]
    dt_14 <- copy(dt_04)[, `:=`(Age = "1-4", Population = Population * (1 - split_0_1))]
    pop_dt <- rbindlist(list(pop_dt[Age != "0-4"], dt_01, dt_14), use.names = TRUE)
  }

  # Step 2: Collapse 65+ bands
  bands_65plus <- c("65-69", "70-74", "75-79", "80-84", "85+",
                    "65+")  # in case already collapsed
  dt_65 <- pop_dt[Age %in% bands_65plus]
  if (nrow(dt_65) > 0) {
    dt_65_agg <- dt_65[, .(Population = sum(Population, na.rm = TRUE)),
                       by = .(Sex, Year)]
    dt_65_agg[, Age := "65+"]
    pop_dt <- rbindlist(list(pop_dt[!Age %in% bands_65plus], dt_65_agg),
                        use.names = TRUE, fill = TRUE)
  }

  # Filter to our 15 age bands
  pop_dt <- pop_dt[Age %in% asr_age_levels]
  pop_dt[, Age := factor(Age, levels = asr_age_levels)]

  # Rename population column
  setnames(pop_dt, "Population", "pop", skip_absent = TRUE)

  # Join weights
  pop_dt <- merge(pop_dt, who_std_weights[, .(Age, weight, weight_pct)],
                  by = "Age", all.x = TRUE)

  setorder(pop_dt, Year, Sex, Age)
  return(pop_dt)
}


# ══════════════════════════════════════════════════════════════════════════════
# D. ASR CALCULATION
# ══════════════════════════════════════════════════════════════════════════════

#' Calculate age-specific rates + ASR contributions for arbitrary groupings.
#'
#' Uses direct method: ASR = Σ (weight_i × rate_i) where rate_i = cases_i / pop_i.
#' Attaches Poisson exact 95% CIs per stratum.
#'
#' @param case_dt     data.table with `Age`, group columns, and `cases`
#' @param pop_dt      data.table with `Age`, group columns, `pop`, and `weight`
#' @param group_cols  character vector of grouping columns (e.g. "Sex", "Year")
#' @param per         rate denominator (default 1e5)
#' @return data.table with rates and CIs

calc_asr_by_group <- function(case_dt,
                              pop_dt,
                              group_cols = "Sex",
                              per        = 1e5) {

  case_dt <- as.data.table(copy(case_dt))
  pop_dt  <- as.data.table(copy(pop_dt))

  # Ensure Age is factor
  case_dt[, Age := factor(Age, levels = asr_age_levels)]
  pop_dt[, Age := factor(Age, levels = asr_age_levels)]

  # Merge cases and population
  merge_cols <- c("Age", intersect(group_cols, names(case_dt)))
  dt <- merge(case_dt, pop_dt, by = intersect(names(case_dt), names(pop_dt)),
              all.x = TRUE)

  # Calculate age-specific rate
  dt[, rate_per_100k := fifelse(pop > 0, (cases / pop) * per, NA_real_)]

  # ASR contribution: weight × rate
  dt[, asr_contribution := weight * rate_per_100k]

  # Poisson exact CIs (requires epitools)
  if (requireNamespace("epitools", quietly = TRUE)) {
    dt[, c("ci_lower", "ci_upper") := {
      ci <- mapply(function(x, pt) {
        if (is.na(x) || is.na(pt) || pt <= 0) return(c(NA_real_, NA_real_))
        res <- epitools::pois.exact(x, pt = pt, conf.level = 0.95)
        c(res$lower * per, res$upper * per)
      }, cases, pop, SIMPLIFY = TRUE)
      list(ci[1, ], ci[2, ])
    }]
  } else {
    # Fallback: Wald approximation
    dt[, `:=`(
      ci_lower = rate_per_100k - 1.96 * sqrt(cases) / pop * per,
      ci_upper = rate_per_100k + 1.96 * sqrt(cases) / pop * per
    )]
    message("epitools not installed; using Wald approximation for CIs.")
  }

  # Weighted CI contribution (for ASR CI propagation)
  dt[, ci_lower_weighted := weight * ci_lower]
  dt[, ci_upper_weighted := weight * ci_upper]

  setorder(dt, Age)
  return(dt)
}


#' Collapse age-specific rows into a single ASR per group.
#'
#' @param asr_dt      data.table from calc_asr_by_group()
#' @param group_cols  character vector of grouping columns
#' @return data.table: group columns, asr, asr_lower, asr_upper, total_cases, total_pop

asr_summary <- function(asr_dt, group_cols = "Sex") {
  asr_dt <- as.data.table(copy(asr_dt))

  asr_dt[, .(
    asr         = sum(asr_contribution, na.rm = TRUE),
    asr_lower   = sum(ci_lower_weighted, na.rm = TRUE),
    asr_upper   = sum(ci_upper_weighted, na.rm = TRUE),
    total_cases = sum(cases, na.rm = TRUE),
    total_pop   = sum(pop, na.rm = TRUE)
  ), by = group_cols]
}


# ══════════════════════════════════════════════════════════════════════════════
# E. SEX-STRATIFIED IRR
# ══════════════════════════════════════════════════════════════════════════════

#' Compute incidence rate ratios (Female vs Male) per age band.
#'
#' Uses two-sample Poisson exact test.
#'
#' @param asr_dt data.table from calc_asr_by_group() containing both "Female" and "Male"
#' @return wide data.table with IRR, CIs, and p-value per age band

calc_sex_irr <- function(asr_dt) {
  asr_dt <- as.data.table(copy(asr_dt))

  # Pivot wide
  wide <- dcast(
    asr_dt,
    Age + weight + weight_pct ~ Sex,
    value.var = c("cases", "pop", "rate_per_100k", "asr_contribution",
                  "ci_lower", "ci_upper"),
    fill = 0
  )

  # Poisson exact test per age band
  wide[, `:=`(
    irr       = NA_real_,
    irr_lower = NA_real_,
    irr_upper = NA_real_,
    p_value   = NA_character_
  )]

  for (i in seq_len(nrow(wide))) {
    cf <- wide$cases_Female[i]
    cm <- wide$cases_Male[i]
    pf <- wide$pop_Female[i]
    pm <- wide$pop_Male[i]

    if (!is.na(cf) && !is.na(cm) && cf >= 0 && cm >= 0 && pf > 0 && pm > 0) {
      pt <- tryCatch(
        stats::poisson.test(
          x = c(as.integer(round(cf)), as.integer(round(cm))),
          T = c(pf, pm),
          alternative = "two.sided",
          conf.level = 0.95
        ),
        error = function(e) NULL
      )
      if (!is.null(pt)) {
        set(wide, i, "irr",       unname(pt$estimate))
        set(wide, i, "irr_lower", pt$conf.int[1])
        set(wide, i, "irr_upper", pt$conf.int[2])
        set(wide, i, "p_value",   format_pvalue(pt$p.value))
      }
    }
  }

  setorder(wide, Age)
  return(wide)
}

#' Format p-value for display
#' @param p numeric p-value
#' @return character
format_pvalue <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (requireNamespace("gtsummary", quietly = TRUE)) {
    return(gtsummary::style_pvalue(p))
  }
  if (p < 0.001) return("<0.001")
  if (p < 0.01)  return(formatC(p, format = "f", digits = 3))
  if (p < 0.05)  return(formatC(p, format = "f", digits = 3))
  return(formatC(p, format = "f", digits = 2))
}

#' Format confidence interval as "lower - upper"
fmt_ci <- function(lo, hi, digits = 2) {
  ifelse(is.na(lo) | is.na(hi), NA_character_,
         paste0(formatC(lo, format = "f", digits = digits),
                " - ",
                formatC(hi, format = "f", digits = digits)))
}


# ══════════════════════════════════════════════════════════════════════════════
# F. PUBLICATION-QUALITY TABLE
# ══════════════════════════════════════════════════════════════════════════════

#' Create a flextable of age-specific + age-standardised rates by sex.
#'
#' Two-row header: top level = Gender, bottom = Measure.
#' Includes WHO weight column, population, cases, age-specific rate,
#' age-standardised rate, 95% CI, p-value.
#'
#' @param irr_dt           wide data.table from calc_sex_irr()
#' @param condition_label  string for table caption
#' @param years_label      string for footnote (e.g. "2020-2024")
#' @param avg_years        integer: number of years averaged (for dividing pop/cases)
#' @return flextable object

asr_table <- function(irr_dt,
                      condition_label = "",
                      years_label     = "2020-2024",
                      avg_years       = 5L) {

  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop("Install flextable: install.packages('flextable')")
  }

  dt <- as.data.table(copy(irr_dt))

  # Average population and cases over years
  pop_case_cols <- grep("^(pop_|cases_)", names(dt), value = TRUE)
  for (col in pop_case_cols) {
    set(dt, j = col, value = round(dt[[col]] / avg_years, 0))
  }

  # Format rates
  rate_cols <- grep("rate_per_100k|asr_contribution", names(dt), value = TRUE)
  for (col in rate_cols) {
    set(dt, j = col, value = round(dt[[col]], 2))
  }

  # CI strings
  dt[, ci_Female := fmt_ci(ci_lower_Female, ci_upper_Female)]
  dt[, ci_Male   := fmt_ci(ci_lower_Male, ci_upper_Male)]

  # Weight as percentage
  dt[, weight_pct := round(weight * 100, 2)]

  # Build the total row
  total_row <- dt[, .(
    Age = factor("Total", levels = c(asr_age_levels, "Total")),
    weight_pct      = 100,
    pop_Female      = sum(pop_Female, na.rm = TRUE),
    cases_Female    = sum(cases_Female, na.rm = TRUE),
    rate_per_100k_Female = round(sum(cases_Female, na.rm = TRUE) /
                                   sum(pop_Female, na.rm = TRUE) * 1e5, 2),
    asr_contribution_Female = round(sum(asr_contribution_Female, na.rm = TRUE), 2),
    ci_Female       = NA_character_,
    pop_Male        = sum(pop_Male, na.rm = TRUE),
    cases_Male      = sum(cases_Male, na.rm = TRUE),
    rate_per_100k_Male = round(sum(cases_Male, na.rm = TRUE) /
                                 sum(pop_Male, na.rm = TRUE) * 1e5, 2),
    asr_contribution_Male = round(sum(asr_contribution_Male, na.rm = TRUE), 2),
    ci_Male         = NA_character_,
    p_value         = NA_character_
  )]

  dt[, Age := factor(Age, levels = c(asr_age_levels, "Total"))]

  # Select and order columns
  display_cols <- c(
    "Age", "weight_pct",
    "pop_Female", "cases_Female", "rate_per_100k_Female",
    "asr_contribution_Female", "ci_Female",
    "pop_Male", "cases_Male", "rate_per_100k_Male",
    "asr_contribution_Male", "ci_Male",
    "p_value"
  )

  # Ensure all columns exist in both dt and total_row
  for (col in display_cols) {
    if (!col %in% names(dt)) dt[, (col) := NA]
    if (!col %in% names(total_row)) total_row[, (col) := NA]
  }

  tbl <- rbindlist(list(
    dt[, ..display_cols],
    total_row[, ..display_cols]
  ), use.names = TRUE, fill = TRUE)

  setorder(tbl, Age)

  ft <- flextable::flextable(tbl)

  # Column labels (2-level headers built manually)
  ft <- flextable::set_header_labels(ft, values = list(
    Age                       = "Age",
    weight_pct                = "WHO\nWeight (%)",
    pop_Female                = "Avg\nPopulation",
    cases_Female              = "Avg\nCases",
    rate_per_100k_Female      = "Age Specific\nRate",
    asr_contribution_Female   = "ASR\nContribution",
    ci_Female                 = "95% CI",
    pop_Male                  = "Avg\nPopulation",
    cases_Male                = "Avg\nCases",
    rate_per_100k_Male        = "Age Specific\nRate",
    asr_contribution_Male     = "ASR\nContribution",
    ci_Male                   = "95% CI",
    p_value                   = "p-value"
  ))

  # Add a top header row for Female / Male
  ft <- flextable::add_header_row(ft, values = c(
    "", "", "Female", "Female", "Female", "Female", "Female",
    "Male", "Male", "Male", "Male", "Male", ""
  ), top = TRUE)

  ft <- flextable::merge_h(ft, part = "header")
  ft <- flextable::theme_vanilla(ft)
  ft <- flextable::autofit(ft)

  # Caption
  if (nchar(condition_label) > 0) {
    ft <- flextable::set_caption(ft, paste0(
      "Age-standardised rates by age and sex for ", condition_label,
      " (", years_label, "), South Africa"
    ))
  }

  # Footnotes
  ft <- flextable::footnote(ft,
    part = "header", i = 2, j = c(3, 8),
    value = flextable::as_paragraph(
      "Population estimates from Statistics SA MYPE. ",
      "Average populations and cases over the study period. ",
      "Rates per 100 000 person-years."
    ),
    ref_symbols = "*"
  )

  ft <- flextable::footnote(ft,
    part = "header", i = 2, j = c(6, 11),
    value = flextable::as_paragraph(
      "ASR = Age-Standardised Rate (direct method); ",
      "each age-specific rate multiplied by the WHO standard population weight."
    ),
    ref_symbols = "\u2020"
  )

  ft <- flextable::footnote(ft,
    part = "header", i = 2, j = 13,
    value = flextable::as_paragraph(
      "Two-sample Poisson exact test comparing female vs male ",
      "incidence within each age group (two-sided)."
    ),
    ref_symbols = "\u2021"
  )

  return(ft)
}


# ══════════════════════════════════════════════════════════════════════════════
# G. PUBLICATION-QUALITY PLOT
# ══════════════════════════════════════════════════════════════════════════════

#' Line + ribbon plot of age-specific rates by sex.
#'
#' Styled after SAMJ / NEJM format: direct end-labels, CI ribbon,
#' clean minimal theme.
#'
#' @param asr_dt           data.table from calc_asr_by_group()
#' @param condition_label  plot title
#' @param ci               show confidence ribbons (default TRUE)
#' @return ggplot object

asr_line_plot <- function(asr_dt,
                          condition_label = "",
                          ci              = TRUE) {

  dt <- as.data.table(copy(asr_dt))
  dt <- dt[Age %in% asr_age_levels]
  dt[, Age := factor(Age, levels = asr_age_levels)]

  # End labels for direct labelling (highest rate per sex)
  ends <- dt[, .SD[which.max(rate_per_100k)], by = Sex]

  # Colour palette (teal + purple, inspired by the legacy CDC palette)
  sex_colours <- c(Female = "#45c1c0", Male = "#500778")

  p <- ggplot(dt, aes(x = Age, y = rate_per_100k, group = Sex, colour = Sex))

  if (ci) {
    p <- p + geom_ribbon(
      aes(ymin = ci_lower, ymax = ci_upper, fill = Sex),
      alpha = 0.15, colour = NA, show.legend = FALSE
    ) +
    scale_fill_manual(values = sex_colours)
  }

  p <- p +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2, stroke = 0) +
    ggrepel::geom_text_repel(
      data = ends,
      aes(label = Sex),
      nudge_y = 0.003, hjust = -0.2, size = 4,
      fontface = "bold", show.legend = FALSE
    ) +
    scale_colour_manual(values = sex_colours) +
    scale_y_continuous(
      "Incidence per 100 000 person-years",
      labels = scales::label_comma(accuracy = 0.01),
      expand = expansion(mult = c(0.02, 0.08))
    ) +
    scale_x_discrete("Age group", expand = expansion(add = 0.2)) +
    coord_cartesian(clip = "off") +
    labs(title = condition_label) +
    theme_nmc() +
    theme(legend.position = "none")

  return(p)
}


# ══════════════════════════════════════════════════════════════════════════════
# H. END-TO-END PIPELINE
# ══════════════════════════════════════════════════════════════════════════════

#' Full ASR pipeline: case-level data → population build → ASR → table + plot.
#'
#' @param case_level_dt  data.table with raw case-level records.
#'                       Must contain age (years) and sex columns
#'                       (auto-detected from common variants).
#' @param pop_dt         population data.table (NULL → NMCleaner::pop)
#' @param years          integer vector of years to include
#' @param condition_label  text for table caption / plot title
#' @param split_0_1     proportion of 0-4 pop for 0-1 band
#' @return named list:
#'   $table      — flextable
#'   $plot       — ggplot
#'   $data       — full rate data.table
#'   $data_wide  — sex-wide with IRR
#'   $summary    — ASR summary per sex

asr_pipeline <- function(case_level_dt,
                         pop_dt          = NULL,
                         years           = 2020:2024,
                         condition_label = "",
                         split_0_1       = 1 / 5) {

  dt <- as.data.table(copy(case_level_dt))

  # ── 1. Find and standardise age column ──
  age_col <- .find_age_col(dt)
  if (is.null(age_col)) stop("No recognised age column found. Expected one of: age_years, Age_years, age_tested_years, patient_age_years, age")
  setnames(dt, age_col, "age_years", skip_absent = TRUE)
  dt[, age_years := as.numeric(age_years)]

  # ── 2. Find and standardise sex column ──
  sex_col <- .find_sex_col(dt)
  if (is.null(sex_col)) stop("No recognised sex column found. Expected one of: Sex, sex, gender, patient_gender")
  setnames(dt, sex_col, "Sex", skip_absent = TRUE)

  # Standardise sex values
  dt[, Sex := fcase(
    grepl("^[Ff]", Sex), "Female",
    grepl("^[Mm]", Sex), "Male",
    default = NA_character_
  )]
  dt <- dt[!is.na(Sex)]

  # ── 3. Assign age bands ──
  dt[, Age := assign_age_band(age_years)]
  dt <- dt[!is.na(Age)]

  # ── 4. Derive year if not present ──
  if (!"Year" %in% names(dt) && "year" %in% names(dt)) {
    setnames(dt, "year", "Year")
  }
  if (!"Year" %in% names(dt)) {
    # Try to derive from a date column
    date_col <- intersect(c("notification_date", "date", "symptom_date"), names(dt))[1]
    if (!is.na(date_col)) {
      dt[, Year := year(as.Date(get(date_col)))]
    } else {
      dt[, Year := years[1]]
      message("No date/year column found; assigning all cases to year ", years[1])
    }
  }

  # Filter to requested years
  dt <- dt[Year %in% years]

  # ── 5. Aggregate cases ──
  cases_by_age_sex <- dt[, .(cases = .N), by = .(Age, Sex)]

  # Also by year for the temporal dataset
  cases_by_age_sex_year <- dt[, .(cases = .N), by = .(Age, Sex, Year)]

  # ── 6. Build population ──
  pop <- build_population_by_age_sex(pop_dt = pop_dt, years = years,
                                     split_0_1 = split_0_1)

  # Average population across years for the main table
  pop_avg <- pop[, .(pop = sum(pop, na.rm = TRUE),
                     weight = mean(weight, na.rm = TRUE),
                     weight_pct = mean(weight_pct, na.rm = TRUE)),
                 by = .(Age, Sex)]

  # Average cases across years
  n_years <- length(unique(cases_by_age_sex_year$Year))
  # For the total table we use the summed pop (all years)

  # ── 7. Calculate ASR ──
  asr_dt <- calc_asr_by_group(
    case_dt    = cases_by_age_sex,
    pop_dt     = pop_avg,
    group_cols = "Sex"
  )

  # ── 8. Sex-wide with IRR ──
  asr_wide <- calc_sex_irr(asr_dt)

  # ── 9. Summary ──
  summ <- asr_summary(asr_dt, group_cols = "Sex")

  # ── 10. Table ──
  ft <- tryCatch(
    asr_table(asr_wide,
              condition_label = condition_label,
              years_label     = paste0(min(years), "-", max(years)),
              avg_years       = n_years),
    error = function(e) {
      message("Could not create flextable: ", e$message)
      NULL
    }
  )

  # ── 11. Plot ──
  pl <- tryCatch(
    asr_line_plot(asr_dt, condition_label = condition_label),
    error = function(e) {
      message("Could not create plot: ", e$message)
      NULL
    }
  )

  # ── 12. Year-stratified data ──
  asr_by_year <- tryCatch(
    calc_asr_by_group(
      case_dt    = cases_by_age_sex_year,
      pop_dt     = pop,
      group_cols = c("Sex", "Year")
    ),
    error = function(e) NULL
  )

  return(list(
    table        = ft,
    plot         = pl,
    data         = asr_dt,
    data_wide    = asr_wide,
    data_by_year = asr_by_year,
    summary      = summ
  ))
}
