# ──────────────────────────────────────────────────────────────────────────────
# incidence_functions.r
# Incidence rate calculations for the NMC dashboard
#
# Adapted from: NMC_reports_scripts/scripts_and_functions/incidence_function.R
#               NMC_reports_scripts/Age_Standardise_Rates.r
#
# Works with aggregated data from prepare_dashboard_data.r and NMCleaner::pop
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(dplyr)
})

source(file.path(here::here(), "R/plot_themes.r"))

# ── 1. Province-level incidence ──────────────────────────────────────────────
#
#' Calculate incidence per 100 000 by province.
#'
#' @param agg_dt        data.table with columns: condition, prov_, count (aggregated)
#' @param pop_dt        data.table with prov_ and pop (total population).
#'                      If NULL, uses a built-in 2024 MYPE estimate.
#' @param per           denominator for rate (default 100 000)
#' @return  data.table with: condition, prov_, cases, pop, incidence

calc_incidence_province <- function(agg_dt,
                                    pop_dt = NULL,
                                    per    = 1e5) {
  agg_dt <- as.data.table(copy(agg_dt))

  if (is.null(pop_dt)) {
    pop_dt <- data.table(
      prov_ = c("EC", "FS", "GP", "KZN", "LP", "MP", "NC", "NW", "WC"),
      pop   = c(6734001, 2928903, 16098571, 11538813, 5941439, 4743584,
                1303047, 4141774, 7187004)
    )
  }

  cases <- agg_dt[, .(cases = sum(count, na.rm = TRUE)), by = .(condition, prov_)]
  out   <- merge(cases, pop_dt, by = "prov_", all.x = TRUE)
  out[, incidence := round(cases / pop * per, 2)]
  setorder(out, condition, -incidence)
  return(out)
}


# ── 2. Age-stratified incidence ──────────────────────────────────────────────
#
#' @param case_dt  data.table with condition, agecategory (or Age), count
#' @param pop_dt   data.table with Age (matching levels) and pop
#' @param per      denominator (default 100 000)

age_levels_default <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
  "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
  "60-64", "65+"
)

calc_incidence_age <- function(case_dt,
                               pop_dt,
                               age_col = "agecategory",
                               per     = 1e5) {
  case_dt <- as.data.table(copy(case_dt))
  setnames(case_dt, age_col, "Age", skip_absent = TRUE)

  cases <- case_dt[, .(cases = sum(count, na.rm = TRUE)), by = .(condition, Age)]
  out   <- merge(cases, pop_dt, by = "Age", all.x = TRUE)
  out[, incidence := round(cases / pop * per, 2)]
  out[, Age := factor(Age, levels = age_levels_default)]
  setorder(out, condition, Age)
  return(out)
}


# ── 3. Incidence bar chart (province) ────────────────────────────────────────
#
#' Stacked bar chart of incidence by province, coloured by condition.
#' Adapted from legacy incidence_calc() plotting code.

plot_incidence_province <- function(inci_dt,
                                    top_n = 12,
                                    title = "Notification incidence per 100 000") {

  inci_dt <- as.data.table(copy(inci_dt))

  # Rank conditions by total incidence
  totals <- inci_dt[, .(total = sum(incidence, na.rm = TRUE)), by = condition]
  setorder(totals, total)
  top_conds <- tail(totals$condition, top_n)
  inci_dt <- inci_dt[condition %in% top_conds]
  inci_dt[, condition := factor(condition, levels = totals[condition %in% top_conds, condition])]

  # Order provinces by total incidence
  prov_order <- inci_dt[, .(total = sum(incidence, na.rm = TRUE)), by = prov_]
  setorder(prov_order, -total)
  inci_dt[, prov_ := factor(prov_, levels = prov_order$prov_)]

  ggplot(inci_dt, aes(x = prov_, y = incidence, fill = condition)) +
    geom_col(colour = "white", linewidth = 0.2) +
    scale_fill_nmc(name = "Condition") +
    labs(title = title, x = "Province", y = "Per 100 000") +
    theme_nmc() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}


# ── 4. Incidence bar chart (age) ─────────────────────────────────────────────

plot_incidence_age <- function(inci_dt,
                               top_n = 12,
                               title = "Notification incidence by age group") {

  inci_dt <- as.data.table(copy(inci_dt))
  inci_dt[, Age := factor(Age, levels = age_levels_default)]

  totals <- inci_dt[, .(total = sum(incidence, na.rm = TRUE)), by = condition]
  setorder(totals, total)
  top_conds <- tail(totals$condition, top_n)
  inci_dt <- inci_dt[condition %in% top_conds]
  inci_dt[, condition := factor(condition, levels = totals[condition %in% top_conds, condition])]

  ggplot(inci_dt, aes(x = Age, y = incidence, fill = condition)) +
    geom_col(colour = "white", linewidth = 0.2) +
    scale_fill_nmc(name = "Condition") +
    labs(title = title, x = "Age group", y = "Per 100 000") +
    theme_nmc() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}


# ── 5. Age-standardised rate (direct method) ─────────────────────────────────
#
#' Direct age standardisation using WHO World Standard Population.
#' Adapted from NMC_reports_scripts/Age_Standardise_Rates.r
#'
#' @param inci_dt   data.table with Age, cases, pop (age-specific counts & pop)
#' @param std_pop   named numeric vector of standard population weights by Age.
#'                  Default = WHO World Standard (Segi, modified by Ahmad & Boschi-Pinto)
#' @return  numeric: age-standardised rate per 100 000

who_standard_pop <- c(
  "0-4"   = 8860, "5-9"   = 8690, "10-14" = 8600, "15-19" = 8470,
  "20-24" = 8220, "25-29" = 7930, "30-34" = 7610, "35-39" = 7150,
  "40-44" = 6590, "45-49" = 6040, "50-54" = 5370, "55-59" = 4550,
  "60-64" = 3720, "65+"   = 6510
)

calc_asr <- function(inci_dt, std_pop = who_standard_pop, per = 1e5) {
  inci_dt <- as.data.table(copy(inci_dt))

  # Ensure matching age levels
  inci_dt <- inci_dt[Age %in% names(std_pop)]
  inci_dt[, age_rate := cases / pop]
  inci_dt[, weight   := std_pop[as.character(Age)]]

  asr <- inci_dt[, sum(age_rate * weight, na.rm = TRUE) / sum(weight, na.rm = TRUE)] * per
  return(round(asr, 2))
}
