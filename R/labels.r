# ──────────────────────────────────────────────────────────────────────────────
# labels.r
# Single source of truth for province / epiweek / formatting helpers used
# across multiple .qmd pages. Replaces duplicated `prov_labels` definitions in
# by-condition/, by-province/, explorer/.
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

# ── Province code ↔ name lookup ──────────────────────────────────────────────
# Mirrors `province_names` in R/plot_themes.r but is the canonical lookup used
# by the .qmd pages (kept here so plot_themes.r stays focused on plotting).
prov_labels <- c(
  EC  = "Eastern Cape",  FS = "Free State",    GP  = "Gauteng",
  KZN = "KwaZulu-Natal", LP = "Limpopo",       MP  = "Mpumalanga",
  NC  = "Northern Cape", NW = "North West",    WC  = "Western Cape"
)

#' Translate a vector of province codes to full names.
#' Unknown codes are returned unchanged.
prov_label <- function(codes) {
  out <- prov_labels[as.character(codes)]
  out[is.na(out)] <- as.character(codes)[is.na(out)]
  unname(out)
}

# ── Epiweek (MMWR / CDC convention: Sun-start) ───────────────────────────────
# NMC reporting in SA follows NICD which aligns with MMWR. We expose two
# helpers; pages should use these instead of `floor_date(date, "week")`.

#' MMWR epiweek start date (Sunday-anchored).
mmwr_week_start <- function(d) {
  d <- as.Date(d)
  # lubridate::floor_date with week_start = 7 = Sunday
  floor_date(d, unit = "week", week_start = 7)
}

#' MMWR epiweek number (1–53).
mmwr_week <- function(d) {
  lubridate::epiweek(as.Date(d))
}

#' MMWR epiyear (the year the week belongs to).
mmwr_year <- function(d) {
  lubridate::epiyear(as.Date(d))
}
