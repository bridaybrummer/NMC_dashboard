# ──────────────────────────────────────────────────────────────────────────────
# map_functions.r
# Choropleth and hotspot map functions for the NMC dashboard
#
# Adapted from: NMC_reports_scripts/scripts_and_functions/map.R
#               NMC_reports_scripts/scripts_and_functions/incidence_functions_ForMapping.R
#               NMC_reports_scripts/scripts_and_functions/GGAnimate_FeverRash.r
#
# Uses sf + ggplot2 for static maps, plotly::ggplotly() for interactive.
# Shapefiles sourced from NMCleaner::shape_files.
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(sf)
  library(dplyr)
  library(plotly)
})

source(file.path(here::here(), "R/plot_themes.r"))

# ── Helpers ──────────────────────────────────────────────────────────────────

#' Get province-level shapefile (cached).
#' Merges district polygons into 9 provinces using NMCleaner::shape_files.
get_province_shape <- function() {
  if (!requireNamespace("NMCleaner", quietly = TRUE)) {
    stop("NMCleaner package required. Install with: devtools::install_github('bridaybrummer/NMCleaner')")
  }
  NMCleaner::shape_files$districts %>%
    group_by(province) %>%
    summarise(geometry = sf::st_union(geometry)) %>%
    ungroup()
}

#' Get district-level shapefile
get_district_shape <- function() {
  if (!requireNamespace("NMCleaner", quietly = TRUE)) {
    stop("NMCleaner package required.")
  }
  NMCleaner::shape_files$districts
}

#' Standardise district name for fuzzy join
#' Lowercases and strips "city", "metro", "of", "district" prefixes.
standardise_district <- function(x) {
  x <- tolower(x)
  x <- gsub("city|metro|of|district", "", x)
  trimws(x)
}


# ── 1. Province choropleth (case counts) ─────────────────────────────────────
#
#' @param cases_dt   data.table with prov_ and cases (or count)
#' @param value_col  column to fill (default "cases")
#' @param title      plot title
#' @param fill_lab   legend title
#' @param interactive  if TRUE, return ggplotly; else ggplot

plot_province_map <- function(cases_dt,
                              value_col   = "cases",
                              title       = NULL,
                              fill_lab    = "Cases",
                              interactive = TRUE) {

  prov_shape <- get_province_shape()

  # Match province codes to shape
  # NMCleaner shapes use full names; create a lookup
  prov_lookup <- data.table(
    province = c("Eastern Cape", "Free State", "Gauteng", "KwaZulu-Natal",
                 "Limpopo", "Mpumalanga", "Northern Cape", "North West",
                 "Western Cape"),
    prov_    = c("EC", "FS", "GP", "KZN", "LP", "MP", "NC", "NW", "WC")
  )

  cases_dt <- as.data.table(copy(cases_dt))
  merged <- merge(prov_lookup, cases_dt, by = "prov_", all.x = TRUE)
  shape_merged <- merge(prov_shape, merged, by = "province", all.x = TRUE)
  shape_merged$label <- paste0(shape_merged$prov_, "\nn=",
                                ifelse(is.na(shape_merged[[value_col]]), 0,
                                       format(shape_merged[[value_col]], big.mark = ",")))

  p <- ggplot(shape_merged) +
    geom_sf(aes(fill = .data[[value_col]])) +
    geom_sf_text(aes(label = label), size = 3, colour = "#333333") +
    scale_fill_viridis_c(option = "C", na.value = "#F0F0F0", name = fill_lab) +
    labs(title = title) +
    theme_nmc_map()

  if (interactive) return(ggplotly(p, tooltip = c("fill", "text")))
  return(p)
}


# ── 2. Province choropleth (incidence) ───────────────────────────────────────

plot_province_incidence_map <- function(inci_dt,
                                        condition_name = NULL,
                                        interactive    = TRUE) {
  plot_province_map(
    cases_dt    = inci_dt,
    value_col   = "incidence",
    title       = if (!is.null(condition_name))
                    paste0(condition_name, " — Incidence per 100 000") else NULL,
    fill_lab    = "Per 100k",
    interactive = interactive
  )
}


# ── 3. District choropleth ──────────────────────────────────────────────────
#
#' Adapted from map.R district mapping with fuzzy join.
#' @param cases_dt  data.table with district and cases (or value_col)

plot_district_map <- function(cases_dt,
                              value_col   = "cases",
                              title       = NULL,
                              fill_lab    = "Cases",
                              top_n_label = 10,
                              interactive = TRUE) {

  district_shape <- get_district_shape()

  cases_dt <- as.data.table(copy(cases_dt))
  cases_dt[, district_clean := standardise_district(district)]

  shape_dt <- as.data.table(st_drop_geometry(district_shape))
  shape_dt[, district_clean := standardise_district(district)]

  # Merge (exact match on cleaned name)
  merged <- merge(district_shape %>% mutate(district_clean = standardise_district(district)),
                  cases_dt[, c("district_clean", value_col), with = FALSE],
                  by = "district_clean", all.x = TRUE)

  # Label top N
  peaks <- merged %>%
    st_drop_geometry() %>%
    as.data.table() %>%
    .[order(-get(value_col))] %>%
    head(top_n_label)

  merged_sf <- st_as_sf(merged)

  p <- ggplot(merged_sf) +
    geom_sf(aes(fill = .data[[value_col]])) +
    scale_fill_viridis_c(option = "C", na.value = "#F0F0F0", name = fill_lab) +
    geom_sf_text(
      data = merged_sf %>% filter(district_clean %in% peaks$district_clean),
      aes(label = district), size = 2, colour = "#333333"
    ) +
    labs(title = title) +
    theme_nmc_map()

  if (interactive) return(ggplotly(p))
  return(p)
}
