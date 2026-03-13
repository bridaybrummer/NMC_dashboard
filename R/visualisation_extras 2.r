# ──────────────────────────────────────────────────────────────────────────────
# visualisation_extras.r
# Speciality visualisation functions for the NMC dashboard
#
# Adapted from:
#   - NMC_reports_scripts/scripts_and_functions/ggridges_function.R  (ridge plots)
#   - NMC_reports_scripts/scripts_and_functions/dot_counts.R         (bubble matrix)
#   - NMC_reports_scripts/scripts_and_functions/context_table.R      (surveillance table)
#   - NMC_reports_scripts/scripts_and_functions/Data_completeness.r  (quality metrics)
#   - NMC_reports_scripts/scripts_and_functions/make_fever_rash.r    (condition grouping)
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(dplyr)
  library(lubridate)
})

source(file.path(here::here(), "R/plot_themes.r"))


# ══════════════════════════════════════════════════════════════════════════════
# A. CONDITION HELPERS
# ══════════════════════════════════════════════════════════════════════════════

#' Combine Measles + Rubella into "Fever-Rash" aggregate.
#' Adapted from NMC_reports_scripts/scripts_and_functions/make_fever_rash.r
#'
#' @param dt  data.table/data.frame with a `condition` column
#' @param label replacement label (default "Fever-Rash")
#' @return dt with Measles/Rubella recoded

make_fever_rash <- function(dt, label = "Fever-Rash") {
  dt <- as.data.table(copy(dt))
  dt[condition %in% c("Measles", "Rubella"), condition := label]
  return(dt)
}


# ══════════════════════════════════════════════════════════════════════════════
# B. RIDGE PLOT — Condition timelines
# ══════════════════════════════════════════════════════════════════════════════

#' Ridge-line density plot of notification dates by condition.
#' Adapted from NMC_reports_scripts/scripts_and_functions/ggridges_function.R
#'
#' @param dt             data.table with `notification_date` and `condition`
#' @param conditions     character vector of conditions to include (ordered top→bottom)
#' @param jittered       show jittered tick marks per case (default TRUE)
#' @param scale_factor   ridge overlap factor (default 1.5)
#' @param title          plot title

plot_condition_ridges <- function(dt,
                                  conditions   = NULL,
                                  jittered     = TRUE,
                                  scale_factor = 1.5,
                                  title        = "Notification density by condition") {

  if (!requireNamespace("ggridges", quietly = TRUE)) {
    stop("Install ggridges: install.packages('ggridges')")
  }

  dt <- as.data.table(copy(dt))
  dt[, notification_date := as.Date(notification_date)]

  if (is.null(conditions)) {
    conditions <- dt[, .N, by = condition][order(-N), condition]
  }

  dt <- dt[condition %in% conditions]
  dt[, condition := factor(condition, levels = rev(conditions))]

  p <- ggplot(dt, aes(x = notification_date, y = condition, fill = condition)) +
    ggridges::stat_density_ridges(
      show.legend    = FALSE,
      panel_scaling  = TRUE,
      scale          = scale_factor,
      alpha          = 0.75,
      jittered_points = jittered,
      point_shape    = "|",
      point_size     = 2,
      point_alpha    = 0.4
    ) +
    scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
    labs(title = title, x = "Date of notification", y = NULL) +
    theme_nmc() +
    theme(
      panel.spacing.y = unit(1.5, "lines"),
      strip.text.y.left = element_text(angle = 0)
    )

  return(p)
}


# ══════════════════════════════════════════════════════════════════════════════
# C. DOT/BUBBLE MATRIX — Condition × Province
# ══════════════════════════════════════════════════════════════════════════════

#' Bubble-matrix of notification counts (condition × province).
#' Adapted from NMC_reports_scripts/scripts_and_functions/dot_counts.R
#'
#' @param dt        data.table with condition, prov_, and count columns
#' @param count_col column for size (default "cases")
#' @param max_size  max bubble diameter (default 18)
#' @param title     plot title

plot_bubble_matrix <- function(dt,
                               count_col = "cases",
                               max_size  = 18,
                               title     = "Notifications by condition and province") {

  dt <- as.data.table(copy(dt))

  # Order conditions by total count ascending (bottom = most)
  cond_order <- dt[, .(total = sum(get(count_col), na.rm = TRUE)), by = condition]
  setorder(cond_order, total)
  dt[, condition := factor(condition, levels = cond_order$condition)]

  # Order provinces by total count descending (left = most)
  prov_order <- dt[, .(total = sum(get(count_col), na.rm = TRUE)), by = prov_]
  setorder(prov_order, -total)
  dt[, prov_ := factor(prov_, levels = prov_order$prov_)]

  ggplot(dt, aes(x = prov_, y = condition, size = .data[[count_col]],
                 colour = condition)) +
    geom_point(alpha = 0.8) +
    scale_size_area(max_size = max_size, name = "Cases") +
    scale_colour_viridis_d(option = "turbo", guide = "none") +
    labs(title = title, x = "Province", y = NULL) +
    theme_nmc() +
    theme(panel.grid.major = element_line(colour = "#EBEBEB"))
}


# ══════════════════════════════════════════════════════════════════════════════
# D. CONTEXT TABLE — Weekly surveillance summary
# ══════════════════════════════════════════════════════════════════════════════

#' Build a weekly context table (conditions × recent epiweeks) as a DT datatable.
#' Adapted from NMC_reports_scripts/scripts_and_functions/context_table.R
#'
#' @param dt           data.table with date, condition, count
#' @param n_weeks      number of recent epiweeks to display (default 12)
#' @param conditions   optional vector of conditions to include
#' @return  A wide data.table (condition × epiweek) suitable for DT::datatable()

build_context_table <- function(dt,
                                n_weeks    = 12L,
                                conditions = NULL) {

  dt <- as.data.table(copy(dt))
  dt[, epiweek := floor_date(as.Date(date), unit = "week")]

  if (!is.null(conditions)) {
    dt <- dt[condition %in% conditions]
  }

  # Aggregate
  agg <- dt[, .(n = sum(count, na.rm = TRUE)), by = .(condition, epiweek)]

  # Keep last n_weeks
  recent_weeks <- sort(unique(agg$epiweek), decreasing = TRUE)[seq_len(n_weeks)]
  recent_weeks <- sort(recent_weeks)
  agg <- agg[epiweek %in% recent_weeks]

  # Also compute the historical weekly average (all weeks)
  all_avg <- dt[, .(n = sum(count, na.rm = TRUE)),
                by = .(condition, epiweek = floor_date(as.Date(date), "week"))]
  avg_by_cond <- all_avg[, .(avg = round(mean(n, na.rm = TRUE), 1)), by = condition]

  # Pivot wide
  wide <- dcast(agg, condition ~ epiweek, value.var = "n", fill = 0L)

  # Add average column
  wide <- merge(avg_by_cond, wide, by = "condition", all.y = TRUE)
  setnames(wide, "avg", "Wkly Avg")

  # Order by total desc
  num_cols <- setdiff(names(wide), c("condition", "Wkly Avg"))
  wide[, total := rowSums(.SD, na.rm = TRUE), .SDcols = num_cols]
  setorder(wide, -total)
  wide[, total := NULL]

  # Format epiweek column names as "Wk NN"
  old_names <- num_cols
  new_names <- paste0("Wk ", format(as.Date(old_names), "%V"))
  setnames(wide, old_names, new_names)

  return(wide)
}


# ══════════════════════════════════════════════════════════════════════════════
# E. DATA COMPLETENESS — Quality metrics
# ══════════════════════════════════════════════════════════════════════════════

#' Calculate completeness (% non-missing) for key variables by province or year.
#' Adapted from NMC_reports_scripts/scripts_and_functions/Data_completeness.r
#'
#' @param dt     data.table with raw case-level data
#' @param vars   character vector of variable names to assess
#' @param by_col grouping column (e.g. "prov_" or "year")
#' @return  data.table: by_col, variable, n_total, n_complete, pct_complete

calc_completeness <- function(dt,
                              vars   = c("folder_no", "patient_name",
                                         "patient_surname", "symptom_date",
                                         "diagnosis_date", "patient_vital_status"),
                              by_col = "prov_") {

  dt <- as.data.table(copy(dt))

  # Keep only variables that exist in the data
 vars <- intersect(vars, names(dt))

  results <- rbindlist(lapply(vars, function(v) {
    dt[, .(
      variable     = v,
      n_total      = .N,
      n_complete   = sum(!is.na(get(v)) & !grepl("unknown", get(v), ignore.case = TRUE)),
      pct_complete = round(
        sum(!is.na(get(v)) & !grepl("unknown", get(v), ignore.case = TRUE)) / .N * 100, 1
      )
    ), by = by_col]
  }))

  return(results)
}


#' Heatmap of data completeness by variable and grouping.
#' Adapted from Data_completeness.r patterns.

plot_completeness_heatmap <- function(comp_dt,
                                      by_col = "prov_",
                                      title  = "Data completeness (%)") {

  ggplot(comp_dt, aes(x = .data[[by_col]], y = variable, fill = pct_complete)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = paste0(pct_complete, "%")), size = 3, colour = "#333333") +
    scale_fill_gradient2(low = "#ef4444", mid = "#fbbf24", high = "#22c55e",
                         midpoint = 75, limits = c(0, 100), name = "%") +
    labs(title = title, x = NULL, y = NULL) +
    theme_nmc() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))
}


# ══════════════════════════════════════════════════════════════════════════════
# F. SPARKLINE HELPERS — for DT tables
# ══════════════════════════════════════════════════════════════════════════════

#' Generate an inline SVG sparkline from a numeric vector.
#' Useful for DT::datatable() columns showing weekly trends.
#'
#' @param x         numeric vector
#' @param width     SVG width in px (default 80)
#' @param height    SVG height in px (default 20)
#' @param colour    line colour
#' @return  HTML string

sparkline_svg <- function(x, width = 80, height = 20, colour = "#1d4ed8") {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) < 2) return("")

  # Normalise to SVG viewport
  xmin <- 0; xmax <- length(x) - 1
  ymin <- min(x); ymax <- max(x)
  if (ymax == ymin) ymax <- ymin + 1

  xs <- (seq_along(x) - 1) / xmax * (width - 4) + 2
  ys <- height - 2 - (x - ymin) / (ymax - ymin) * (height - 4)

  points <- paste(xs, ys, sep = ",", collapse = " ")

  paste0(
    '<svg width="', width, '" height="', height, '" xmlns="http://www.w3.org/2000/svg">',
    '<polyline points="', points, '" fill="none" stroke="', colour,
    '" stroke-width="1.5" stroke-linecap="round"/>',
    '</svg>'
  )
}
