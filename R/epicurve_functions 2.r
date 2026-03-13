# ──────────────────────────────────────────────────────────────────────────────
# epicurve_functions.r
# Epicurve plotting functions for the NMC dashboard
#
# Adapted from: NMC_reports_scripts/scripts_and_functions/epicurve.R
#               NMC_reports_scripts/Disease_of_the_Month.rmd patterns
#
# Works with aggregated data from prepare_dashboard_data.r
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(ggplot2)
  library(data.table)
  library(lubridate)
  library(plotly)
})

source(file.path(here::here(), "R/plot_themes.r"))

# ── 1. Weekly epicurve (static ggplot) ───────────────────────────────────────
#
#' @param dt  data.table with columns: date, condition, count.
#'            `count` is daily case count (0-filled from prepare_dashboard_data).
#' @param date_col    column name for date (default "date")
#' @param count_col   column name for count (default "count")
#' @param fill_col    column to map to fill (e.g. "condition", "case_definition")
#' @param date_breaks e.g. "1 week", "1 month"
#' @param title       plot title
#' @return  ggplot object

plot_epicurve <- function(dt,
                          date_col    = "date",
                          count_col   = "count",
                          fill_col    = NULL,
                          date_breaks = "1 month",
                          date_labels = "%b %Y",
                          title       = NULL,
                          subtitle    = NULL) {

  dt <- as.data.table(copy(dt))

  # Aggregate to epiweek
  dt[, epiweek := floor_date(as.Date(get(date_col)), unit = "week")]

  if (!is.null(fill_col)) {
    agg <- dt[, .(n = sum(get(count_col), na.rm = TRUE)),
              by = c("epiweek", fill_col)]
    p <- ggplot(agg, aes(x = epiweek, y = n, fill = .data[[fill_col]])) +
      geom_col(width = 6, colour = NA) +
      scale_fill_nmc(name = fill_col)
  } else {
    agg <- dt[, .(n = sum(get(count_col), na.rm = TRUE)), by = .(epiweek)]
    p <- ggplot(agg, aes(x = epiweek, y = n)) +
      geom_col(width = 6, fill = "#1d4ed8", colour = NA)
  }

  p <- p +
    scale_x_date(date_labels = date_labels, date_breaks = date_breaks,
                 expand = expansion(mult = c(0.01, 0.03))) +
    labs(
      title    = title,
      subtitle = subtitle,
      x        = "Epiweek",
      y        = "Notifications"
    ) +
    theme_nmc()

  return(p)
}


# ── 2. Interactive plotly epicurve ───────────────────────────────────────────
#
#' Wraps plot_epicurve() into a plotly object with hover tooltips.

plotly_epicurve <- function(dt, ..., height = 400) {
  p <- plot_epicurve(dt, ...)
  ggplotly(p, height = height,
           tooltip = c("x", "y", "fill")) %>%
    plotly::layout(
      legend = list(orientation = "h", y = -0.15),
      hovermode = "x unified"
    )
}


# ── 3. Monthly bar chart (condition comparison) ─────────────────────────────
#
#' Stacked monthly bars by condition — useful for "By Province" overview.
#' @param dt        data.table with date, condition, count
#' @param top_n     show only top N conditions by total count

plot_monthly_bars <- function(dt,
                              date_col  = "date",
                              count_col = "count",
                              top_n     = 10,
                              title     = NULL) {

  dt <- as.data.table(copy(dt))
  dt[, month := floor_date(as.Date(get(date_col)), unit = "month")]

  # Rank conditions
  totals <- dt[, .(total = sum(get(count_col), na.rm = TRUE)), by = .(condition)]
  setorder(totals, -total)
  top_conditions <- head(totals$condition, top_n)

  dt[, condition_grp := ifelse(condition %in% top_conditions, condition, "Other")]
  dt[, condition_grp := factor(condition_grp,
                                levels = c(top_conditions, "Other"))]

  agg <- dt[, .(n = sum(get(count_col), na.rm = TRUE)),
            by = .(month, condition_grp)]

  ggplot(agg, aes(x = month, y = n, fill = condition_grp)) +
    geom_col(colour = NA) +
    scale_fill_nmc(name = "Condition") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "2 months") +
    labs(title = title, x = "Month", y = "Notifications") +
    theme_nmc() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# ── 4. Province comparison faceted epicurve ──────────────────────────────────
#
#' Small-multiple epicurves, one facet per province

plot_epicurve_faceted <- function(dt,
                                  date_col    = "date",
                                  count_col   = "count",
                                  facet_col   = "prov_",
                                  fill_col    = "condition",
                                  date_breaks = "3 months",
                                  ncol        = 3,
                                  title       = NULL) {

  dt <- as.data.table(copy(dt))
  dt[, epiweek := floor_date(as.Date(get(date_col)), unit = "week")]

  agg <- dt[, .(n = sum(get(count_col), na.rm = TRUE)),
            by = c("epiweek", facet_col, fill_col)]

  ggplot(agg, aes(x = epiweek, y = n, fill = .data[[fill_col]])) +
    geom_col(width = 6, colour = NA, show.legend = FALSE) +
    facet_wrap(as.formula(paste("~", facet_col)), ncol = ncol, scales = "free_y") +
    scale_x_date(date_labels = "%b %y", date_breaks = date_breaks) +
    scale_fill_nmc() +
    labs(title = title, x = NULL, y = "n") +
    theme_nmc(base_size = 9) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
