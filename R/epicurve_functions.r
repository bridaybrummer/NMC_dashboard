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
      scale_fill_nmc(name = format_col_title(fill_col))
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

  # Build province-name labeller if faceting by province code
  facet_labeller <- if (facet_col == "prov_") {
    labeller(.cols = as_labeller(province_names))
  } else {
    label_value
  }

  ggplot(agg, aes(x = epiweek, y = n, fill = .data[[fill_col]])) +
    geom_col(width = 6, colour = NA) +
    facet_wrap(as.formula(paste("~", facet_col)), ncol = ncol, scales = "free_y",
               labeller = facet_labeller) +
    scale_x_date(date_labels = "%b %y", date_breaks = date_breaks) +
    scale_fill_nmc(name = format_col_title(fill_col)) +
    labs(title = title, x = NULL, y = "Notifications") +
    theme_nmc(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom",
      legend.title    = element_text(size = 8),
      legend.text     = element_text(size = 7),
      legend.key.size = unit(0.5, "lines")
    )
}


# ── 5. Reporting-delay truncation helper ─────────────────────────────────────
#
#' Mark recent days as "provisional" so plots/signals can either drop or
#' visually de-emphasise them. Recent counts are always under-reported because
#' notifications take days to enter the system.
#'
#' @param dt          data.table with `date`
#' @param truncate    integer days from the latest date to flag (default 14)
#' @param drop        if TRUE, remove rows in the truncation window;
#'                    otherwise add `is_provisional` flag (default FALSE)
#' @return  data.table

apply_reporting_truncation <- function(dt, truncate = 14L, drop = FALSE) {
  dt <- as.data.table(copy(dt))
  if (!"date" %in% names(dt)) return(dt)
  dt[, date := as.Date(date)]
  cutoff <- max(dt$date, na.rm = TRUE) - truncate
  if (drop) {
    return(dt[date <= cutoff])
  }
  dt[, is_provisional := date > cutoff]
  dt[]
}


# ── 6. Seasonal baseline (current year vs historical median + IQR) ───────────
#
#' Build a tidy historical-baseline table for a single condition (or all
#' conditions when grouped). For each MMWR epiweek the function returns the
#' median, 25–75% range and 5–95% range of weekly counts across `baseline_years`.
#'
#' @param dt              data.table: `date`, `count` (or `n`), optional `condition`
#' @param baseline_years  integer vector of years to use as historical baseline
#' @param value_col       column with daily counts (default "count")
#' @param group_cols      additional grouping columns (e.g. "condition")
#' @return  data.table with columns: epiweek (1–53), <group_cols>,
#'          median, p25, p75, p05, p95, n_years

seasonal_baseline <- function(dt,
                              baseline_years = NULL,
                              value_col      = "count",
                              group_cols     = NULL) {

  dt <- as.data.table(copy(dt))
  dt[, date := as.Date(date)]

  if (is.null(baseline_years)) {
    latest_year   <- max(year(dt$date), na.rm = TRUE)
    baseline_years <- (latest_year - 5):(latest_year - 1)
  }

  # Aggregate to (epiyear, epiweek) per group
  dt[, epiyear := lubridate::epiyear(date)]
  dt[, epiweek := lubridate::epiweek(date)]

  by_weekly <- c("epiyear", "epiweek", group_cols)
  weekly <- dt[, .(weekly_n = sum(get(value_col), na.rm = TRUE)), by = by_weekly]

  hist <- weekly[epiyear %in% baseline_years]
  if (nrow(hist) == 0L) return(data.table())

  by_baseline <- c("epiweek", group_cols)
  hist[, .(
    median  = median(weekly_n, na.rm = TRUE),
    p25     = quantile(weekly_n, 0.25, na.rm = TRUE),
    p75     = quantile(weekly_n, 0.75, na.rm = TRUE),
    p05     = quantile(weekly_n, 0.05, na.rm = TRUE),
    p95     = quantile(weekly_n, 0.95, na.rm = TRUE),
    n_years = uniqueN(epiyear)
  ), by = by_baseline]
}


#' Plot current-year weekly cases against the historical median + IQR ribbon.
#'
#' @param dt              data.table: `date`, `count` (single condition, or
#'                        already filtered)
#' @param baseline_years  see seasonal_baseline()
#' @param current_year    epiyear to highlight (default = latest year in data)
#' @param title           plot title
#' @param truncate_days   recent days flagged as provisional (default 14)
#' @return  ggplot object

plot_epicurve_seasonal <- function(dt,
                                   baseline_years = NULL,
                                   current_year   = NULL,
                                   value_col      = "count",
                                   title          = NULL,
                                   subtitle       = NULL,
                                   truncate_days  = 14L) {

  dt <- as.data.table(copy(dt))
  dt[, date := as.Date(date)]

  if (is.null(current_year)) current_year <- max(lubridate::epiyear(dt$date), na.rm = TRUE)
  if (is.null(baseline_years)) baseline_years <- (current_year - 5):(current_year - 1)

  base_dt <- seasonal_baseline(dt, baseline_years = baseline_years,
                               value_col = value_col)
  if (nrow(base_dt) == 0L) {
    warning("No historical baseline available for years ",
            paste(range(baseline_years), collapse = "–"))
  }

  # Current-year weekly aggregation
  dt[, epiyear := lubridate::epiyear(date)]
  dt[, epiweek := lubridate::epiweek(date)]
  dt[, week_start := mmwr_week_start(date)]

  cur <- dt[epiyear == current_year,
            .(weekly_n = sum(get(value_col), na.rm = TRUE),
              week_start = min(week_start)),
            by = .(epiweek)]

  # Provisional flag — last `truncate_days` worth of weeks
  latest_date <- max(dt$date, na.rm = TRUE)
  cur[, is_provisional := week_start > (latest_date - truncate_days)]

  baseline_label <- sprintf("Historical (%d–%d)",
                            min(baseline_years), max(baseline_years))

  p <- ggplot()

  if (nrow(base_dt) > 0L) {
    p <- p +
      geom_ribbon(data = base_dt,
                  aes(x = epiweek, ymin = p05, ymax = p95,
                      fill = "p05_p95"),
                  alpha = 0.18) +
      geom_ribbon(data = base_dt,
                  aes(x = epiweek, ymin = p25, ymax = p75,
                      fill = "p25_p75"),
                  alpha = 0.35) +
      geom_line(data = base_dt,
                aes(x = epiweek, y = median,
                    colour = "median"),
                linewidth = 0.6, linetype = "dashed")
  }

  p <- p +
    geom_line(data  = cur[is_provisional == FALSE],
              aes(x = epiweek, y = weekly_n, colour = "current"),
              linewidth = 1) +
    geom_point(data = cur[is_provisional == FALSE],
               aes(x = epiweek, y = weekly_n, colour = "current"),
               size = 1.6) +
    geom_line(data  = cur[is_provisional == TRUE],
              aes(x = epiweek, y = weekly_n, colour = "current"),
              linewidth = 0.7, linetype = "dotted") +
    scale_fill_manual(name = NULL,
                      values = c(p25_p75 = "#94a3b8", p05_p95 = "#cbd5e1"),
                      labels = c(p25_p75 = paste0(baseline_label, " IQR"),
                                 p05_p95 = paste0(baseline_label, " 5–95%"))) +
    scale_colour_manual(name = NULL,
                        values = c(median  = "#475569",
                                   current = "#b91c1c"),
                        labels = c(median  = paste0(baseline_label, " median"),
                                   current = paste0(current_year, " observed"))) +
    scale_x_continuous(breaks = seq(1, 53, 4),
                       expand = expansion(mult = c(0.01, 0.02))) +
    labs(title = title, subtitle = subtitle,
         x = "MMWR epiweek", y = "Weekly notifications",
         caption = "Dotted segment = provisional (subject to reporting delay)") +
    theme_nmc() +
    theme(legend.position = "bottom")

  p
}
