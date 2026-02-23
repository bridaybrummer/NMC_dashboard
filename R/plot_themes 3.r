# ──────────────────────────────────────────────────────────────────────────────
# plot_themes.r
# Consistent ggplot2 themes and colour palettes for the NMC dashboard
#
# Adapted from: NMC_reports_scripts/scripts_and_functions/epicurve.R
#               NMC_reports_scripts/scripts_and_functions/dot_counts.R
# ──────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(ggplot2)
  library(RColorBrewer)
})

# ── 1. Dashboard colour palette ──────────────────────────────────────────────
#    Extends the Paired palette with custom NMC colours for 18+ conditions.

nmc_palette <- c(
  brewer.pal(12, "Paired"),
  "#1f78b4", "darkolivegreen", "darkred", "orange", "#6a3d9a", "#b15928"
)

#' Named province colour vector (9 SA provinces)
province_colours <- c(
  EC  = "#1b9e77", FS  = "#d95f02", GP  = "#7570b3",

  KZN = "#e7298a", LP  = "#66a61e", MP  = "#e6ab02",
  NC  = "#a6761d", NW  = "#666666", WC  = "#1f78b4"
)

province_names <- c(
  EC  = "Eastern Cape",  FS = "Free State",    GP  = "Gauteng",
  KZN = "KwaZulu-Natal", LP = "Limpopo",       MP  = "Mpumalanga",
  NC  = "Northern Cape",  NW = "North West",   WC  = "Western Cape"
)

# ── 2. Base dashboard theme (light, clean) ───────────────────────────────────
#    Adapted from as_epicurve_excel() and dot_counts styling.

theme_nmc <- function(base_size = 11) {
  theme_minimal(base_size = base_size) %+replace%
    theme(
      text             = element_text(size = base_size, colour = "#333333"),
      panel.background = element_rect(fill = "#FAFAFA", colour = NA),
      plot.background  = element_rect(fill = "#FAFAFA", colour = NA),
      panel.grid.major = element_line(colour = "#E5E5E5", linewidth = 0.3),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "#FAFAFA", colour = NA),
      legend.text      = element_text(colour = "#333333"),
      legend.title     = element_text(colour = "#333333", size = base_size),
      plot.title       = element_text(size = base_size + 4, colour = "#333333",
                                       face = "bold", margin = margin(b = 6)),
      plot.subtitle    = element_text(size = base_size + 1, colour = "#666666",
                                       margin = margin(b = 8)),
      plot.caption     = element_text(size = base_size - 2, colour = "#888888"),
      axis.ticks       = element_line(colour = "#CCCCCC"),
      axis.text.x      = element_text(angle = 0, size = base_size - 1),
      strip.text       = element_text(face = "bold", size = base_size)
    )
}

# ── 3. Map theme (no axes) ──────────────────────────────────────────────────
#    Adapted from NMC_reports_scripts/scripts_and_functions/map.R

theme_nmc_map <- function(base_size = 11) {
  theme_nmc(base_size = base_size) %+replace%
    theme(
      axis.line   = element_blank(),
      axis.ticks  = element_blank(),
      axis.text   = element_blank(),
      axis.title  = element_blank(),
      panel.grid  = element_blank()
    )
}

# ── 4. Scale helpers ─────────────────────────────────────────────────────────

#' Fill scale using the extended NMC palette
scale_fill_nmc <- function(...) {
  scale_fill_manual(values = nmc_palette, ...)
}

#' Colour scale using the extended NMC palette
scale_colour_nmc <- function(...) {
  scale_colour_manual(values = nmc_palette, ...)
}

#' Province fill scale (9 fixed colours)
scale_fill_province <- function(...) {
  scale_fill_manual(values = province_colours, ...)
}

#' Province colour scale (9 fixed colours)
scale_colour_province <- function(...) {
  scale_colour_manual(values = province_colours, ...)
}
