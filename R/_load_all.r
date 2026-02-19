# ──────────────────────────────────────────────────────────────────────────────
# _load_all.r
# Master loader: source all dashboard utility functions
#
# Usage in any .qmd page:
#   source("R/_load_all.r")    # from project root
#   source("../R/_load_all.r") # from a sub-directory like by-province/
#
# Each module is self-contained and can also be sourced individually.
# ──────────────────────────────────────────────────────────────────────────────

project_root <- here::here()

source(file.path(project_root, "R/plot_themes.r"))
source(file.path(project_root, "R/epicurve_functions.r"))
source(file.path(project_root, "R/signal_detection.r"))
source(file.path(project_root, "R/incidence_functions.r"))
source(file.path(project_root, "R/asr_functions.r"))
source(file.path(project_root, "R/map_functions.r"))
source(file.path(project_root, "R/visualisation_extras.r"))
source(file.path(project_root, "R/quality_functions.r"))
source(file.path(project_root, "R/tabulate_functions.r"))
source(file.path(project_root, "R/epinow2_functions.r"))

cat("NMC dashboard functions loaded.\n")
