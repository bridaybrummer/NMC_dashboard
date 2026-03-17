# scripts/render-by-province.R
# Renders province.qmd for each province and copies the output to the
# appropriate sub-folder (e.g. by-province/gp/index.html).
#
# province.qmd uses embed-resources: true so each HTML is self-contained.
# Data is loaded from data/processed/agg_province.rds inside the .qmd itself.

if (Sys.getenv("QUARTO_PREVIEW") == "1") {
  message("Skipping province renders for preview.")
  quit(save = "no")
}

provinces <- yaml::read_yaml("_variables.yml")$provinces
if (is.null(provinces)) {
  provinces <- c("EC", "FS", "GP", "KZN", "LP", "MP", "NC", "NW", "WC")
}

src_html <- file.path("by-province", "province.html")

for (p in provinces) {
  out_dir <- file.path("by-province", tolower(p))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # 1) Render province.qmd with this province parameter
  system2("quarto", c("render", "by-province/province.qmd",
                       "-P", paste0("province=", p)))

  # 2) Copy self-contained HTML into target folder
  file.copy(src_html, file.path(out_dir, "index.html"), overwrite = TRUE)

  # 3) Also sync to _site/
  site_dir <- file.path("_site", "by-province", tolower(p))
  dir.create(site_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(src_html, file.path(site_dir, "index.html"), overwrite = TRUE)

  message("Done: ", file.path(out_dir, "index.html"))
}

# Clean up intermediate file
unlink(src_html)
message("All ", length(provinces), " province pages rendered.")

