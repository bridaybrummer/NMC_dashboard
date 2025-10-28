if (Sys.getenv("QUARTO_PREVIEW") == "1") {
  message("Skipping province renders for preview.")
  quit(save = "no")
}

Sys.getenv("QUARTO_PREVIEW")

source("R/aggregator_national.r")
source("R/tabulate_functions.r")

provinces <- yaml::read_yaml("_variables.yml")$provinces

src_html <- file.path("by-province", "province.html")


# scripts/render-by-province.R
# scripts/render-by-province.R
provinces <- c("EC", "FS", "GP", "KZN", "LP", "MP", "NC", "NW", "WC")
provinces  <- yaml::read_yaml("_variables.yml")$province

src_html <- file.path("by-province", "province.html")

for (p in provinces) {
  out_dir <- file.path("by-province", tolower(p))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  # 1) Render to by-province/province.html (default output)
  system2("quarto", c("render", "by-province/province.qmd", "-P", paste0("province=", p)))

  # 2) Copy into target folder as index.html
  file.copy(src_html, file.path(out_dir, "index.html"), overwrite = TRUE)
  message("Made: ", file.path(out_dir, "index.html"))
}
