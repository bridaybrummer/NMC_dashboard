# ══════════════════════════════════════════════════════════════════════════════
# run_report.R — Monthly NMC Surveillance Report Runner
# ══════════════════════════════════════════════════════════════════════════════
#
# USAGE
# ─────
# 1. Set REPORTING_MONTH to the first day of the month you want to report:
#       REPORTING_MONTH <- "YYYY-MM-01"
#
# 2. Choose an output format: "html" (web, self-contained) or "docx" (Word)
#
# 3. Run this script: source("monthly-reports/run_report.R")
#    or press Ctrl/Cmd+Shift+S in RStudio with this file open.
#
# Output is saved to monthly-reports/rendered/
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. Configuration ──────────────────────────────────────────────────────────

REPORTING_MONTH <- "2025-11-01"   # ← CHANGE THIS to your target month

OUTPUT_FORMATS  <- c("html", "docx")  # formats to render; remove one if not needed

# Optional: override the output directory (default: monthly-reports/rendered/)
OUTPUT_DIR <- file.path("monthly-reports", "rendered")

# ── 2. Derived values (no need to edit below) ─────────────────────────────────

report_date  <- as.Date(REPORTING_MONTH)
month_label  <- format(report_date, "%Y-%m")        # e.g. "2025-11"
month_pretty <- format(report_date, "%B %Y")        # e.g. "November 2025"

# (no ext / out_file here — derived per format in the render loop below)

message("╔══════════════════════════════════════════════════════╗")
message("  Rendering NMC Monthly Report")
message("  Month   : ", month_pretty)
message("  Formats : ", paste(toupper(OUTPUT_FORMATS), collapse = " + "))
message("  Output  : ", normalizePath(OUTPUT_DIR, mustWork = FALSE))
message("╚══════════════════════════════════════════════════════╝")

# ── 3. Create output directory if needed ──────────────────────────────────────

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  message("Created output directory: ", OUTPUT_DIR)
}

# ── 4. Render ─────────────────────────────────────────────────────────────────

if (!requireNamespace("quarto", quietly = TRUE)) {
  stop("The 'quarto' R package is required. Install with: install.packages('quarto')")
}

# Ensure Quarto can find Rscript (needed when running from CLI outside RStudio).
# When Quarto is invoked via Rscript, PATH may not include R's bin directory so
# Quarto falls back to its bundled Rscript (which may not exist). QUARTO_R fixes this.
if (nchar(Sys.getenv("QUARTO_R")) == 0) {
  rscript_path <- file.path(R.home("bin"), "Rscript")
  if (file.exists(rscript_path)) Sys.setenv(QUARTO_R = rscript_path)
}

rendered_paths <- character(0)

for (fmt in OUTPUT_FORMATS) {
  ext       <- if (fmt == "docx") "docx" else "html"
  out_file  <- paste0(month_label, "-monthly-report.", ext)
  dest_path <- file.path(OUTPUT_DIR, out_file)

  message(sprintf("\n[%s] Rendering %s ...", toupper(fmt), month_pretty))

  quarto_bin <- quarto::quarto_path()
  old_path   <- Sys.getenv("PATH")
  Sys.setenv(
    QUARTO_R = file.path(R.home("bin"), "Rscript"),
    PATH     = paste(R.home("bin"), old_path, sep = ":")
  )

  result <- system2(
    command = quarto_bin,
    args    = c(
      "render",
      normalizePath(file.path("monthly-reports", "report.qmd")),
      "--to", fmt,
      "-P",   paste0("reporting_month:", REPORTING_MONTH)
    ),
    stdout = "", stderr = "", wait = TRUE
  )
  Sys.setenv(PATH = old_path)

  if (result != 0) {
    warning("quarto CLI exited with status ", result, " for format: ", fmt)
    next
  }

  # Quarto writes report.{ext} next to the .qmd file; move and rename to OUTPUT_DIR.
  rendered_source <- file.path("monthly-reports", paste0("report.", ext))
  if (file.exists(rendered_source)) file.rename(rendered_source, dest_path)

  if (file.exists(dest_path)) {
    file_kb <- round(file.size(dest_path) / 1024)
    message(sprintf("✓ Saved: %s (%s kb)", basename(dest_path), format(file_kb, big.mark = ",")))
    rendered_paths <- c(rendered_paths, normalizePath(dest_path))
  } else {
    warning("Output not found after render: ", dest_path)
  }
}

if (length(rendered_paths) > 0) {
  message("\nAll outputs saved to: ", normalizePath(OUTPUT_DIR))
  for (p in rendered_paths) message("  ", basename(p))
  html_path <- rendered_paths[grepl("\\.html$", rendered_paths)]
  if (length(html_path) > 0 && interactive()) browseURL(html_path[1])
} else {
  warning("No reports were successfully rendered.")
}

