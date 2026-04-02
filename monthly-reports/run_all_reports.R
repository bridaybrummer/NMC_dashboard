# ══════════════════════════════════════════════════════════════════════════════
# run_all_reports.R — Batch Monthly NMC Surveillance Report Runner
# ══════════════════════════════════════════════════════════════════════════════
#
# Generates monthly reports for the last N months, saving self-contained HTML
# files to monthly-reports/rendered/. Run this before deploying the dashboard
# so all reports are available on the site.
#
# DATA PIPELINE NOTE
# ──────────────────
# This script does NOT run the dashboard aggregation scripts
# (prepare_dashboard_data.r, prepare_bod_data.r, etc.). Monthly report.qmd
# reads new_master.feather directly, so no pre-aggregation is needed here.
# The full pipeline (data prep → reports → quarto render → deploy) is
# orchestrated by dev/deploy.r, which calls this script automatically.
#
# USAGE
# ─────
# Standalone (generates reports only):
#   source("monthly-reports/run_all_reports.R")
#
# Full deployment (data + reports + site render + git push):
#   source("dev/deploy.r")
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. Configuration ──────────────────────────────────────────────────────────

N_MONTHS       <- 2                      # how many past months to render
OUTPUT_FORMATS <- c("html", "docx")    # formats to render for each month
SKIP_EXISTING  <- TRUE                  # set FALSE to re-render reports that already exist
OUTPUT_DIR     <- file.path("monthly-reports", "rendered")

# ── 2. Derived months list ────────────────────────────────────────────────────

# Build a vector of the first day of each of the last N months.
# "Last month" = the month prior to the current month (data must exist).
today       <- Sys.Date()
this_month  <- as.Date(format(today, "%Y-%m-01"))

# Sequence of N months ending with last month
months_seq <- seq(
  from       = as.Date(format(this_month - 1, "%Y-%m-01")),  # last month
  by         = "-1 month",
  length.out = N_MONTHS
)
months_seq <- rev(sort(months_seq))   # chronological order (oldest first)

month_labels  <- format(months_seq, "%Y-%m")
month_pretty  <- format(months_seq, "%B %Y")

# ── 3. Display plan ───────────────────────────────────────────────────────────

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("  NMC Batch Monthly Report Runner\n")
cat(sprintf("  Months to render : %d (%s → %s)\n",
            N_MONTHS, month_pretty[1], month_pretty[N_MONTHS]))
cat(sprintf("  Output directory : %s\n", OUTPUT_DIR))
cat(sprintf("  Formats          : %s\n", paste(toupper(OUTPUT_FORMATS), collapse = " + ")))
cat(sprintf("  Skip existing    : %s\n", SKIP_EXISTING))
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

# ── 4. Dependency check ───────────────────────────────────────────────────────

if (!requireNamespace("quarto", quietly = TRUE)) {
  stop("The 'quarto' R package is required. Install with: install.packages('quarto')")
}
if (!requireNamespace("here", quietly = TRUE)) {
  stop("The 'here' R package is required. Install with: install.packages('here')")
}

# Quarto CLI needs to know where Rscript is. When called via Rscript itself
# (e.g. source() or Rscript ...) the PATH may not include R's bin directory,
# so Quarto falls back to looking in its own bundle (which doesn't have R).
# Setting QUARTO_R to the running Rscript resolves this.
if (nchar(Sys.getenv("QUARTO_R")) == 0) {
  rscript_path <- file.path(R.home("bin"), "Rscript")
  if (file.exists(rscript_path)) {
    Sys.setenv(QUARTO_R = rscript_path)
    message("Set QUARTO_R = ", rscript_path)
  }
}

if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR, recursive = TRUE)
  message("Created output directory: ", OUTPUT_DIR)
}

# ── 5. Render loop ────────────────────────────────────────────────────────────

total        <- length(months_seq) * length(OUTPUT_FORMATS)
success      <- 0L
skipped      <- 0L
failed       <- 0L
failed_items <- character(0)

for (i in seq_along(months_seq)) {
  cat(sprintf("[%d/%d] %s\n", i, length(months_seq), month_pretty[i]))

  for (fmt in OUTPUT_FORMATS) {
    ext       <- if (fmt == "docx") "docx" else "html"
    out_file  <- paste0(month_labels[i], "-monthly-report.", ext)
    dest_path <- file.path(OUTPUT_DIR, out_file)

    cat(sprintf("  [%s] ... ", toupper(fmt)))

    # Skip this format if the output already exists and SKIP_EXISTING is TRUE
    if (SKIP_EXISTING && file.exists(dest_path)) {
      cat("SKIPPED (exists)\n")
      skipped <- skipped + 1L
      next
    }

    tryCatch({
      # Call the Quarto CLI directly via system2() WITHOUT the 'env' argument.
      # When 'env' is supplied, R wraps the call in 'sh -c env KEY=VALUE cmd',
      # which breaks when PATH entries contain spaces (e.g. VS Code debug paths).
      # Instead, set vars in the current process with Sys.setenv() so they are
      # inherited by the child process directly.
      quarto_bin  <- quarto::quarto_path()
      rscript_bin <- file.path(R.home("bin"), "Rscript")
      old_path    <- Sys.getenv("PATH")

      Sys.setenv(
        QUARTO_R = rscript_bin,
        PATH     = paste(R.home("bin"), old_path, sep = ":")
      )

      system2_result <- system2(
        command = quarto_bin,
        args    = c(
          "render",
          normalizePath(file.path("monthly-reports", "report.qmd")),
          "--to", fmt,
          # No --output: Quarto writes report.{ext} next to the input file
          # (monthly-reports/), co-located with report_files/ so the
          # embed-resources step can find the Quarto JS libs.
          # (Providing --output with a filename redirects output to the project
          # root via `../` and breaks the embed step.)
          "-P",   paste0("reporting_month:", format(months_seq[i], "%Y-%m-%d"))
        ),
        stdout = "",   # print to console
        stderr = "",
        wait   = TRUE
      )

      Sys.setenv(PATH = old_path)   # restore PATH

      if (system2_result != 0) stop("quarto CLI exited with status ", system2_result)

      # Quarto placed the rendered file at monthly-reports/report.{ext}.
      # Move and rename it into the rendered/ archive directory.
      rendered_source <- normalizePath(
        file.path("monthly-reports", paste0("report.", ext)),
        mustWork = FALSE
      )
      if (file.exists(rendered_source) && rendered_source != normalizePath(dest_path, mustWork = FALSE)) {
        file.rename(rendered_source, dest_path)
      }

      if (file.exists(dest_path)) {
        file_kb <- round(file.size(dest_path) / 1024)
        cat(sprintf("OK (%s kb)\n", format(file_kb, big.mark = ",")))
        success <- success + 1L
      } else {
        cat("FAILED (output not found)\n")
        failed_items <- c(failed_items, paste0(month_pretty[i], " [", toupper(fmt), "]"))
        failed <- failed + 1L
      }

    }, error = function(e) {
      cat(sprintf("FAILED — %s\n", conditionMessage(e)))
      failed_items <<- c(failed_items, paste0(month_pretty[i], " [", toupper(fmt), "]"))
      failed <<- failed + 1L
    })
  }
}

# ── 6. Summary ────────────────────────────────────────────────────────────────

cat("\n")
cat("── Summary ──────────────────────────────────────────────────────\n")
cat(sprintf("  ✓ Rendered : %d\n", success))
cat(sprintf("  ↷ Skipped  : %d\n", skipped))
if (failed > 0) {
  cat(sprintf("  ✗ Failed   : %d\n", failed))
  for (fi in failed_items) cat("    -", fi, "\n")
}
cat("─────────────────────────────────────────────────────────────────\n")
cat(sprintf("Reports saved to: %s\n\n", normalizePath(OUTPUT_DIR)))

# ── 7. Remind about deployment ────────────────────────────────────────────────

# ── 7. Generate Quarto blog listing stubs ────────────────────────────────────
# For every month whose HTML rendered successfully, create a stub .qmd in
# monthly-reports/posts/ so that the native Quarto listing on index.qmd picks
# it up automatically. Stubs are only created once (not overwritten).

posts_dir <- file.path("monthly-reports", "posts")
if (!dir.exists(posts_dir)) dir.create(posts_dir, recursive = TRUE)

quarter_label <- function(d) paste0("Q", ceiling(as.integer(format(d, "%m")) / 3))

for (i in seq_along(months_seq)) {
  html_dest <- file.path(OUTPUT_DIR, paste0(month_labels[i], "-monthly-report.html"))
  stub_path <- file.path(posts_dir, paste0(month_labels[i], ".qmd"))

  if (!file.exists(html_dest) || file.exists(stub_path)) next

  has_docx  <- file.exists(file.path(OUTPUT_DIR, paste0(month_labels[i], "-monthly-report.docx")))
  year_str  <- format(months_seq[i], "%Y")
  q_str     <- quarter_label(months_seq[i])
  date_str  <- format(months_seq[i], "%Y-%m-%d")

  # Days in month for the reporting period note
  days_in_month <- as.integer(format(
    seq(months_seq[i], by = "month", length.out = 2)[2] - 1, "%d"
  ))
  period_note <- sprintf("Reporting period: **1 – %d %s**. Data sourced from the NMCSS.",
                         days_in_month, month_pretty[i])

  btn_view <- sprintf(
    '[View Report](../rendered/%s-monthly-report.html){.btn .btn-primary target="_blank"}',
    month_labels[i]
  )
  btn_word <- if (has_docx) sprintf(
    ' &nbsp;\n[Download Word](../rendered/%s-monthly-report.docx){.btn .btn-outline-primary download="%s-monthly-report.docx"}',
    month_labels[i], month_labels[i]
  ) else ""

  stub <- paste0(
    '---\n',
    'title: "', month_pretty[i], '"\n',
    'date: ', date_str, '\n',
    'categories: [', year_str, ', ', q_str, ']\n',
    'description: "National monthly summary of NMC notifications for ', month_pretty[i],
    ' — Category 1 & 2 conditions, provincial breakdown, data quality, and NMC App statistics."\n',
    '---\n\n',
    '::: {.callout-note appearance="minimal"}\n',
    period_note, '\n',
    ':::\n\n',
    btn_view, btn_word, '\n'
  )

  writeLines(stub, stub_path)
  message("Created listing stub: ", stub_path)
}

cat(
  "Next steps:\n",
  "  1. Run 'quarto render' (or 'quarto publish') to build the full site.\n",
  "  2. Quarto will copy monthly-reports/rendered/ into _site/ automatically.\n",
  "  3. The monthly-reports/posts/ stubs drive the native listing on the index page.\n\n"
)
