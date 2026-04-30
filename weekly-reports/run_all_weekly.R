# ══════════════════════════════════════════════════════════════════════════════
# run_all_weekly.R — Batch Weekly NMC IDSR Report Runner
# ══════════════════════════════════════════════════════════════════════════════
#
# Generates weekly IDSR reports for the last N complete CDC epiweeks, saving
# self-contained HTML and DOCX files to weekly-reports/rendered/.
#
# CDC epiweeks start on Sunday. The most recent COMPLETE epiweek is the
# one that ended last Saturday.
#
# USAGE
# ─────
# Standalone:
#   source("weekly-reports/run_all_weekly.R")
#
# Full deployment:
#   source("dev/deploy.r")   (which calls this automatically)
# ══════════════════════════════════════════════════════════════════════════════

# ── 1. Configuration ──────────────────────────────────────────────────────────

N_WEEKS        <- 52                    # how many past epiweeks to render
OUTPUT_FORMATS <- c("html", "docx")     # formats to render for each week
SKIP_EXISTING  <- FALSE                  # set FALSE to re-render all
OUTPUT_DIR     <- file.path("weekly-reports", "rendered")

# ── 2. Compute epiweek Sunday start dates ────────────────────────────────────

today <- Sys.Date()

# Days since last Sunday  (0 = Sun, 1 = Mon …, 6 = Sat)
days_since_sunday <- as.integer(format(today, "%w"))

# Start of the current (possibly incomplete) week
current_week_start <- today - days_since_sunday

# Most recent COMPLETE epiweek ends last Saturday
last_complete_week_start <- current_week_start - 7

# Build N-week sequence (oldest → newest)
week_starts <- rev(seq(
  from       = last_complete_week_start,
  by         = "-1 week",
  length.out = N_WEEKS
))

# CDC epiweek labels and pretty display strings
week_nums   <- lubridate::epiweek(week_starts)
week_years  <- lubridate::epiyear(week_starts)
week_stems  <- sprintf("%d-W%02d", week_years, week_nums)                 # e.g. "2026-W14"
week_pretty <- sprintf("Epiweek %d, %d (%s – %s)",
                       week_nums, week_years,
                       format(week_starts,       "%d %b"),
                       format(week_starts + 6,   "%d %b %Y"))

# ── 3. Display plan ───────────────────────────────────────────────────────────

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("  NMC Batch Weekly IDSR Report Runner\n")
cat(sprintf("  Weeks to render  : %d (%s → %s)\n",
            N_WEEKS, week_stems[1], week_stems[N_WEEKS]))
cat(sprintf("  Output directory : %s\n", OUTPUT_DIR))
cat(sprintf("  Formats          : %s\n", paste(toupper(OUTPUT_FORMATS), collapse = " + ")))
cat(sprintf("  Skip existing    : %s\n", SKIP_EXISTING))
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

# ── 4. Dependency check ───────────────────────────────────────────────────────

if (!requireNamespace("quarto", quietly = TRUE))
  stop("The 'quarto' R package is required. Install with: install.packages('quarto')")
if (!requireNamespace("lubridate", quietly = TRUE))
  stop("The 'lubridate' R package is required.")

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

success      <- 0L
skipped      <- 0L
failed       <- 0L
failed_items <- character(0)

for (i in seq_along(week_starts)) {
  cat(sprintf("[%d/%d] %s\n", i, length(week_starts), week_pretty[i]))

  for (fmt in OUTPUT_FORMATS) {
    ext       <- if (fmt == "docx") "docx" else "html"
    out_file  <- paste0(week_stems[i], "-weekly-report.", ext)
    dest_path <- file.path(OUTPUT_DIR, out_file)

    cat(sprintf("  [%s] ... ", toupper(fmt)))

    if (SKIP_EXISTING && file.exists(dest_path)) {
      cat("SKIPPED (exists)\n")
      skipped <- skipped + 1L
      next
    }

    tryCatch({
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
          normalizePath(file.path("weekly-reports", "report.qmd")),
          "--to", fmt,
          "-P",   paste0("reporting_week_start:", format(week_starts[i], "%Y-%m-%d"))
        ),
        stdout = "",
        stderr = "",
        wait   = TRUE
      )

      Sys.setenv(PATH = old_path)

      if (system2_result != 0) stop("quarto CLI exited with status ", system2_result)

      # Move rendered file (report.{ext}) into the archive directory
      rendered_source <- normalizePath(
        file.path("weekly-reports", paste0("report.", ext)),
        mustWork = FALSE
      )
      if (file.exists(rendered_source) &&
          rendered_source != normalizePath(dest_path, mustWork = FALSE)) {
        file.rename(rendered_source, dest_path)
      }

      if (file.exists(dest_path)) {
        file_kb <- round(file.size(dest_path) / 1024)
        cat(sprintf("OK (%s kb)\n", format(file_kb, big.mark = ",")))
        success <- success + 1L
      } else {
        cat("FAILED (output not found)\n")
        failed_items <- c(failed_items, paste0(week_pretty[i], " [", toupper(fmt), "]"))
        failed <- failed + 1L
      }

    }, error = function(e) {
      cat(sprintf("FAILED — %s\n", conditionMessage(e)))
      failed_items <<- c(failed_items, paste0(week_pretty[i], " [", toupper(fmt), "]"))
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

# ── 7. Generate Quarto blog listing stubs ────────────────────────────────────

posts_dir <- file.path("weekly-reports", "posts")
if (!dir.exists(posts_dir)) dir.create(posts_dir, recursive = TRUE)

for (i in seq_along(week_starts)) {
  html_dest <- file.path(OUTPUT_DIR, paste0(week_stems[i], "-weekly-report.html"))
  stub_path <- file.path(posts_dir, paste0(week_stems[i], ".qmd"))

  if (!file.exists(html_dest) || file.exists(stub_path)) next

  has_docx <- file.exists(
    file.path(OUTPUT_DIR, paste0(week_stems[i], "-weekly-report.docx"))
  )

  # Quarter label for categories
  month_num <- as.integer(format(week_starts[i], "%m"))
  q_str     <- paste0("Q", ceiling(month_num / 3))
  year_str  <- as.character(week_years[i])

  # Listing stub date = Sunday of the epiweek (for sort order)
  date_str <- format(week_starts[i], "%Y-%m-%d")

  btn_view <- sprintf(
    '[View Report](../rendered/%s-weekly-report.html){.btn .btn-primary target="_blank"}',
    week_stems[i]
  )
  btn_word <- if (has_docx) sprintf(
    ' &nbsp;\n[Download Word](../rendered/%s-weekly-report.docx){.btn .btn-outline-primary download="%s-weekly-report.docx"}',
    week_stems[i], week_stems[i]
  ) else ""

  stub <- paste0(
    '---\n',
    'title: "', sprintf("Epiweek %d, %d", week_nums[i], week_years[i]), '"\n',
    'date: ', date_str, '\n',
    'categories: [', year_str, ', ', q_str, ']\n',
    'description: "Weekly IDSR summary: Category 1 & 2 notifications, confirmed trends, and epitable for ',
    format(week_starts[i], "%d %B"), ' – ',
    format(week_starts[i] + 6, "%d %B %Y"), '."\n',
    '---\n\n',
    '::: {.callout-note appearance="minimal"}\n',
    'Reporting period: **', format(week_starts[i], "%d %B"), ' – ',
    format(week_starts[i] + 6, "%d %B %Y"),
    '** (CDC Epiweek ', week_nums[i], ', ', week_years[i], ')\n',
    ':::\n\n',
    btn_view, btn_word, '\n'
  )

  writeLines(stub, stub_path)
  message("Created listing stub: ", stub_path)
}

cat("Next steps:\n",
    "  1. Inspect rendered reports in weekly-reports/rendered/\n",
    "  2. Run: source('dev/deploy.r') to publish to GitHub Pages\n\n")
