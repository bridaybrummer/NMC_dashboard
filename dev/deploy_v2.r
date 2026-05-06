# =============================================================================
# deploy_v2.r — Modular Dashboard Deploy Function
# =============================================================================
#
# A single function `deploy()` that lets you pick exactly which preparation
# steps, reports, and publish actions run.  Every stage can be toggled
# individually, so you can do quick partial deploys (e.g. just re-render the
# site after editing a .qmd) or a full from-scratch rebuild.
#
# QUICK USAGE
# ───────────
#   source("dev/deploy_v2.r")
#
#   # Full rebuild (everything ON)
#   deploy(preset = "full")
#
#   # Default fast deploy (core data + render + push)
#   deploy()
#
#   # Just re-render and push (skip all data prep & reports)
#   deploy(preset = "render_only")
#
#   # Custom: refresh BoD + 6 monthly reports, keep weekly reports as-is
#   deploy(
#     run_bod        = TRUE,
#     run_monthly    = TRUE,  n_months = 6,
#     run_weekly     = FALSE,
#     run_forecasts  = FALSE
#   )
#
#   # Dry run — show plan, don't execute
#   deploy(dry_run = TRUE)
#
# PRESETS
# ───────
#   "full"        – everything on (slowest, most complete)
#   "default"     – core data + render + push (no BoD/ASR/forecasts/reports)
#   "data_only"   – run all data scripts, skip render & push
#   "reports"     – regenerate weekly + monthly reports only
#   "render_only" – just `quarto render` + git push
#   "site_only"   – render + push main + push gh-pages (no data, no reports)
# =============================================================================


# ── Helper: locate project root ──────────────────────────────────────────────
.find_project_root <- function() {
  path <- normalizePath(getwd())
  repeat {
    if (file.exists(file.path(path, "_quarto.yml"))) return(path)
    parent <- dirname(path)
    if (parent == path) {
      stop("Cannot locate project root (_quarto.yml not found). ",
           "Run from the project directory.")
    }
    path <- parent
  }
}


# ── Helper: pretty section banner ────────────────────────────────────────────
.banner <- function(msg) {
  bar <- strrep("─", max(60, nchar(msg) + 4))
  cat("\n", bar, "\n  ", msg, "\n", bar, "\n", sep = "")
}


# ── Helper: run an R script via Rscript and stop on failure ──────────────────
.run_rscript <- function(script, label = script) {
  rscript_path <- file.path(R.home("bin"), "Rscript")
  cat(sprintf("→ Running %s ...\n", label))
  rc <- system(paste(shQuote(rscript_path), shQuote(script)))
  if (rc != 0) stop(sprintf("✗ %s failed (exit %d). Aborting deploy.", label, rc))
  cat(sprintf("✓ %s complete.\n", label))
  invisible(TRUE)
}


# ── Helper: clean corrupted refs (safe; from v1) ─────────────────────────────
.clean_corrupt_refs <- function() {
  clean <- function(dir) {
    if (!dir.exists(dir)) return(character(0))
    refs <- list.files(dir, pattern = " ", full.names = TRUE, recursive = TRUE)
    for (ref in refs) {
      cat("  Removing corrupted ref:", ref, "\n")
      file.remove(ref)
    }
    refs
  }
  removed <- c(clean(".git/refs/heads"), clean(".git/refs/remotes"))

  packed_refs_path <- ".git/packed-refs"
  if (file.exists(packed_refs_path)) {
    lines <- readLines(packed_refs_path, warn = FALSE)
    keep  <- vapply(lines, function(l) {
      if (startsWith(l, "#")) return(TRUE)
      length(strsplit(trimws(l), " ")[[1]]) == 2
    }, logical(1))
    if (any(!keep)) {
      writeLines(lines[keep], packed_refs_path)
      cat("✓ Removed", sum(!keep), "corrupted packed-ref(s).\n")
    }
  }
  invisible(removed)
}


# ── Presets ──────────────────────────────────────────────────────────────────
.presets <- list(
  full = list(
    run_dashboard_data = TRUE,
    run_bod            = TRUE,
    run_asr            = TRUE,
    run_forecasts      = TRUE,
    run_monthly        = TRUE,
    run_weekly         = TRUE,
    run_render         = TRUE,
    push_main          = TRUE,
    push_gh_pages      = TRUE
  ),
  default = list(
    run_dashboard_data = TRUE,
    run_bod            = FALSE,
    run_asr            = FALSE,
    run_forecasts      = FALSE,
    run_monthly        = FALSE,
    run_weekly         = FALSE,
    run_render         = TRUE,
    push_main          = TRUE,
    push_gh_pages      = TRUE
  ),
  data_only = list(
    run_dashboard_data = TRUE,
    run_bod            = TRUE,
    run_asr            = TRUE,
    run_forecasts      = TRUE,
    run_monthly        = FALSE,
    run_weekly         = FALSE,
    run_render         = FALSE,
    push_main          = FALSE,
    push_gh_pages      = FALSE
  ),
  reports = list(
    run_dashboard_data = FALSE,
    run_bod            = FALSE,
    run_asr            = FALSE,
    run_forecasts      = FALSE,
    run_monthly        = TRUE,
    run_weekly         = TRUE,
    run_render         = FALSE,
    push_main          = FALSE,
    push_gh_pages      = FALSE
  ),
  render_only = list(
    run_dashboard_data = FALSE,
    run_bod            = FALSE,
    run_asr            = FALSE,
    run_forecasts      = FALSE,
    run_monthly        = FALSE,
    run_weekly         = FALSE,
    run_render         = TRUE,
    push_main          = TRUE,
    push_gh_pages      = TRUE
  ),
  site_only = list(
    run_dashboard_data = FALSE,
    run_bod            = FALSE,
    run_asr            = FALSE,
    run_forecasts      = FALSE,
    run_monthly        = FALSE,
    run_weekly         = FALSE,
    run_render         = TRUE,
    push_main          = TRUE,
    push_gh_pages      = TRUE
  )
)


# ─────────────────────────────────────────────────────────────────────────────
# deploy() — main entry point
# ─────────────────────────────────────────────────────────────────────────────
#
# @param preset             One of names(.presets) or NULL.  Sets defaults that
#                           individual flags can override.
# @param run_dashboard_data Run R/prepare_dashboard_data.r
# @param run_bod            Run R/prepare_bod_data.r (Burden of Disease)
# @param run_asr            Run R/prepare_asr_data.r (Age-Standardised Rates)
# @param run_forecasts      Run R/run_forecasts.r (EpiNow2 forecasts)
# @param run_monthly        Generate monthly reports
# @param n_months           Override N_MONTHS in run_all_reports.R
# @param skip_existing_monthly Skip existing monthly HTML/DOCX (faster)
# @param run_weekly         Generate weekly IDSR reports
# @param n_weeks            Override N_WEEKS in run_all_weekly.R
# @param skip_existing_weekly Skip existing weekly HTML/DOCX (faster)
# @param run_render         Run `quarto render`
# @param push_main          Commit & push _site/ + sources to origin/main
# @param push_gh_pages      Force-push _site/ to origin/gh-pages
# @param commit_message     Override default commit message
# @param dry_run            Print plan only; run nothing
#
deploy <- function(preset                = "default",
                   run_dashboard_data    = NULL,
                   run_bod               = NULL,
                   run_asr               = NULL,
                   run_forecasts         = NULL,
                   run_monthly           = NULL,
                   n_months              = NULL,
                   skip_existing_monthly = NULL,
                   run_weekly            = NULL,
                   n_weeks               = NULL,
                   skip_existing_weekly  = NULL,
                   run_render            = NULL,
                   push_main             = NULL,
                   push_gh_pages         = NULL,
                   commit_message        = "Update site after Quarto render",
                   dry_run               = FALSE) {

  # ── 1. Resolve preset + per-flag overrides ────────────────────────────────
  if (!is.null(preset)) {
    if (!preset %in% names(.presets)) {
      stop("Unknown preset '", preset, "'. Choose one of: ",
           paste(names(.presets), collapse = ", "))
    }
    cfg <- .presets[[preset]]
  } else {
    cfg <- .presets$default
  }

  overrides <- list(
    run_dashboard_data = run_dashboard_data,
    run_bod            = run_bod,
    run_asr            = run_asr,
    run_forecasts      = run_forecasts,
    run_monthly        = run_monthly,
    run_weekly         = run_weekly,
    run_render         = run_render,
    push_main          = push_main,
    push_gh_pages      = push_gh_pages
  )
  for (k in names(overrides)) {
    if (!is.null(overrides[[k]])) cfg[[k]] <- isTRUE(overrides[[k]])
  }

  # ── 2. Project root ───────────────────────────────────────────────────────
  setwd(.find_project_root())
  cat("Project root:", getwd(), "\n")

  # ── 3. Plan summary ───────────────────────────────────────────────────────
  .banner("Deploy plan")
  cat(sprintf("  preset                 : %s\n", preset %||% "(none)"))
  cat(sprintf("  run_dashboard_data     : %s\n", cfg$run_dashboard_data))
  cat(sprintf("  run_bod                : %s\n", cfg$run_bod))
  cat(sprintf("  run_asr                : %s\n", cfg$run_asr))
  cat(sprintf("  run_forecasts          : %s\n", cfg$run_forecasts))
  cat(sprintf("  run_monthly            : %s%s\n", cfg$run_monthly,
              if (!is.null(n_months)) sprintf(" (n=%d)", n_months) else ""))
  cat(sprintf("  run_weekly             : %s%s\n", cfg$run_weekly,
              if (!is.null(n_weeks))  sprintf(" (n=%d)", n_weeks)  else ""))
  cat(sprintf("  run_render             : %s\n", cfg$run_render))
  cat(sprintf("  push_main              : %s\n", cfg$push_main))
  cat(sprintf("  push_gh_pages          : %s\n", cfg$push_gh_pages))
  cat(sprintf("  commit_message         : %s\n", commit_message))
  cat(sprintf("  dry_run                : %s\n", dry_run))

  if (dry_run) {
    cat("\n[dry_run] No actions executed.\n")
    return(invisible(cfg))
  }

  # ── 4. Git repo init / origin ─────────────────────────────────────────────
  if (cfg$push_main || cfg$push_gh_pages) {
    if (!dir.exists(".git")) {
      system("git init"); cat("Initialized new Git repository.\n")
    }
    remotes <- system("git remote", intern = TRUE)
    if (!("origin" %in% remotes)) {
      system("git remote add origin https://github.com/bridaybrummer/NMC_dashboard.git")
      cat("Added remote 'origin'.\n")
    }
  }

  # ── 5. Data preparation ───────────────────────────────────────────────────
  if (cfg$run_dashboard_data) {
    .banner("Step: core dashboard data")
    .run_rscript("R/prepare_dashboard_data.r", "prepare_dashboard_data.r")
    expected <- c("data/processed/agg_national.rds",
                  "data/processed/agg_province.rds",
                  "data/processed/agg_district.rds")
    missing <- expected[!file.exists(expected)]
    if (length(missing)) {
      stop("Core data prep did not produce: ", paste(missing, collapse = ", "))
    }
  }

  if (cfg$run_bod) {
    .banner("Step: Burden of Disease data")
    .run_rscript("R/prepare_bod_data.r", "prepare_bod_data.r")
    bod_files <- c("bod_deaths.rds", "bod_yll.rds", "bod_yll_province.rds",
                   "bod_yll_year.rds", "bod_yld.rds", "bod_yld_year.rds",
                   "bod_daly.rds", "bod_daly_province.rds", "bod_daly_year.rds")
    missing <- bod_files[!file.exists(file.path("data/processed", bod_files))]
    if (length(missing)) {
      stop("BoD prep did not produce: ", paste(missing, collapse = ", "))
    }
  }

  if (cfg$run_asr) {
    .banner("Step: ASR data")
    .run_rscript("R/prepare_asr_data.r", "prepare_asr_data.r")
  }

  if (cfg$run_forecasts) {
    .banner("Step: Forecasts (EpiNow2)")
    .run_rscript("R/run_forecasts.r", "run_forecasts.r")
  }

  # ── 6. Monthly reports ────────────────────────────────────────────────────
  if (cfg$run_monthly) {
    .banner("Step: Monthly reports")
    if (!is.null(n_months))              N_MONTHS      <<- as.integer(n_months)
    if (!is.null(skip_existing_monthly)) SKIP_EXISTING <<- isTRUE(skip_existing_monthly)
    source("monthly-reports/run_all_reports.R", local = FALSE)
    cat("✓ Monthly reports complete.\n")
  }

  # ── 7. Weekly reports ─────────────────────────────────────────────────────
  if (cfg$run_weekly) {
    .banner("Step: Weekly reports")
    if (!is.null(n_weeks))              N_WEEKS       <<- as.integer(n_weeks)
    if (!is.null(skip_existing_weekly)) SKIP_EXISTING <<- isTRUE(skip_existing_weekly)
    source("weekly-reports/run_all_weekly.R", local = FALSE)
    cat("✓ Weekly reports complete.\n")
  }

  # ── 8. Quarto render ──────────────────────────────────────────────────────
  if (cfg$run_render) {
    .banner("Step: Quarto render")
    rc <- system("quarto render")
    if (rc != 0) stop("quarto render failed (exit ", rc, ").")
    if (!file.exists("_site/index.html")) {
      system("ls -la _site/")
      stop("Render failed — _site/index.html not produced.")
    }
    cat("✓ _site/index.html present.\n")
  }

  # ── 9. Push main ──────────────────────────────────────────────────────────
  if (cfg$push_main) {
    .banner("Step: push main")
    system("git add .")
    system("git add -f _site/")

    commit_status <- system("git diff-index --quiet HEAD --")
    if (commit_status != 0) {
      system(sprintf("git commit -m %s", shQuote(commit_message)))
      cat("Committed changes.\n")
    } else {
      cat("No changes to commit.\n")
    }

    system("git config --global http.postBuffer 524288000")
    system("git branch -M main")

    .clean_corrupt_refs()
    system("git remote prune origin")

    stash_result <- system("git stash push -m 'deploy-script-autostash' --include-untracked")
    stashed <- (stash_result == 0 &&
                length(system("git stash list | grep deploy-script-autostash",
                              intern = TRUE)) > 0)

    pull_result <- system("git pull origin main --rebase")
    if (pull_result != 0) {
      system("git rebase --abort 2>/dev/null")
      if (stashed) system("git stash pop")
      stop("git pull --rebase failed; resolve conflicts manually.")
    }
    if (stashed) { system("git stash pop"); cat("Restored stashed changes.\n") }

    if (system("git diff --cached --quiet") != 0) {
      system(sprintf("git commit -m %s",
                     shQuote(paste(commit_message, "(post-rebase)"))))
    }

    push_result <- system("git push -u origin main")
    if (push_result != 0) stop("git push origin main failed.")
    cat("✓ main branch updated.\n")
  }

  # ── 10. Push gh-pages ─────────────────────────────────────────────────────
  if (cfg$push_gh_pages) {
    .banner("Step: push gh-pages")
    if (!file.exists("_site/index.html")) {
      stop("Cannot push gh-pages: _site/index.html missing. ",
           "Run with run_render = TRUE first.")
    }

    deploy_tmp <- "/tmp/_ghpages_deploy"
    system(paste("rm -rf", shQuote(deploy_tmp)))
    dir.create(deploy_tmp, recursive = TRUE)

    system(paste0("cp -r _site/. ", shQuote(deploy_tmp), "/"))
    file.create(file.path(deploy_tmp, ".nojekyll"))

    remote_url <- system("git remote get-url origin", intern = TRUE)

    old_wd <- getwd()
    setwd(deploy_tmp)
    system("git init -b gh-pages")
    system("git add .")
    system(sprintf("git commit -m %s", shQuote("Deploy site to gh-pages")))
    system(paste("git remote add origin", shQuote(remote_url)))
    rc <- system("git push origin gh-pages --force")
    setwd(old_wd)

    system(paste("rm -rf", shQuote(deploy_tmp)))
    if (rc != 0) stop("Push to gh-pages failed.")
    cat("✓ gh-pages branch updated.\n")
  }

  .banner("Deploy complete")
  invisible(cfg)
}


# ── Tiny null-coalescing helper (R has no built-in `%||%` < 4.4) ─────────────
`%||%` <- function(a, b) if (is.null(a)) b else a


# ─────────────────────────────────────────────────────────────────────────────
# When sourced interactively, just expose deploy() — DO NOT auto-run it.
# To run end-to-end like the old script:
#     source("dev/deploy_v2.r"); deploy(preset = "full")
# ─────────────────────────────────────────────────────────────────────────────
if (interactive()) {
  message("deploy_v2 loaded. Call deploy() with a preset or custom flags.")
}
