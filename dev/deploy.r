# ===============================
# Update GitHub Main & GitHub Pages
# ===============================

# ── Deploy options ────────────────────────────────────────────────────────────
# Set run_all_scripts = TRUE to (re-)generate ALL processed data before rendering.
# This includes the Burden of Disease indicators (prepare_bod_data.r) and any
# other optional preparation scripts.  Set to FALSE (default) to skip them and
# only run the core dashboard data pipeline (prepare_dashboard_data.r).
run_all_scripts <- TRUE
# ─────────────────────────────────────────────────────────────────────────────

# 1. Resolve project root regardless of where this script is called from.
#    Walks up from cwd checking for _quarto.yml; works whether you run via
#    Rscript dev/deploy.r, source() inside RStudio, or open the Project.
find_project_root <- function() {
  path <- normalizePath(getwd())
  repeat {
    if (file.exists(file.path(path, "_quarto.yml"))) return(path)
    parent <- dirname(path)
    if (parent == path) stop("Cannot locate project root (_quarto.yml not found). ",
                             "Run from the project directory.")
    path <- parent
  }
}
setwd(find_project_root())
cat("Project root:", getwd(), "\n")

# List files in the directory.
cat("Directory listing:\n")
system("ls")

# 2. Initialize the Git repository if it hasn't been initialized.
if (!dir.exists(".git")) {
    system("git init")
    cat("Initialized new Git repository.\n")
} else {
    cat("Git repository already exists.\n")
}

# Add the remote "origin" if it does not exist.
remotes <- system("git remote", intern = TRUE)
if (!("origin" %in% remotes)) {
    system("git remote add origin https://github.com/bridaybrummer/NMC_dashboard.git")
    cat("Added remote 'origin'.\n")
} else {
    cat("Remote 'origin' already exists.\n")
}

# 3. Refresh data before rendering.
rscript_path <- file.path(R.home("bin"), "Rscript")

# 3a. Core dashboard data (always runs).
cat("Running core data preparation pipeline...\n")
prep_result <- system(paste(shQuote(rscript_path), "R/prepare_dashboard_data.r"))
if (prep_result != 0) {
    stop("Core data preparation failed. Aborting deploy.")
}
if (!file.exists("data/processed/agg_national.rds") ||
    !file.exists("data/processed/agg_province.rds") ||
    !file.exists("data/processed/agg_district.rds")) {
    stop("Core data preparation did not produce expected .rds files. Aborting deploy.")
}
cat("✓ Core data preparation complete.\n")

# 3b. Optional full-pipeline scripts (runs when run_all_scripts = TRUE).
if (run_all_scripts) {
    cat("run_all_scripts = TRUE — running additional preparation scripts...\n")

    # Burden of Disease indicators (YLL, YLD, DALY) required by burden-of-disease/index.qmd
    cat("  Running prepare_bod_data.r...\n")
    bod_result <- system(paste(shQuote(rscript_path), "R/prepare_bod_data.r"))
    if (bod_result != 0) {
        stop("Burden of Disease data preparation failed. Aborting deploy.")
    }
    bod_files <- c("bod_deaths.rds", "bod_yll.rds", "bod_yll_province.rds",
                   "bod_yll_year.rds", "bod_yld.rds", "bod_yld_year.rds",
                   "bod_daly.rds", "bod_daly_province.rds", "bod_daly_year.rds")
    missing_bod <- bod_files[!file.exists(file.path("data/processed", bod_files))]
    if (length(missing_bod) > 0) {
        stop("prepare_bod_data.r did not produce: ", paste(missing_bod, collapse = ", "),
             ". Aborting deploy.")
    }
    cat("  ✓ Burden of Disease data complete.\n")

    # Add further optional scripts here following the same pattern, e.g.:
    # asr_result <- system(paste(shQuote(rscript_path), "R/prepare_asr_data.r"))

    cat("✓ All additional preparation scripts complete.\n")
} else {
    cat("run_all_scripts = FALSE — skipping additional preparation scripts.\n")
}

# 4. Render your Quarto site.
cat("Rendering Quarto site...\n")
system("quarto render")

# 3.1 — Abort early if render failed
if (file.exists("_site/index.html")) {
  cat("✓ _site/index.html found\n")
} else {
  cat("✗ _site/index.html missing - check render output\n")
  system("ls -la _site/")
  stop("Render failed — _site/index.html not produced. Aborting deploy.")
}

# 4. Stage all files (force-add _site/ which is in .gitignore).
system("git add .")
system("git add -f _site/")
cat("Staged all files (including _site/).\n")

# 5. Commit changes if any are staged.
commit_status <- system("git diff-index --quiet HEAD --")
if (commit_status != 0) {
    system("git commit -m 'Update site after Quarto render'")
    cat("Committed changes.\n")
} else {
    cat("No changes to commit.\n")
}

# 6. Increase the HTTP post buffer (useful for large pushes).
system("git config --global http.postBuffer 524288000")

# 7. Ensure the current branch is named 'main'.
system("git branch -M main")
cat("Renamed branch to 'main'.\n")

# 7.1 Clean up any corrupted branch references (e.g., "main 2" with spaces).
corrupt_refs <- list.files(".git/refs/heads", pattern = " ", full.names = TRUE)
if (length(corrupt_refs) > 0) {
    cat("Found corrupted branch references:\n")
    for (ref in corrupt_refs) {
        cat("  Removing:", ref, "\n")
        file.remove(ref)
    }
    cat("✓ Cleaned up corrupted refs.\n")
} else {
    cat("No corrupted branch references found.\n")
}

# 8. Pull remote changes (with rebase) to ensure your local branch is up-to-date.
cat("Pulling latest changes from remote 'main' branch with rebase...\n")

# Stash any remaining uncommitted changes that might block rebase
stash_result <- system("git stash push -m 'deploy-script-autostash' --include-untracked")
stashed <- (stash_result == 0 && length(system("git stash list | grep deploy-script-autostash", intern = TRUE)) > 0)

pull_result <- system("git pull origin main --rebase")
if (pull_result != 0) {
    cat("Pull with rebase failed. Aborting rebase...\n")
    system("git rebase --abort")
    if (stashed) system("git stash pop")
    stop("Rebase failed — resolve conflicts manually before deploying.")
}

# Restore stashed changes if any
if (stashed) {
    system("git stash pop")
    cat("Restored stashed changes.\n")
}

# (Optional) Check again if there are staged changes post-rebase, and commit if needed.
status <- system("git diff --cached --quiet")
if (status != 0) {
    system("git commit -m 'Update site after Quarto render (post-rebase)'")
    cat("Committed additional changes.\n")
} else {
    cat("No additional changes to commit.\n")
}

# 9. Push changes to the remote 'main' branch.
cat("Pushing changes to remote 'main' branch...\n")
push_result <- system("git push -u origin main")
if (push_result != 0) {
    stop("Push to main failed. Aborting deploy.")
}
cat("Main branch updated.\n")

# ====================================
# Update GitHub Pages (gh-pages branch)
# ====================================

cat("\nUpdating gh-pages branch...\n")

# Remove any existing local 'gh-pages' branch.
local_branches <- system("git branch", intern = TRUE)
if (any(grepl("gh-pages", local_branches))) {
    system("git branch -D gh-pages")
    cat("Deleted local 'gh-pages' branch.\n")
}

# Copy _site/ to a temp location so it survives branch switching.
system("rm -rf /tmp/_site_deploy && cp -r _site /tmp/_site_deploy")
cat("Copied _site/ to temp location.\n")

# Create an orphan gh-pages branch (no history, clean slate).
system("git checkout --orphan gh-pages")
system("git rm -rf . 2>/dev/null")              # remove all tracked files

# Remove ALL remaining untracked/ignored files (data/, caches, etc.)
# so they are not accidentally staged and pushed to the public gh-pages branch.
system("find . -not -name '.git' -not -path './.git/*' -mindepth 1 -delete 2>/dev/null")

# Copy the rendered site into the repo root and add .nojekyll.
system("cp -r /tmp/_site_deploy/* .")
system("touch .nojekyll")

# Stage everything and commit.
system("git add .")
system("git commit -m 'Deploy site to gh-pages'")
cat("Created gh-pages commit from _site/ contents.\n")

# Force push the new 'gh-pages' branch to GitHub.
system("git push origin gh-pages --force")
cat("Pushed 'gh-pages' branch to remote.\n")

# Return to main and clean up.
system("git checkout main")
system("git branch -D gh-pages")
system("rm -rf /tmp/_site_deploy")
cat("Cleaned up local 'gh-pages' branch and temp files.\n")

# Final status check.
cat("\nFinal Git status:\n")
system("git status")