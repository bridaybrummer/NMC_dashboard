# ===============================
# Update GitHub Main & GitHub Pages
# ===============================

# 1. Set the working directory to your project folder.
#setwd("/Users/briday/Desktop/study_stats_site")
setwd(getwd() )
cat("Current working directory:", getwd(), "\n")

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
cat("Running data preparation pipeline...\n")
prep_result <- system("Rscript R/prepare_dashboard_data.r")
if (prep_result != 0) {
    stop("Data preparation failed. Aborting deploy.")
}
if (!file.exists("data/processed/agg_national.rds") ||
    !file.exists("data/processed/agg_province.rds") ||
    !file.exists("data/processed/agg_district.rds")) {
    stop("Data preparation did not produce expected .rds files. Aborting deploy.")
}
cat("✓ Data preparation complete.\n")

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

# 8. Pull remote changes (with rebase) to ensure your local branch is up-to-date.
cat("Pulling latest changes from remote 'main' branch with rebase...\n")
pull_result <- system("git pull origin main --rebase")
if (pull_result != 0) {
    cat("Pull with rebase failed. Aborting rebase...\n")
    system("git rebase --abort")
    stop("Rebase failed — resolve conflicts manually before deploying.")
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