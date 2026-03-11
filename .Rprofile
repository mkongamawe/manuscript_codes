source("renv/activate.R")

options(pkgType = "source")

# --- Custom Conda-renv Bridge ---
local({
  conda_lib <- file.path(Sys.getenv("HOME"),
                         "miniconda3/envs/r_env/lib/R/library")
  if (dir.exists(conda_lib)) {
    .libPaths(c(.libPaths(), conda_lib))
  }
})

.Last <- function() {
  if (interactive()) {
    cat("\n--- Terminating Session: Auto-snapshotting renv ---\n")
# prompt = FALSE ensures it doesn't wait for your 'y/n' input while you're trying to leave
    renv::snapshot(prompt = FALSE)
  }
}
