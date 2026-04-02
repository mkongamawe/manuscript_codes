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

# Enable r session watcher to track radian from zsh or tmux
if (interactive()) {
  init_file <- file.path(
    Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"),
    ".vscode-R", "init.R"
  )
  
  if (Sys.getenv("VSCODE_INJECTION") == "1" && file.exists(init_file)) {
    source(init_file)
  }
}

.Last <- function() {
  if (interactive()) {
    cat("\n--- Terminating Session: Auto-snapshotting renv ---\n")
# prompt = FALSE ensures it doesn't wait for your 'y/n' input while you're trying to leave
    renv::snapshot(prompt = FALSE)
  }
}
