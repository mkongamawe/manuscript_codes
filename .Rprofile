source("renv/activate.R")

.Last <- function() {
  if (interactive()) {
    cat("\n--- Terminating Session: Auto-snapshotting renv ---\n")
# prompt = FALSE ensures it doesn't wait for your 'y/n' input while you're trying to leave
    renv::snapshot(prompt = FALSE)
  }
}
