# Coverage report for the app's source files.
# Run from the project root:  Rscript tests/coverage.R
#
# Measured per file because server.R source()s functions.R/formulas.R at load
# time (which would drop instrumentation if they were measured together):
#   * functions.R + formulas.R  <- the function/utility + formula tests
#   * server.R                  <- a testServer() driver over all branches
#   * ui.R                      <- sourcing the UI builds every control
suppressMessages({
  library(shiny); library(ggplot2); library(bslib)
  library(testthat); library(covr)
})

cov_ff <- file_coverage(
  source_files = c("functions.R", "formulas.R"),
  test_files   = c("tests/testthat/test-utils.R", "tests/testthat/test-functions.R")
)
cov_sv <- file_coverage("server.R", "tests/coverage/driver-server.R")
cov_ui <- file_coverage("ui.R",     "tests/coverage/driver-ui.R")

combined <- c(cov_ff, cov_sv, cov_ui)
class(combined) <- "coverage"

pct <- function(x) sprintf("%.1f%%", covr::percent_coverage(x))
cat("\n================ COVERAGE ================\n")
cat(sprintf("  functions.R + formulas.R : %s\n", pct(cov_ff)))
cat(sprintf("  server.R                 : %s\n", pct(cov_sv)))
cat(sprintf("  ui.R                     : %s\n", pct(cov_ui)))
cat("  ----------------------------------------\n")
cat(sprintf("  OVERALL                  : %s\n", pct(combined)))
cat("=========================================\n")

# Per-file zero-hit lines (useful when chasing the last few %).
z <- tryCatch(as.data.frame(covr::zero_coverage(combined)), error = function(e) NULL)
if (!is.null(z) && nrow(z)) {
  line_col <- intersect(c("first_line", "line"), names(z))[1]
  cat("\nUncovered lines (defensive guards / unreachable switch defaults):\n")
  for (f in unique(z$filename)) {
    ll <- sort(unique(z[[line_col]][z$filename == f]))
    cat(sprintf("  %s: %s\n", basename(f), paste(ll, collapse = ", ")))
  }
}
