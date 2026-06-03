# Entry point for the test suite. Run from the project root with:
#   Rscript tests/testthat.R
# (or interactively: testthat::test_dir("tests/testthat"))
library(testthat)
library(shiny)
library(ggplot2)
library(bslib)

testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
