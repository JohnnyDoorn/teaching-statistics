# Shared setup for SSR slides
# Source this from topic files: source("../../_setup.R")
# Working directory is the lecture folder (e.g., SSR/2026/11. t_test/)
# so ../../ points to SSR/

# --- Libraries ---
if (!"DT" %in% installed.packages())      install.packages("DT")
if (!"gsheet" %in% installed.packages())   install.packages("gsheet")
if (!"plotrix" %in% installed.packages())  install.packages("plotrix")

library("DT")
library("gsheet")
library("plotrix")

# --- Shared plotting functions ---
source("../../plotFunctionsSSR.r")

# --- Helper: standardised DT table ---
ssrTable <- function(data, height = 415, options = list(), ...) {
  defaults <- list(searching = FALSE,
                   scrollY   = height,
                   paging    = FALSE,
                   info      = FALSE)
  # User-supplied options override defaults
  opts <- modifyList(defaults, options)
  DT::datatable(data, options = opts, ...)
}

# --- Helper: load IQ data (Google Sheets) ---
loadIQData <- function(year = 2025) {
  urls <- list(
    "2024" = "https://docs.google.com/spreadsheets/d/1E9tlgFEv8OAyPBe_y2lwn2eUI1JmCg05Wk_wONAK1UM/edit?usp=sharing",
    "2025" = "https://docs.google.com/spreadsheets/d/1wfuAqJwIx3p-ZXPBi3oyQWwJi4kM1XFUL1ilVRaRpVQ/edit?usp=sharing"
  )
  url <- urls[[as.character(year)]]
  if (is.null(url)) stop("No IQ data URL for year ", year)
  data <- gsheet::gsheet2tbl(url)[, -1]
  colnames(data) <- c("ownIQ", "nextIQ")
  data
}

# --- Helper: path to central datasets folder ---
# From lecture dir: ../../../datasets/
datasetsPath <- function(...) {
  file.path("../../../datasets", ...)
}
