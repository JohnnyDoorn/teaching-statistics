# Shared setup for SSR slides
# Source this from topic files: source("../../_setup.R")
# Working directory is the lecture folder (e.g., ssr/2026/11. t_test/)
# so ../../ points to ssr/

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

# --- IQ data, collected live from the students via a Google Form ---
# One response sheet per year. The current year's sheet is empty until the
# students fill it in during the t-test lecture, so iqYear() falls back a year
# while it is still empty: the lecture renders on real data beforehand, and
# re-rendering mid-lecture picks up this year's numbers with no edit to a slide.
iqSheets <- list(
  "2024" = "https://docs.google.com/spreadsheets/d/1E9tlgFEv8OAyPBe_y2lwn2eUI1JmCg05Wk_wONAK1UM/edit?usp=sharing",
  "2025" = "https://docs.google.com/spreadsheets/d/1wfuAqJwIx3p-ZXPBi3oyQWwJi4kM1XFUL1ilVRaRpVQ/edit?usp=sharing",
  "2026" = "https://docs.google.com/spreadsheets/d/1dmUUdfpre14fkvT7G6u_ZmZnHfLb0wd9mMnL1x7yg4Y/edit?usp=sharing"
)

# Survives the re-source that every lecture child does, so one render fetches
# each sheet once rather than once per chunk.
if (!exists(".iqCache")) .iqCache <- new.env(parent = emptyenv())

loadIQData <- function(year) {
  key <- as.character(year)
  if (!is.null(.iqCache[[key]])) return(.iqCache[[key]])

  url <- iqSheets[[key]]
  if (is.null(url)) stop("No IQ data URL for year ", year)

  data <- as.data.frame(gsheet::gsheet2tbl(url))[, -1]  # drop the timestamp
  colnames(data) <- c("ownIQ", "nextIQ")

  .iqCache[[key]] <- data
  data
}

# The year whose IQ data the slides should use. Bump `current` each year.
iqYear <- function(current = 2026) {
  if (nrow(loadIQData(current)) > 0) return(current)
  message("IQ sheet for ", current, " is still empty - using ", current - 1,
          " instead. Re-render once the students have filled in the form.")
  current - 1
}

# --- Helper: path to central datasets folder ---
# From lecture dir: ../../../datasets/
datasetsPath <- function(...) {
  file.path("../../../datasets", ...)
}
