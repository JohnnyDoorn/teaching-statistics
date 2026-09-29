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

# The current academic year. Bump this each year (and add its sheet above).
iqCurrentYear <- 2026

# The year whose IQ data the slides should use.
iqYear <- function(current = iqCurrentYear) {
  if (nrow(loadIQData(current)) > 0) return(current)
  message("IQ sheet for ", current, " is still empty - using ", current - 1,
          " instead. Re-render once the students have filled in the form.")
  current - 1
}

# Writes the IQ data to datasets/<year>/ for use in JASP: this year's answers
# (wide), and this year's answers against last year's (long, with a year
# column). Does nothing while this year's sheet is still empty.
writeIQData <- function(current = iqCurrentYear) {
  if (iqYear(current) != current) return(invisible(NULL))
  thisYear <- loadIQData(current)
  lastYear <- loadIQData(current - 1)

  wide <- data.frame(`own-iq`      = thisYear$ownIQ,
                     `neighbor-iq` = thisYear$nextIQ, check.names = FALSE)
  long <- data.frame(year          = rep(c(current - 1, current),
                                         c(nrow(lastYear), nrow(thisYear))),
                     `own-iq`      = c(lastYear$ownIQ,  thisYear$ownIQ),
                     `neighbor-iq` = c(lastYear$nextIQ, thisYear$nextIQ),
                     check.names = FALSE)

  dir <- datasetsPath(current)
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  write.csv(wide, file.path(dir, sprintf("iq-estimates-%d.csv", current)),
            row.names = FALSE)
  write.csv(long, file.path(dir, sprintf("iq-estimates-%d-vs-%d.csv",
                                         current - 1, current)),
            row.names = FALSE)
}

# --- Helper: path to central datasets folder ---
# From lecture dir: ../../../datasets/
datasetsPath <- function(...) {
  file.path("../../../datasets", ...)
}
