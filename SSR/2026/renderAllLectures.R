setwd("/Users/johnny/GitHubStuff/teaching-statistics/SSR/2026/")

files <- list.files(full.names = TRUE, pattern = "^[0-9]+\\..+\\.qmd$", recursive = TRUE)

for (thisFile in files) {
  message("Rendering: ", thisFile)
  quarto::quarto_render(thisFile)
}
