setwd("/Users/johnny/GitHubStuff/teaching-statistics/SSR/2025/")

files <- list.files(full.names = TRUE, pattern = "\\.qmd", recursive = TRUE)
files <- files[-length(files)]

for (thisFile in files[c(12, 13, 14, 15, 16)]) {
  print(thisFile)
  quarto::quarto_render(thisFile)
}


