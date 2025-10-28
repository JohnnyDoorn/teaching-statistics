setwd("/Users/johnny/GitHubStuff/teaching-statistics/SSR/2025/")

files <- list.files(full.names = TRUE, pattern = "\\.qmd", recursive = TRUE)
files <- files[-length(files)]

for (thisFile in files[7]) {
  print(thisFile)
  quarto::quarto_render(thisFile)
}


