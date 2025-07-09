getwd()
setwd("/Users/johnny/GitHubStuff/statistics-lectures/courses/SSR/SSR_2023-2024/")

files <- list.files(full.names = TRUE, pattern = "\\.qmd", recursive = TRUE)
files <- files[-length(files)]

for (thisFile in files) {
  print(thisFile)
  quarto::quarto_render(thisFile)
}


# quarto::quarto_render(files[10])
