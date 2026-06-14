# Bookdown with R Markdown
unlink("_book", recursive = TRUE)# Remove the _book directory if it exists

bookdown::render_book("index.Rmd", output_format = "bookdown::gitbook")
bookdown::render_book("index.Rmd", output_format = "bookdown::pdf_book")
bookdown::render_book("index.Rmd", output_format = "all")
bookdown::render_book("index.Rmd", output_format = "html_document")

#Quarto
quarto::quarto_render()


