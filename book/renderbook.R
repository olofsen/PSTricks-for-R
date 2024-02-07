library(bookdown)

render_book("index.Rmd", "bookdown::pdf_document2", new_session=FALSE)

q("no")
