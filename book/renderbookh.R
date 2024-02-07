library(bookdown)

render_book("index.Rmd", "bookdown::html_document2", new_session=FALSE)

q("no")
