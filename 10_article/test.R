# load required R extension packages:
library("rticles")
library("rmarkdown")

# create a new document using a template:
rmarkdown::draft(file = "MyArticle.Rmd",
                 template = "copernicus_article",
                 package = "rticles", edit = FALSE)

# render the source of the document to the default output format:
rmarkdown::render(input = "MyArticle/MyArticle.Rmd")