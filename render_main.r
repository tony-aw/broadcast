library(quarto)
library(stringi)

quarto_render("website/index.qmd", "gfm", "Readme.md")
file.copy("website/Readme.md", "Readme.md")
