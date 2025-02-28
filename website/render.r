library(quarto)
library(stringi)

detection <- "---\ntitle:"
lst.files <- list.files("man/", pattern = "qmd")
for(i in lst.files) {
  filename <- i
  title <- stri_replace_last(filename, "", fixed = ".qmd")
  temp <- readLines(file.path("man", filename))
  check <- stringi::stri_detect(paste0(temp[1:2], collapse = "\n"), fixed=detection)
  if(!check) {
    temp <- c("---", paste0("title: ", title), "---", temp)
    writeLines(temp, file.path("man", filename))
  }
}

quarto_render()
