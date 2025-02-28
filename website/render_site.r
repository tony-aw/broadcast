
# first, terminate R
# then, set working directory to source file locations

library(quarto)
library(stringi)

file.copy("index.qmd", "vignettes/a_readme.qmd", overwrite = TRUE)

detection <- "---\ntitle:"
lst.files <- list.files("man/", pattern = "qmd")
for(i in lst.files) {
  filename <- i
  title <- stri_replace_last(filename, "", fixed = ".qmd")
  temp <- readLines(file.path("man", filename))
  check <- stringi::stri_detect(paste0(temp[1:2], collapse = "\n"), fixed = detection)
  if(!check && !stri_detect(title, fixed = "aaa")) {
    temp <- c("---", paste0("title: ", title), "---", temp)
    writeLines(temp, file.path("man", filename))
  }
}

quarto_render()

# wait until render is done!
quarto_preview()
