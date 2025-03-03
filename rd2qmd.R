
# set-up ====
library(stringi)
source("siteutils.R")



# pre-process Rds for linking ====
funs <- getNamespaceExports("broadcast")
lst.files <- list.files("man", pattern = "Rd")
pkgpath <- getwd()
for(i in lst.files) {
  print(i)
  filepath <- file.path("man", i)
  outpath <- file.path("website", "preprocess")
  rd_links(filepath, outpath, funs)
}



# convert Rd to qmd ====
lst.files <- list.files(file.path("website", "preprocess"), pattern = "Rd")
pkgpath <- getwd()
for(i in lst.files) {
  print(i)
  filepath <- file.path("website", "preprocess", i)
  outpath <- file.path("website", "man")
  rd2qmd(filepath, outpath, pkgpath)
}


# adapt man titles ====
detection <- "---\ntitle:"
lst.files <- list.files("website/man/", pattern = "qmd")
for(i in lst.files) {
  filename <- i
  title <- stri_replace_last(filename, "", fixed = ".qmd")
  temp <- readLines(file.path("website", "man", filename))
  check <- stringi::stri_detect(paste0(temp[1:2], collapse = "\n"), fixed = detection)
  if(!check && !stri_detect(title, fixed = "aaa")) {
    temp <- c("---", paste0("title: ", title), "---", temp)
    writeLines(temp, file.path("website", "man", filename))
  }
}


# unpack links ====
funs <- getNamespaceExports("broadcast")
lst.files <- list.files("website/man/", pattern = "qmd")
for(i in lst.files) {
  filepath <- file.path("website", "man", i)
  outpath <- file.path("website", "man")
  qmd_extractlinks(filepath, outpath, funs)
}


# end of rd2qmd ====

