
# set-up ====
library(stringi)
source("siteutils.R")


################################################################################
# Create man pages ====
#

# make vector of function and topic names ====
funs <- c(
  getNamespaceExports("broadcast"),
  "broadcast_help", "broadcast_operators", "broadcast_casting",
  "broadcast_bind", "broadcast_linearalgebra", "broadcast_typecast"
)


## pre-process Rds for linking ====
lst.files <- list.files("man", pattern = "Rd")
pkgpath <- getwd()
for(i in lst.files) {
  print(i)
  filepath <- file.path("man", i)
  outpath <- file.path("preprocess", "man")
  rd_links(filepath, outpath, funs)
}



## convert Rd to qmd ====
lst.files <- list.files(file.path("preprocess", "man"), pattern = "Rd")
pkgpath <- getwd()
for(i in lst.files) {
  print(i)
  filepath <- file.path("preprocess", "man", i)
  temp_html <- stri_replace_last(i, ".html", fixed = ".Rd")
  temp_html <- file.path("preprocess", "man", temp_html)
  outpath <- file.path("website", "man")
  rd2qmd(filepath, temp_html, outpath, pkgpath)
}


## adapt man titles ====
detection <- "---\ntitle:"
lst.files <- list.files("website/man/", pattern = "qmd")
for(i in lst.files) {
  print(i)
  filename <- i
  title <- stri_replace_last(filename, "", fixed = ".qmd")
  temp <- readLines(file.path("website", "man", filename))
  check <- stringi::stri_detect(paste0(temp[1:2], collapse = "\n"), fixed = detection)
  if(!check && !stri_detect(title, fixed = "aaa")) {
    temp <- c("---", paste0("title: ", title), "---", temp)
    writeLines(temp, file.path("website", "man", filename))
  }
}


## unpack links in man pages ====
lst.files <- list.files("website/man/", pattern = "qmd")
for(i in lst.files) {
  filepath <- file.path("website", "man", i)
  outpath <- file.path("website", "man")
  qmd_extractlinks(filepath, outpath, funs)
}


################################################################################
# Process Vignettes, Index page ====
#


# clear any present links in vignettes ====
funs <- getNamespaceExports("broadcast")
lst.files <- list.files(file.path("website", "vignettes"), pattern = "qmd")
for(i in lst.files) {
  filepath <- file.path("website", "vignettes", i)
  temp <- readLines(filepath)
  p <- paste0("\\[", funs, "\\(\\)\\]", "\\(\\/man\\/.*\\.qmd\\)")
  rp <- paste0("`", funs, "()`")
  temp <- stri_replace_all(
    temp, rp, regex = p, vectorize_all = FALSE
  )
  writeLines(temp, file.path("website", "vignettes", i))
}


# create links in vignettes ====
funs <- getNamespaceExports("broadcast")
lst.files <- list.files(file.path("website", "vignettes"), pattern = "qmd")
for(i in lst.files) {
  filepath <- file.path("website", "vignettes", i)
  temp <- readLines(filepath)
  p <- paste0("`", funs, "()`")
  rp <- paste0("[", funs, "()]", "(/man/", rd_index(funs), ".qmd)")
  temp <- stri_replace_all(
    temp, rp, fixed = p, vectorize_all = FALSE
  )
  writeLines(temp, file.path("website", "vignettes", i))
}




################################################################################
# Process About pages ====
#



# clear any present links in About pages ====
funs <- getNamespaceExports("broadcast")
lst.files <- list.files(file.path("website", "About"), pattern = "qmd")
for(i in lst.files) {
  filepath <- file.path("website", "About", i)
  temp <- readLines(filepath)
  p <- paste0("\\[", funs, "\\(\\)\\]", "\\(\\/man\\/.*\\.qmd\\)")
  rp <- paste0("`", funs, "()`")
  temp <- stri_replace_all(
    temp, rp, regex = p, vectorize_all = FALSE
  )
  writeLines(temp, file.path("website", "About", i))
}


# create links in About pages ====
funs <- getNamespaceExports("broadcast")
lst.files <- list.files(file.path("website", "About"), pattern = "qmd")
for(i in lst.files) {
  filepath <- file.path("website", "About", i)
  temp <- readLines(filepath)
  p <- paste0("`", funs, "()`")
  rp <- paste0("[", funs, "()]", "(/man/", rd_index(funs), ".qmd)")
  temp <- stri_replace_all(
    temp, rp, fixed = p, vectorize_all = FALSE
  )
  writeLines(temp, file.path("website", "About", i))
}

# end of rd2qmd ====

