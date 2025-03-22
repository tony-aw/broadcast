
# set-up ====
library(stringi)
source("siteutils.R")


################################################################################
# Create man pages ====
#

## pre-process Rds for linking ====
funs <- getNamespaceExports("broadcast")
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
funs <- getNamespaceExports("broadcast")
lst.files <- list.files("website/man/", pattern = "qmd")
for(i in lst.files) {
  filepath <- file.path("website", "man", i)
  outpath <- file.path("website", "man")
  qmd_extractlinks(filepath, outpath, funs)
}


################################################################################
# Create Vignettes, Index page, and GitHub Readme ====
#

# copy & render intro template ====
from <- "intro_template.qmd"
to <- file.path("website", "vignettes", "a_readme.qmd")
file.copy(from, to, overwrite = TRUE)
to <- "README.qmd"
file.copy(from, to, overwrite = TRUE)
gfm <- readLines("README.qmd")
gfm <- stri_replace_all(
  gfm,
  "'R'",
  fixed = '`r fa("r-project")`'
)
writeLines(gfm, to)
quarto::quarto_render(to, "gfm", "README.md")



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

# copy readme vignette to index page ====
from <- file.path("website", "vignettes", "a_readme.qmd")
to <- file.path("website", "index.qmd")
file.copy(from, to, overwrite = TRUE)


# end of rd2qmd ====

