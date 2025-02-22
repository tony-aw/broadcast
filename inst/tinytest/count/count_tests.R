# count number of tests
library(broadcast)
library(tinytest)

# set working directory to source file location
SourceFileLocation <- function() {
  # BATCH way:
  path <- funr::get_script_path()
  if(!is.null(path)) return(path)
  
  # R-Studio way:
  if(Sys.getenv("RSTUDIO") == "1") {
    if(rstudioapi::isAvailable(version_needed = NULL,child_ok = FALSE)) {
      return(dirname(rstudioapi::getSourceEditorContext()$path))
    }
    if(is.null(knitr::current_input(dir = TRUE)) == FALSE) {
      return(knitr::current_input(dir = TRUE))
    }
    return(getwd())
  }
}

enumerate_total <- 0


# root folder tests ====

wd <- SourceFileLocation()
setwd(wd)
setwd("..")
getwd()
files <- list.files(normalizePath(getwd()), pattern = ".R", full.names = TRUE)
max.width <- max(stringi::stri_width(basename(files))) + 8
for(iFile in files) {
  capture.output(source(normalizePath(iFile)), file = nullfile()) |> suppressMessages()
  cat(stringi::stri_pad_right(basename(iFile), max.width), " -> ", enumerate,  "\n")
  enumerate_total <- enumerate_total + enumerate
}
rem <-  setdiff(ls(), c("SourceFileLocation", "enumerate", "enumerate_total"))
rm(list = rem)


# sub folder tests ====

subfolders <- c(
  "bind_array", "bc_num", "bc_b", "bc_cplx", "bc_str", "bc_list", "bc_generic",
  "internal"
)
wd <- SourceFileLocation()
setwd(wd)
setwd("..")
path <- file.path(getwd(), subfolders) |> normalizePath()
files <- list.files(path, pattern = "*.R", full.names = TRUE)
max.width <- max(stringi::stri_width(basename(files))) + 8

for(iSubFolder in subfolders) {
  
  wd <- SourceFileLocation()
  setwd(wd)
  setwd("..")
  setwd(normalizePath(iSubFolder))
  getwd()
  files <- list.files(normalizePath(getwd()), pattern = ".R", full.names = TRUE)
  print(iSubFolder)
  for(iFile in files) {
    capture.output(source(normalizePath(iFile)), file = nullfile()) |> suppressMessages()
    cat(stringi::stri_pad_right(basename(iFile), max.width), " -> ", enumerate,  "\n")
    enumerate_total <- enumerate_total + enumerate
  }
  rem <-  setdiff(ls(), c("SourceFileLocation", "enumerate", "enumerate_total", "max.width"))
  rm(list = rem)
  cat("\n")
  
}


print(enumerate_total)


# end ====

