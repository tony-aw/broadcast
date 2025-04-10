
library(stringi)

rd_index <- function(funs, pkg = "broadcast") {
  tempfun <- function(x) {utils:::index.search(x, find.package(pkg)) |> basename()}
  lapply(funs, tempfun) |> unlist()
}

rd_links <- function(source_file, target_dir, funs) {
  rd <- readLines(source_file)
  
  # simple function/topic links:
  funlinks <- paste0("\\link{", funs, "}")
  rp <- paste0("\\code{[", funs, "](", rd_index(funs), ".qmd)}")
  temp <- stri_replace_all(
    rd, rp, fixed = funlinks, vectorize_all = FALSE
  )
  
  
  
  # alias function/topic links:
  extract_all <- stri_extract_all(temp, regex = "\\\\link\\[=.*?\\]\\{.*?\\}") |>
    unlist() |> na.omit() |> unique()
  if(length(extract_all) > 0L) {
    detect <- paste0("\\[=", funs, "\\]")
    detect <- paste0(detect, collapse = "|")
    extract_all <- stri_subset(extract_all, regex = detect)
    if(length(extract_all) > 0L) {
      extract_funs <- stri_replace_all(
        extract_all, c("", ""), regex = c("\\\\link\\[=", "\\]\\{.*?\\}"), vectorize_all = FALSE
      )
      extract_alias<-  stri_replace_all(
        extract_all, c("", ""), regex = c("\\\\link\\[=.*?\\]\\{", "\\}"), vectorize_all = FALSE
      )
      rp <- paste0("\\code{[", extract_alias, "]", "(", rd_index(extract_funs), ".qmd)}")
      temp <- stri_replace_all(
        temp, rp, fixed = extract_all, vectorize_all = FALSE
      )
    }
    
  }
  
  
  
  # write new Rd:
  fn <- file.path(target_dir, basename(source_file))
  writeLines(temp, con = fn)
  
}


qmd_extractlinks <- function(source_file, target_dir, funs) {
  
  qmd <- readLines(source_file)
  
  extract_all <- stri_extract_all(qmd, regex = "<code>\\[.*?\\]\\(.*?\\)</code>") |>
    unlist() |> na.omit() |> unique()
  if(length(extract_all) > 0L) {
    detect <- paste0(rd_index(funs), collapse = "|")
    extract_all <- stri_subset(extract_all, regex = detect)
    if(length(extract_all) > 0L) {
      rp <- stri_replace_all(
        extract_all, c("", ""), fixed = c("<code>", "</code>"),
        vectorize_all = FALSE
      )
      temp <- stri_replace_all(
        qmd, rp, fixed = extract_all, vectorize_all = FALSE
      )
    }
  }
  else {
    temp <- qmd
  }
  
  fn <- file.path(target_dir, basename(source_file))
  writeLines(temp, con = fn)
  
  
}

# ADAPTED from the altdoc package:
.pkg_name <- function(path) {
  return("broadcast")
}

.readlines <- function(x) {
  readLines(x, warn = FALSE)
}

rd2qmd <- function(source_file, temp_html, target_dir, path) {
  if (missing(source_file) || !file.exists(source_file)) {
    stop("source_file must be a valid file path.", call. = FALSE)
  }
  if (missing(source_file) || !dir.exists(target_dir)) {
    stop("target_dir must be a valid directory.", call. = FALSE)
  }
  
  # Rd -> html
  rd <- tools::parse_Rd(source_file)
  tools::Rd2HTML(rd, out = temp_html)
  
  # superfluous header and footer
  tmp <- .readlines(temp_html)
  tmp <- tmp[(grep("</table>$", tmp)[1] + 1):length(tmp)]
  tmp <- utils::head(tmp, -4)
  
  # first column (odd entries) of table in Arguments should not be wrapped
  idx <- grep("<td>", tmp)
  idx <- idx[seq_along(idx) %% 2 == 1]
  tmp[idx] <- sub(
    "<td>",
    '<td style = "white-space: collapse; font-family: monospace; vertical-align: top">',
    tmp[idx]
  )
  
  # escape the $ in man pages otherwise it thinks it is a latex equation and
  # doesn't escape symbols between two $.
  # tmp <- gsub("\\$", "\\\\$", tmp)
  
  # process \doi{...} tags that were expanded to \Sexpr[results=rd]{tools:::Rd_expr_doi("...")}
  tmp <- gsub(
    "(\\\\Sexpr\\[results=rd\\]\\{tools:::Rd_expr_doi\\(\\\")([^\\\"]+)(\\\"\\)\\})",
    "[doi:\\2](https://doi.org/\\2)",
    tmp
  )
  
  # examples: evaluate code blocks (assume examples are always last)
  pkg <- .pkg_name(path)
  pkg_load <- paste0("library(\"", pkg, "\")")
  idx <- which(tmp == "<h3>Examples</h3>")
  
  if (length(idx) == 1) {
    # until next section or the end
    idx_post_examples <- grep("<h3>", tmp)
    idx_post_examples <- idx_post_examples[idx_post_examples > idx]
    if (length(idx_post_examples) > 0) {
      ex <- tmp[(idx + 1):(idx_post_examples[1] - 1)]
    } else {
      ex <- tmp[(idx + 1):length(tmp)]
    }
    ex <- gsub("<.*>", "", ex)
    ex <- gsub("&lt;", "<", ex)
    ex <- gsub("&gt;", ">", ex)
    ex <- gsub("&amp;", "&", ex)
    ex <- gsub("\\$", "$", ex, fixed = TRUE)
    ex <- ex[!grepl("## Not run:", ex)]
    ex <- ex[!grepl("## End", ex)]
    
    # respect \dontrun{} and \donttest{}. This is too aggressive because it
    # ignores all tests whenever one of the two tags appear anywhere, but it
    # would be very hard to parse different examples wrapped or not wrapped in a
    # \donttest{}.
    block_eval <- !any(grepl("dontrun|donttest|## Not run:", tmp))
    
    # hack to support `examplesIf`. This is very ugly and probably fragile
    # added in roxygen2::rd-examples.R
    # https://github.com/r-lib/roxygen2/blob/db4dd9a4de2ce6817c17441d481cf5d03ef220e2/R/rd-examples.R#L17
    regex <- ') (if (getRversion() >= "3.4") withAutoprint else force)({ # examplesIf'
    exampleIf <- grep(regex, rd, fixed = TRUE)[1]
    if (!is.na(exampleIf[1])) {
      exampleIf <- sub(
        regex,
        "",
        as.character(rd)[exampleIf],
        fixed = TRUE
      )
      exampleIf <- sub("^if \\(", "", exampleIf)
      if (!isTRUE(try(eval(parse(text = exampleIf)), silent = TRUE))) {
        block_eval <- FALSE
      }
    }
    
    block <- sprintf(
      "```{r, warning=FALSE, message=FALSE, eval=%s, collapse = TRUE, comment = NA}", # I edited this a bit
      block_eval
    )
    
    tmp <- c(tmp[2:idx], block, pkg_load, ex, "```")
  }
  
  # cleanup equations
  tmp <- gsub(
    '<code class="reqn">(.*?)&gt;(.*?)</code>',
    '<code class="reqn">\\1>\\2</code>',
    tmp
  )
  tmp <- gsub(
    '<code class="reqn">(.*?)&lt;(.*?)</code>',
    '<code class="reqn">\\1<\\2</code>',
    tmp
  )
  tmp <- gsub('<code class="reqn">(.*?)</code>', "\\$\\1\\$", tmp)
  
  # cleanup code
  tmp <- gsub("&#8288;", "", tmp, fixed = TRUE)
  
  # title
  tmp <- gsub("<h2[^>]*>(.*)</h2>", "## \\1 {.unnumbered}\n", tmp)
  
  # Fix title level (use ## and not <h2> so that the TOC can be generated by
  # mkdocs)
  tmp <- gsub("<h2[^>]*>", "", tmp, perl = TRUE)
  tmp <- gsub("<.h2>", "", tmp)
  tmp <- gsub("<h3>", "### ", tmp)
  tmp <- gsub("</h3>", "", tmp)
  
  # paragraph tags are unnecessary in markdown
  tmp <- gsub("<p>", "", tmp, fixed = TRUE)
  tmp <- gsub("</p>", "", tmp, fixed = TRUE)
  
  # write to file
  fn <- file.path(target_dir, sub("Rd$", "qmd", basename(source_file)))
  writeLines(tmp, con = fn)
}

