#' name_extract
#' 
#' extract names using gnfinder. Note that this is taken (near-)verbatim from 
#' the `namext` package by Scott Chamberlain, accessed at 
#' https://github.com/ropensci-archive/namext on 2023-02-15.
#' 
#' @param path (character) path to a file
#' @param verify (logical) verify names? default: `FALSE`
#' @param language (logical) text's language. by default it's
#' automatically detected. default: `NULL`
#' @param no_bayes (logical) do not run Bayes algorithms. default: `FALSE`
#' @param odds_details (logical) show details of odds calculation.
#' default: `FALSE`
#' @param data_sources (numeric vector) IDs of data sources to display for
#' matches. default: `NULL`. e.g., `c(1, 11, 179)`
#' @param tokens (logical) xxx. default: `NULL`
#' @return list of two data.frames:
#' 
#' - meta: metadata
#' - names: names and their parts, varies based on function parameters
#' @importFrom jsonlite fromJSON
#' @importFrom pdftools pdf_text
#' @importFrom tibble tibble
#' @export

name_extract <- function(path, verify = FALSE, language = NULL,
  no_bayes = FALSE, odds_details = FALSE, data_sources = NULL,
  tokens = NULL) {

  gnfinder_exists()

  assert(tokens, c("integer", "numeric"))
  assert(verify, "logical")
  assert(language, "character")
  assert(no_bayes, "logical")
  assert(odds_details, "logical")
  assert(data_sources, c("integer", "numeric"))

  type <- file_type(path)
  if (type == "pdf") {
    text <- pdf_text(path)
    txt_path <- tempfile(fileext = ".txt")
    writeLines(text, txt_path)
    path <- txt_path
  }
  args <- character(0)
  if (verify) args <- c(args, "-c")
  if (!is.null(language)) args <- c(args, "-l", language)
  if (no_bayes) args <- c(args, "-n")
  if (odds_details) args <- c(args, "-o")
  if (!is.null(data_sources)) args <- c(args, "-s", paste0(data_sources, collapse=","))
  if (!is.null(tokens)) args <- c(args, c("-t", tokens))

  z <- sys::exec_internal("gnfinder", c("find", args, path), error = FALSE)
  err_chk(z)
  parsed <- fromJSON(rawToChar(z$stdout))
  # parsed$metadata <- tibble::as_tibble(parsed$metadata)
  # parsed$names <- tibble::as_tibble(parsed$names)
  # return(parsed)
  return(tibble(parsed$names))
}

# internal functions

check4pkg <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    stop("Please install ", x, call. = FALSE)
  } else {
    invisible(TRUE)
  }
}

cl <- function(l) Filter(Negate(is.null), l)

assert <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

file_type <- function(x) {
  if (grepl("\\.pdf$", x)) return("pdf")
  if (grepl("\\.xml$", x)) return("xml")
  return(last(strsplit(x, "\\.")[[1]]))
}

last <- function(x) x[length(x)]

gnfinder_exists <- function() {
  check_gnf <- sys::exec_internal("gnfinder")
  if (check_gnf$status != 0) stop("`gnfinder` not found, see ?name_extract")
  return(TRUE)
}

err_chk <- function(z) {
  if (z$status != 0) {
    err <- rawToChar(z$stderr)
    err <- gsub("Error: ", "", err)
    # language replacement
    err <- gsub("-l detect", "language=\"detect\"", err)
    stop(err, call. = FALSE)
  }
}
