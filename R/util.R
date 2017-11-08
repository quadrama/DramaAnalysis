
#' @title Utility functions
#' @description \code{ensureSuffix} makes certain that a character vector ends in 
#' a given suffix
#' @export
#' @param x The character vector
#' @param suffix The suffix
#' 
ensureSuffix <- function(x, suffix) {
  if (! endsWith(x, suffix)) {
    paste0(x, suffix)
  } else {
    x
  }
}