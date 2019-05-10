
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

#' @title EXPERIMENTAL: Continuous Segmentation
#' @description \code{chunkSegmentation} segments the input drama text table into 
#' chunks of equal size (by token count). Be careful, as this overrides the 
#' \code{Number.Act}, \code{begin.Act} and \code{end.Act} columns, so that the output 
#' can be used with act segmentation for functions like \code{dictionaryStatistics}, 
#' \code{figureStatistics} and \code{frequencytable}.
#' @export
#' @param t Mtext dataframe
#' @param n_chunks Number of chunks into which the text will be equally split
#' @example
#' data(rksp.0)
#' t <- chunkSegmentation(rksp.0$mtext, n_chunks=50) # splits into 50 chunks of equal size
#' dstat <- dictionaryStatistics(t, segment="Act") # uses chunks for segmentation
chunkSegmentation <- function(t, n_chunks=100) {
  t$Number.Act <- cut(seq_along(t$Token.surface), 100, labels = FALSE)
  lapply(1:n_chunks, function(n) {
    current_chunk <- which(t$Number.Act == n)
    t$begin.Act[t$Number.Act == n] <<- current_chunk[1]
    t$end.Act[t$Number.Act == n] <<- tail(current_chunk, n=1)
  })
  t
}