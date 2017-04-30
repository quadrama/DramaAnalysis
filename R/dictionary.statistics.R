#' This method retrieves word lists from the given URL (e.g., github) and
#' counts the number of occurrences of the words in the dictionaries
#' @param t A text
#' @param fieldnames A list of names for the dictionaries. It is expected that files with that name can be found below the URL.
#' @param normalize.by.figure Whether to normalize by figure speech length
#' @param normalize.by.field Whether to normalize by dictionary size. You usually want this.
#' @param names Whether the resulting table contains figure ids or names
#' @param boost A scaling factor to generate nicer values
#' @param baseurl The base path delivering the dictionaries. Should end in a /, field names will be appended and fed into read.csv().
#' @importFrom stats aggregate
#' @importFrom stats ave
#' @importFrom utils read.csv
#' @export
dictionary.statistics <- function(t, fieldnames=c(),
                                  normalize.by.figure = FALSE, normalize.by.field = FALSE, names=FALSE, boost = 1,
                                  baseurl = "https://raw.githubusercontent.com/quadrama/metadata/master/fields/") {
  bylist <- list(t$drama, t$Speaker.figure_id)
  if (names == TRUE)
    bylist <- list(t$drama, t$Speaker.figure_surface)
  r <- aggregate(t, by=bylist, length)[,1:2]
  for (field in fieldnames) {
    url <- paste(baseurl, field, ".txt", sep="")
    list <- read.csv(url, header=F, fileEncoding = "UTF-8")
    r <- cbind(r,  dictionary.statistics.single(t, tolower(list$V1), 
                                                normalize.by.figure = FALSE, 
                                                normalize.by.field = normalize.by.field, names=names)[,3])
  }
  colnames(r) <- c("drama", "figure", fieldnames)
  if (normalize.by.figure == TRUE) {
    tokens <- aggregate(t$Token.surface, by=bylist, function(x) { length(x) })
    r[,-(1:2)] <- r[,-(1:2)] / ave(tokens[[3]], tokens[1:2], FUN=function(x) {x})
  }
  r[,-(1:2)] <- r[,-(1:2)] * boost
  r
}

#' Calculates dictionary statistics for a single word field that is given as a character vector
#' @param t A single or multiple text(s)
#' @param wordfield A character vector containing the words or lemmas to be counted
#' @param names A logical values. Whether to use figure names or ids
#' @param normalize.by.figure A logical value. Whether to normalize by the amount of tokens a figure speaks
#' @param normalize.by.field A logical value. Whether to normalize by the size of the word field
#' @param bylist A list of columns, to be passed into the aggregate function. Can be used to control whether to count by figures or by dramas
#' @param column "Token.surface" or "Token.lemma"
#' @examples
#' data(rksp.0.text)
#' fstat <- dictionary.statistics.single(rksp.0.text, wordfield=c("der"), names=TRUE)
#' @importFrom stats aggregate
#' @export
dictionary.statistics.single <- function(t, wordfield=c(), names = FALSE, normalize.by.figure = FALSE, normalize.by.field = FALSE, bylist = list(t$drama, t$Speaker.figure_id), column="Token.surface") {
  if (names == TRUE)
    bylist <- list(t$drama, t$Speaker.figure_surface)

  r <- aggregate(t[[column]], by=bylist, function(x) {
    if (normalize.by.field == TRUE)
      length(x[tolower(x) %in% wordfield])/length(wordfield)
    else
      length(x[tolower(x) %in% wordfield])
  })

  if (ncol(r)==3) {
    colnames(r) <- c("drama", "figure", "x")
  } else if (ncol(r)==2) {
    colnames(r) <- c("drama", "x")
  }
  if (normalize.by.figure == TRUE) {
    for (i in 1:nrow(r)) {
      if (names == TRUE) {
        r[i,]$x <- r[i,]$x / length(t[t$drama == r[i,1] & t$Speaker.figure_surface == r[i,2],]$Token.lemma)
      } else {
        r[i,]$x <- r[i,]$x / length(t[t$drama == r[i,1] & t$Speaker.figure_id == r[i,2],]$Token.lemma)
      }
    }
  }
  r
}
