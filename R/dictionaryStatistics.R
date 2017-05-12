#' This method retrieves word lists from the given URL (e.g., github) and
#' counts the number of occurrences of the words in the dictionaries
#' @param t A text
#' @param fieldnames A list of names for the dictionaries. It is expected that files with that name can be found below the URL.
#' @param normalizeByFigure Whether to normalize by figure speech length
#' @param normalizeByField Whether to normalize by dictionary size. You usually want this.
#' @param names Whether the resulting table contains figure ids or names
#' @param boost A scaling factor to generate nicer values
#' @param baseurl The base path delivering the dictionaries. Should end in a /, field names will be appended and fed into read.csv().
#' @importFrom stats aggregate
#' @importFrom stats ave
#' @importFrom utils read.csv
#' @export
dictionaryStatistics <- function(t, fieldnames=c(),
                                  normalizeByFigure = FALSE, normalizeByField = FALSE, names=FALSE, boost = 1,
                                  baseurl = "https://raw.githubusercontent.com/quadrama/metadata/master/fields/",
                                  column="Token.surface") {
  bylist <- list(t$drama, t$Speaker.figure_id)
  if (names == TRUE)
    bylist <- list(t$drama, t$Speaker.figure_surface)
  r <- aggregate(t, by=bylist, length)[,1:2]
  for (field in fieldnames) {
    url <- paste(baseurl, field, ".txt", sep="")
    list <- read.csv(url, header=F, fileEncoding = "UTF-8")
    r <- cbind(r,  dictionaryStatisticsSingle(t, tolower(list$V1), 
                                                normalizeByFigure = FALSE, 
                                                normalizeByField = normalizeByField, names=names, column=column)[,3])
  }
  colnames(r) <- c("drama", "figure", fieldnames)
  if (normalizeByFigure == TRUE) {
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
#' @param normalizeByFigure A logical value. Whether to normalize by the amount of tokens a figure speaks
#' @param normalizeByField A logical value. Whether to normalize by the size of the word field
#' @param bylist A list of columns, to be passed into the aggregate function. Can be used to control whether to count by figures or by dramas
#' @param column "Token.surface" or "Token.lemma"
#' @examples
#' data(rksp.0.text)
#' fstat <- dictionaryStatisticsSingle(rksp.0.text, wordfield=c("der"), names=TRUE)
#' @importFrom stats aggregate
#' @export
dictionaryStatisticsSingle <- function(t, wordfield=c(), names = FALSE, normalizeByFigure = FALSE, normalizeByField = FALSE, bylist = list(t$drama, t$Speaker.figure_id), column="Token.surface") {
  if (names == TRUE)
    bylist <- list(t$drama, t$Speaker.figure_surface)

  r <- aggregate(t[[column]], by=bylist, function(x) {
    if (normalizeByField == TRUE)
      length(x[tolower(x) %in% wordfield])/length(wordfield)
    else
      length(x[tolower(x) %in% wordfield])
  })

  if (ncol(r)==3) {
    colnames(r) <- c("drama", "figure", "x")
  } else if (ncol(r)==2) {
    colnames(r) <- c("drama", "x")
  }
  if (normalizeByFigure == TRUE) {
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
