#' This method retrieves word lists from the given URL (e.g., github) and
#' counts the number of occurrences of the words in the dictionaries
#' @param t A text
#' @param fieldnames A list of names for the dictionaries. It is expected that files with that name can be found below the URL.
#' @param normalize Whether to normalize by figure speech length
#' @param names Whether the resulting table contains figure ids or names
#' @param boost A scaling factor to generate nicer values
#' @param baseurl The url delivering the dictionairies
#' @export
dictionary.statistics <- function(t, fieldnames=c(),
                                  normalize = FALSE, names=FALSE, boost = 100,
                                  baseurl = "https://raw.githubusercontent.com/quadrama/metadata/master/fields/") {
  bylist <- list(t$drama, t$Speaker.figure_id)
  if (names == TRUE)
    bylist <- list(t$drama, t$Speaker.figure_surface)
  r <- aggregate(t, by=bylist, length)[,1:2]
  for (field in fieldnames) {
    url <- paste(baseurl, field, ".txt", sep="")
    list <- read.csv(url, header=F, fileEncoding = "UTF-8")
    r <- cbind(r,  dictionary.statistics.single(t, tolower(list$V1), normalize.by.figure =FALSE, normalize.by.field = TRUE,names=names)[,3])
  }
  colnames(r) <- c("drama", "figure", fieldnames)
  if (normalize == TRUE) {
    tokens <- aggregate(t$Token.surface, by=bylist, function(x) { length(x) })
    r[,-(1:2)] <- r[,-(1:2)] / ave(tokens[[3]], tokens[1:2], FUN=function(x) {x})
  }
  r[,-(1:2)] <- r[,-(1:2)] * boost
  r
}

#' @export
dictionary.statistics.single <- function(t, wordfield=c(), names = FALSE, normalize.by.figure = FALSE, normalize.by.field = FALSE) {
  bylist <- list(t$drama, t$Speaker.figure_id)
  if (names == TRUE)
    bylist <- list(t$drama, t$Speaker.figure_surface)

  r <- aggregate(t$Token.surface, by=bylist, function(x) {
    if (normalize.by.field == TRUE)
      length(x[tolower(x) %in% wordfield])/length(wordfield)
    else
      length(x[tolower(x) %in% wordfield])
  })


  colnames(r) <- c("drama", "figure", "x")
  if (normalize.by.figure == TRUE) {
    for (i in 1:nrow(r)) {
      r[i,]$x <- r[i,]$x / length(t[t$drama == r[i,1] & t$Speaker.figure_id == r[i,2],]$Token.lemma)
    }
  }
  r
}
