#' @title Load dictionary from github
#' @description This function loads the word fields that are available on the web.
#' @param fieldnames A list of names for the dictionaries. It is expected that files with that name can be found below the URL.
#' @param baseurl The base path delivering the dictionaries. Should end in a /, field names will be appended and fed into read.csv().
#' @importFrom utils read.csv
#' @export
loadFields <- function(fieldnames=c(),
                      baseurl="https://raw.githubusercontent.com/quadrama/metadata/master/fields/") {
  r <- list()
  for (field in fieldnames) {
    url <- paste(baseurl, field, ".txt", sep="")
    r[[field]] <- as.character((read.csv(url, header=F, fileEncoding = "UTF-8"))$V1)
  }
  r
}

#' This method retrieves word lists from the given URL (e.g., github) and
#' counts the number of occurrences of the words in the dictionaries
#' @param t A text
#' @param fieldnames A list of names for the dictionaries. It is expected that files with that name can be found below the URL.
#' @param fields A list of lists that contains the actual field names. This overrides whatever is given via fieldnames.
#' @param normalizeByFigure Whether to normalize by figure speech length
#' @param normalizeByField Whether to normalize by dictionary size. You usually want this.
#' @param names Whether the resulting table contains figure ids or names
#' @param boost A scaling factor to generate nicer values
#' @param baseurl The base path delivering the dictionaries. Should end in a /, field names will be appended and fed into read.csv().
#' @param column The table column we apply the dictionary on. Should be either "Token.surface" or "Token.lemma".
#' @param ci Whether to ignore case. Defaults to TRUE, i.e., case is ignored.
#' @importFrom stats aggregate
#' @importFrom stats ave
#' @export
dictionaryStatistics <- function(t, fields=loadFields(fieldnames,baseurl),
                                 fieldnames=c(),
                                  normalizeByFigure = FALSE, normalizeByField = FALSE, names=FALSE, boost = 1,
                                  baseurl = "https://raw.githubusercontent.com/quadrama/metadata/master/fields/",
                                  column="Token.surface",ci=TRUE) {
  bylist <- list(t$drama, t$Speaker.figure_id)
  if (names == TRUE)
    bylist <- list(t$drama, t$Speaker.figure_surface)
  r <- aggregate(t, by=bylist, length)[,1:2]
  for (fname in names(fields)) {
    r <- cbind(r,  dictionaryStatisticsSingle(t, fields[[fname]], ci=ci,
                                              normalizeByFigure = FALSE, 
                                              normalizeByField = normalizeByField, 
                                              names=names, column=column)[,3])
  }
  colnames(r) <- c("drama", "figure", names(fields))
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
#' @param fieldNormalizer defaults to the length of the wordfield
#' @param bylist A list of columns, to be passed into the aggregate function. Can be used to control whether to count by figures or by dramas
#' @param column "Token.surface" or "Token.lemma"
#' @param colnames The column names to be used
#' @param ci Logical. Wether to ignore case. Defaults to TRUE
#' @examples
#' data(rksp.0.text)
#' fstat <- dictionaryStatisticsSingle(rksp.0.text, wordfield=c("der"), names=TRUE)
#' @importFrom stats aggregate
#' @importFrom stats na.omit
#' @export
dictionaryStatisticsSingle <- function(t, wordfield=c(), 
                                       names = FALSE, 
                                       normalizeByFigure = FALSE, 
                                       normalizeByField = FALSE, 
                                       fieldNormalizer=length(wordfield), 
                                       bylist = ifelse(names==TRUE,
                                                       "drama,Speaker.figure_surface",
                                                       "drama,Speaker.figure_id"), 
                                       column="Token.surface", ci=TRUE,
                                       colnames=c("drama","figure","x")) 
  {
  dt <- as.data.table(t)
  if (ci) {
    wordfield <- tolower(wordfield)
    casing <- tolower
  } else {
    casing <- identity
  }
  if (normalizeByFigure == TRUE) {
    r <- dt[,
            ((length(stats::na.omit(match(casing(get(column)), wordfield))) / .N) 
              / ifelse(normalizeByField,fieldNormalizer,1)),
           keyby=bylist
           ]
  } else {
    r <- dt[,
            (length(stats::na.omit(match(casing(get(column)), wordfield)))
              / ifelse(normalizeByField,fieldNormalizer,1)),
            keyby=bylist
           ]
  }
  
  if (! is.null(colnames)) {
    colnames(r) <- colnames
  }
  
  r
}

dictionaryStatisticsSingleL <- function(...) {
  dstat <- dictionaryStatisticsSingle(...)
  as.list(dstat)
}

#' @title Dictionary Statistics as a list
#' @description This function applies the same logic as \code{\link{dictionaryStatistics}}, but returns the result as a list with multiple components, and the core data as a matrix
#' @param ... All parameters are passed to \code{\link{dictionaryStatistics}}
#' @seealso \code{\link{dictionaryStatistics}}
#' @details The returned list has three named elements:
#' \describe{
#' \item{drama}{The drama in which these counts have been counted}
#' \item{figure}{the figure these values has spoken}
#' \item{mat}{A matrix containing the actual values}
#' }
#' @export
dictionaryStatisticsL <- function(...) {
  dstat <- dictionaryStatistics(...)
  r <- as.list(dstat[,1:2])
  r$mat <- as.matrix(dstat[,-c(1,2)])
  r
}


dictionary.statistics <- function(...) {
  .Deprecated("dictionaryStatistics")
  dictionaryStatistics(...)
}