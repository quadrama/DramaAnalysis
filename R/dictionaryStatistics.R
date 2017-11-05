#' @title Load dictionary from github
#' @description This function loads the word fields that are available on the web.
#' @param fieldnames A list of names for the dictionaries. It is expected that files with that name can be found below the URL.
#' @param baseurl The base path delivering the dictionaries. Should end in a /, field names will be appended and fed into read.csv().
#' @param fileSuffix The suffix for the dictionary files
#' @importFrom utils read.csv
#' @export
loadFields <- function(fieldnames=c(),
                      baseurl="https://raw.githubusercontent.com/quadrama/metadata/master/fields/",
                      fileSuffix=".txt") {
  r <- list()
  for (field in fieldnames) {
    url <- paste(baseurl, field, fileSuffix, sep="")
    r[[field]] <- as.character((read.csv(url, header=F, fileEncoding = "UTF-8"))$V1)
  }
  r
}

#' @name dictionaryStatistics
#' @title Dictionary Use
#' @description These methods retrieve 
#' count the number of occurrences of the words in the dictionaries, 
#' across different speakers and/or segments.
#' The function \code{dictionaryStatistics()} calculates statistics for 
#' dictionaries with multiple entries, \code{dictionaryStatisticsSingle()} only
#' for a single word list. Functions ending on \code{L} return a list with 
#' multiple components.
#' @param t A text (data.frame or data.table)
#' @param fieldnames A list of names for the dictionaries. 
#' @param fields A list of lists that contains the actual field names. 
#' By default, we try to load the dictionaries using \code{fieldnames} and \code{baseurl}.
#' @param normalizeByFigure Logical. Whether to normalize by figure speech length
#' @param normalizeByField Logical. Whether to normalize by dictionary size. You usually want this.
#' @param names Logical. Whether the resulting table contains figure ids or names.
#' @param boost A scaling factor to generate nicer values.
#' @param baseurl The base path delivering the dictionaries. 
#' Should end in a \code{/}.
#' @param column The table column we apply the dictionary on. 
#' Should be either "Token.surface" or "Token.lemma".
#' @param ci Whether to ignore case. Defaults to TRUE, i.e., case is ignored.
#' @importFrom stats aggregate
#' @importFrom stats ave
#' @seealso \code{\link{loadFields}}
#' @rdname dictionaryStatistics
#' @examples
#' # Check multiple dictionary entries
#' data(rksp.0)
#' dstat <- dictionaryStatistics(rksp.0$mtext, fieldnames=c("Krieg","Familie"), names=TRUE)
#' @export
dictionaryStatistics <- function(t, fields=loadFields(fieldnames,baseurl),
                                 fieldnames=c(),
                                 normalizeByFigure = FALSE, 
                                 normalizeByField = FALSE, 
                                 names = FALSE, 
                                 boost = 1,
                                 baseurl = "https://raw.githubusercontent.com/quadrama/metadata/master/fields/",
                                 column="Token.surface", 
                                 ci = TRUE) {
  bylist <- list(t$corpus, t$drama, t$Speaker.figure_id)
  if (names == TRUE)
    bylist <- list(t$corpus, t$drama, t$Speaker.figure_surface)
  r <- aggregate(t, by=bylist, length)[,1:3]
  for (fname in names(fields)) {
    r <- cbind(r,  dictionaryStatisticsSingle(t, fields[[fname]], ci=ci,
                                              normalizeByFigure = FALSE, 
                                              normalizeByField = normalizeByField, 
                                              names=names, column=column)[,4])
  }
  colnames(r) <- c("corpus","drama", "figure", names(fields))
  if (normalizeByFigure == TRUE) {
    tokens <- aggregate(t$Token.surface, by=bylist, function(x) { length(x) })
    r[,-(1:3)] <- r[,-(1:3)] / ave(tokens[[4]], tokens[1:3], FUN=function(x) {x})
  }
  r[,-(1:3)] <- r[,-(1:3)] * boost
  r
}

#' @param wordfield A character vector containing the words or lemmas to be counted 
#' (only for \code{*Single}-functions)
#' @param fieldNormalizer defaults to the length of the wordfield
#' @param bylist A list of columns, to be passed into the aggregate function. Can be used to control whether to count by figures or by dramas
#' @param colnames The column names to be used
#' @examples
#' # Check a single dictionary entries
#' data(rksp.0)
#' fstat <- dictionaryStatisticsSingle(rksp.0$mtext, wordfield=c("der"), names=TRUE)
#' @importFrom stats aggregate
#' @importFrom stats na.omit
#' @rdname dictionaryStatistics
#' @export
dictionaryStatisticsSingle <- function(t, wordfield=c(), 
                                       names = FALSE, 
                                       normalizeByFigure = FALSE, 
                                       normalizeByField = FALSE, 
                                       fieldNormalizer=length(wordfield), 
                                       bylist = ifelse(names==TRUE,
                                                       "corpus,drama,Speaker.figure_surface",
                                                       "corpus,drama,Speaker.figure_id"), 
                                       column="Token.surface", ci=TRUE,
                                       colnames=c("corpus","drama","figure","x")) 
  {
  # we need this to prevent notes in R CMD check
  .N <- NULL
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
  
  stats::na.omit(r)
}

dictionaryStatisticsSingleL <- function(...) {
  dstat <- dictionaryStatisticsSingle(...)
  as.list(dstat)
}

#' @param ... All parameters are passed to \code{\link{dictionaryStatistics}}
#' @section Returned Lists:
#' The returned list has three named elements:
#' \describe{
#' \item{drama}{The drama in which these counts have been counted}
#' \item{figure}{the figure these values has spoken}
#' \item{mat}{A matrix containing the actual values}
#' }
#' @rdname dictionaryStatistics
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