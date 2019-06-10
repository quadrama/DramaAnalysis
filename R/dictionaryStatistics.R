

#' @title Dictionary Handling
#' @description \code{loadFields()} loads dictionaries that are available on the web as plain text files.
#' @param fieldnames A list of names for the dictionaries. It is expected that files with that name can be found below the URL.
#' @param baseurl The base path delivering the dictionaries. Should end in a /, field names will be appended and fed into read.csv().
#' @param fileSuffix The suffix for the dictionary files
#' @param directory The last component of the base url. 
#' Useful to retrieve enriched word fields from metadata repo.
#' @param fileSep The file separator used to construct the URL
#' Can be overwritten to load local dictionaries.
#' @importFrom readr read_csv locale col_character
#' @section File Format:
#' Dictionary files should contain one word per line, with no comments or any other meta information. 
#' The entry name for the dictionary is given as the file name. It's therefore best if it does not contain
#' special characters. The dictionary must be in UTF-8 encoding, and the file needs to end on .txt.
#' @rdname dictionaryHandling
#' @export
loadFields <- function(fieldnames=c("Liebe","Familie"),
                      baseurl=paste("https://raw.githubusercontent.com/quadrama/metadata/master",
                                    ensureSuffix(directory,fileSep),sep=fileSep),
                      directory="fields/",
                      fileSuffix=".txt",
                      fileSep = "/") {
  r <- list()
  for (field in fieldnames) {
    url <- paste(baseurl, field, fileSuffix, sep="")
    r[[field]] <- as.character((readr::read_csv(url, 
                                                col_names = FALSE, 
                                                locale = readr::locale(),
                                                col_types = c(readr::col_character())))$X1)
  }
  r
}

#' @description \code{enrichDictionary()} enriches an existing dictionary by 
#' addition of similar words, as 
#' measured in a word2vec model. The model can, for instance, be trained with 
#' the package \code{wordVectors}.
#' @param dictionary The base dictionary, a named list of lists.
#' @param model the loaded word2vec model
#' @param top A maximal number of words that we consider 
#' @param minimalSimilarity The minimal similarity for a word in order 
#' to be added
#' @rdname dictionaryHandling
#' @export
#' @examples 
#' \dontrun{
#' # Load base dictionary
#' dict_base <- loadFields(fieldnames=c("Familie","Liebe"))
#' # Load the word2vec model
#' model = wordVectors::read.vectors("models/german-fiction_vectors.bin")
#' # Create a new dictionary with added words
#' dict_enriched <- enrichDictionary(dict_base, model)
#' }
enrichDictionary <- function(dictionary, model, top=100, minimalSimilarity=0.4) {
  r <- dictionary
  for (f in 1:length(dictionary)) {
    fn <- names(dictionary)[[f]]
    sims <- wordVectors::closest_to(model,dictionary[[f]],n=top,fancy_names = FALSE)
    r[[fn]] <- c(r[[fn]],sims[sims$similarity>=minimalSimilarity,1])
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
#' for a single word list. 
#' @param drama A QDDrama object.
#' @param fieldnames A list of names for the dictionaries. 
#' @param fields A list of lists that contains the actual field names. 
#' By default, we load the \code{base_dictionary}.
#' @param normalizeByFigure Logical. Whether to normalize by character 
#' speech length.
#' @param normalizeByField Logical. Whether to normalize by dictionary 
#' size. You usually want this.
#' @param column The table column we apply the dictionary on. 
#' Should be either "Token.surface" or "Token.lemma", the latter is the default.
#' @param ci Whether to ignore case. Defaults to TRUE, i.e., case is ignored.
#' @importFrom stats aggregate ave
#' @importFrom utils as.roman
#' @importFrom data.table as.data.table setcolorder
#' @seealso \code{\link{loadFields}} \code{\link{format.QDHasCharacter}}
#' @rdname dictionaryStatistics
#' @examples
#' # Check multiple dictionary entries
#' data(rksp.0)
#' dstat <- dictionaryStatistics(rksp.0, fieldnames=c("Krieg","Familie"))
#' @export
dictionaryStatistics <- function(drama, fields=base_dictionary[fieldnames],
                                 fieldnames=c("Liebe"),
                                 segment=c("Drama","Act","Scene"),
                                 normalizeByFigure = FALSE, 
                                 normalizeByField = FALSE, 
                                 byFigure = TRUE,
                                 column="Token.lemma", 
                                 ci = TRUE) {
  stopifnot(inherits(drama, "QDDrama"))
  
  
  # we need this to prevent notes in R CMD check
  .N <- NULL
  . <- NULL
  corpus <- NULL
  Speaker.figure_surface <- NULL
  Speaker.figure_id <- NULL
  
  
  segment <- match.arg(segment)
  
  text <- switch(segment,
                 Drama=drama$text,
                 Act=segment(drama$text, drama$segments),
                 Scene=segment(drama$text, drama$segments))
  
  
  bylist <- list(text$corpus, text$drama, text$Speaker.figure_id)
  r <- aggregate(text, by=bylist, length)[,1:3]

  first <- TRUE
  singles <- lapply(names(fields),function(x) {
    dss <- as.data.table(dictionaryStatisticsSingle(drama, fields[[x]], ci=ci,
                                        segment = segment,
                                        byFigure = byFigure,
                                        normalizeByFigure = normalizeByFigure, 
                                        normalizeByField = normalizeByField, 
                                        column=column))
    colnames(dss)[ncol(dss)] <- x
    if (x == names(fields)[[1]]) {
      if (segment=="Scene") {
        u <- unique(text[,c("begin.Scene","Number.Act", "Number.Scene")])
        
        dss <- merge(dss, u, 
                     by.x=c("Number.Act", "Number.Scene"),
                     by.y=c("Number.Act", "Number.Scene"))
        dss$begin.Scene <- NULL
        data.table::setcolorder(dss, c("corpus","drama","Number.Act","Number.Scene","character",x))
      }
      dss
    } else {
      dss[,x,with=FALSE]
    }
  })
  r <- Reduce(cbind,singles)
  class(r) <- c("QDDictionaryStatistics", "QDHasCharacter", switch(segment, 
                                                Drama = "QDByDrama",
                                                Act   = "QDByAct",
                                                Scene ="QDByScene"), "data.frame")
  if (byFigure) 
    class(r) <- append(class(r), "QDByCharacter")
  
  r
}


#' @param wordfield A character vector containing the words or lemmas 
#' to be counted (only for \code{*Single}-functions)
#' @param fieldNormalizer Defaults to the length of the wordfield. 
#' If normalizeByField is given, the absolute numbers are divided 
#' by this number.
#' @param segment The segment level that should be used. By default, 
#' the entire play will be used. Possible values are "Drama" (default), 
#' "Act" or "Scene".
#' @param colnames The column names to be used in the output table.
#' @param byFigure Logical, defaults to TRUE. If false, values will be calculated
#' for the entire segment (play, act, or scene), and not for individual characters.
#' @examples
#' # Check a single dictionary entries
#' data(rksp.0)
#' fstat <- dictionaryStatisticsSingle(rksp.0, wordfield=c("der"))
#' @importFrom stats aggregate
#' @importFrom stats na.omit
#' @importFrom reshape2 melt
#' @importFrom stats as.formula
#' @rdname dictionaryStatistics
#' @export
dictionaryStatisticsSingle <- function(drama, wordfield=c(), 
                                       segment=c("Drama","Act","Scene"),
                                       normalizeByFigure = FALSE, 
                                       normalizeByField = FALSE, 
                                       byFigure = TRUE,
                                       fieldNormalizer = length(wordfield), 
                                       column="Token.lemma", 
                                       ci=TRUE,
                                       colnames=NULL)
  {
  stopifnot(inherits(drama, "QDDrama"))
  
  # we need this to prevent notes in R CMD check
  .N <- NULL
  . <- NULL
  .SD <- NULL
  `:=` <- NULL
  N <- NULL
  value <- NULL

  segment <- match.arg(segment)
  
  text <- switch(segment,
                 Drama=drama$text,
                 Act=segment(drama$text, drama$segments),
                 Scene=segment(drama$text, drama$segments))
  
  bycolumns <- c("corpus",
                 switch(segment,
                        Drama=c("drama"),
                        Act=c("drama","Number.Act"),
                        Scene=c("drama","Number.Act","Number.Scene"))
                 )
  #if (byFigure == TRUE) {
  #  bycolumns <- c(bycolumns, "Speaker.figure_id")
  #}
  bylist <- paste(bycolumns,collapse=",")
  if (ci) {
    wordfield <- tolower(wordfield)
    casing <- tolower
  } else {
    casing <- identity
  }
  if (normalizeByField == FALSE) {
    fieldNormalizer <- 1
  }
  
  dt <- data.table(text)
  dt$match <- casing(dt[[column]]) %in% wordfield
  
  # counting
  # xt <- dt[,.(x=sum(match)),keyby=bylist]
  if (byFigure == TRUE) {
    xt <- dt[,melt(xtabs(~ Speaker.figure_id, data=.SD[match])), 
             keyby=bylist]
  } else {
    xt <- dt[,.(value=sum(match)), keyby=bylist]
  }
  
  if(normalizeByField || normalizeByFigure) {
    xt$value <- as.double(xt$value)
  }
  
  # remove combinations of character and play that don't exist
  if (byFigure == TRUE) {
    xt <- unique(merge(xt, drama$characters, 
                by.x = c("corpus","drama","Speaker.figure_id"), 
                by.y = c("corpus","drama","figure_id"))[,names(xt), with=F])
  }
  # xt$match <- NULL
  
  if (normalizeByFigure == TRUE) {
    if (byFigure == TRUE) {
      bycolumns <- append(bycolumns,"Speaker.figure_id")
    }
    bylist <- paste(bycolumns, collapse=",")
    xt <- merge(xt, dt[,.N,keyby=bylist], 
          by.x = bycolumns,
          by.y = bycolumns)
    xt[,value:=((value/fieldNormalizer)/N), keyby=bylist]
    xt <- xt[,-"N"]
  } else {
    xt$value <- as.double(xt$value) / fieldNormalizer
  }
  
  r <- xt
  colnames(r)[ncol(r)] <- "x"
  if (byFigure) {
    colnames(r)[ncol(r)-1] <- "character"
  }
  if (! is.null(colnames)) {
    colnames(r) <- colnames
  }
  
  r[is.nan(r$x)]$x <- 0
  class(r) <- c("QDDictionaryStatistics", "QDHasCharacter", 
                switch(segment, 
                       Drama = "QDByDrama",
                       Act   = "QDByAct",
                       Scene ="QDByScene"), "data.frame", class(r))
  if (byFigure) {
    class(r) <- append(class(r), "QDByCharacter")
    r$character <- as.factor(r$character)
  }
  r
}




#' @description The function \code{filterByDictionary()} can be used to filter a matrix as produced by 
#' \code{frequencytable()} by the words in the given dictionary(/-ies).
#' @param ft A matrix as produced by \code{frequencytable()}.
#' @param fieldnames A list of names for the dictionaries.
#' @param fields A list of lists that contains the actual field names. 
#' By default, we load the base_dictionary (as in \code{dictionaryStatistics()}).
#' @export
#' @rdname frequencyTable
#' @examples
#' data(rksp.0)
#' filtered <- filterByDictionary(frequencytable(rksp.0, 
#'                                               byFigure = TRUE), 
#'                                               fieldnames=c("Krieg", "Familie"))
#' 

filterByDictionary <- function(ft, 
                           fields=base_dictionary[fieldnames],
                           fieldnames=c("Liebe")) {
  as.matrix(ft[,which(colnames(ft) %in% unlist(fields))])
}

#' @export
#' @rdname dictionaryStatistics
#' @description Extract the number part from a 
#' \code{QDDictionaryStatistics} table as a matrix 
#' @param x An object of the type \code{QDDictionaryStatistics}, 
#' e.g., the output of \code{dictionaryStatistics}.
#' @param ... All other parameters are passed to \code{as.matrix.data.frame()}.
#' @return A numeric matrix that contains the frequency with which 
#' a dictionary is present in a subset of tokens
#' @examples
#' mat <- as.matrix(dictionaryStatistics(rksp.0, fieldnames=c("Krieg","Familie")))
as.matrix.QDDictionaryStatistics <- function (x, ...) {
  stopifnot(inherits(x, "QDDictionaryStatistics"))
  
  # check if there is a column for the character
  if (inherits(x, "QDByCharacter")) {
    byCharacter <- TRUE
  } else {
    byCharacter <- FALSE
  }
  
  # check how many segment columns there are
  if (inherits(x, "QDByDrama")) {
    segment <- "Drama"
    metaCols <- 1:(3+byCharacter)
  } else if (inherits(x, "QDByAct")) {
    segment <- "Act"
    metaCols <- 1:(4+byCharacter)
  } else if (inherits(x, "QDByScene")) {
    segment <- "Scene"
    metaCols <- 1:(5+byCharacter)
  }
  
  as.matrix.data.frame(x[,max(metaCols):ncol(x)])
}

