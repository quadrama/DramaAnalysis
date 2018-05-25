

#' @title Dictionary Handling
#' @description \code{loadFields()} loads dictionaries that are available on the web as plain text files.
#' @param fieldnames A list of names for the dictionaries. It is expected that files with that name can be found below the URL.
#' @param baseurl The base path delivering the dictionaries. Should end in a /, field names will be appended and fed into read.csv().
#' @param fileSuffix The suffix for the dictionary files
#' @param directory The last component of the base url. 
#' Useful to retrieve enriched word fields from metadata repo.
#' @param fileSep The file separator used to construct the URL
#' Can be overwritten to load local dictionaries.
#' @importFrom utils read.csv
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
    r[[field]] <- as.character((read.csv(url, header=F, fileEncoding = "UTF-8"))$V1)
  }
  r
}

#' @description \code{enrichDictionary()} enriches an existing dictionary by addition of similar words, as 
#' measured in a word2vec model.
#' @param dictionary The base dictionary, a named list of lists.
#' @param model the loaded word2vec model
#' @param top A maximal number of words that we consider 
#' @param minimalSimilarity The minimal similarity for a word in order 
#' to be added
#' @importFrom wordVectors closest_to
#' @rdname dictionaryHandling
#' @export
#' @examples 
#' \dontrun{
#' # Load base dictionary
#' dict_base <- loadFields(fieldnames=c("Familie","Liebe"))
#' # Load the word2vec model
#' model = read.vectors("models/german-fiction_vectors.bin")
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
#' @param asList Logical. Whether to return a list with separated components or a single data.frame.
#' @importFrom stats aggregate
#' @importFrom stats ave
#' @importFrom utils as.roman
#' @seealso \code{\link{loadFields}}
#' @rdname dictionaryStatistics
#' @examples
#' \dontrun{
#' # Check multiple dictionary entries
#' data(rksp.0)
#' dstat <- dictionaryStatistics(rksp.0$mtext, fieldnames=c("Krieg","Familie"), names=TRUE)
#' }
#' @export
dictionaryStatistics <- function(t, fields=loadFields(fieldnames,baseurl),
                                 fieldnames=c("Liebe"),
                                 segment=c("Drama","Act","Scene"),
                                 normalizeByFigure = FALSE, 
                                 normalizeByField = FALSE, 
                                 byFigure = TRUE,
                                 names = FALSE, 
                                 boost = 1,
                                 baseurl = "https://raw.githubusercontent.com/quadrama/metadata/master/fields/",
                                 column="Token.surface", 
                                 asList = FALSE,
                                 ci = TRUE) {
  
  # we need this to prevent notes in R CMD check
  .N <- NULL
  . <- NULL
  corpus <- NULL
  drama <- NULL
  Speaker.figure_surface <- NULL
  Speaker.figure_id <- NULL
  
  
  segment <- match.arg(segment)
  
  bylist <- list(t$corpus, t$drama, t$Speaker.figure_id)
  if (names == TRUE)
    bylist <- list(t$corpus, t$drama, t$Speaker.figure_surface)
  r <- aggregate(t, by=bylist, length)[,1:3]
  
  first <- TRUE
  singles <- lapply(names(fields),function(x) {
    dss <- dictionaryStatisticsSingle(t, fields[[x]], ci=ci,
                                        segment=segment,
                                        byFigure = byFigure,
                                        normalizeByFigure = normalizeByFigure, 
                                        normalizeByField = normalizeByField, 
                                        names=names, column=column)
    colnames(dss)[ncol(dss)] <- x
    if (x == names(fields)[[1]]) {
      if (segment=="Scene") {
        u <- unique(t[,c("begin.Scene","Number.Act", "Number.Scene")])
        dss <- merge(dss, u, 
                     by.x="begin.Scene",
                     by.y="begin.Scene")
        dss$begin.Scene <- NULL
        data.table::setcolorder(dss, c("corpus","drama","Number.Act","Number.Scene","figure",x))
      }
      dss
    } else {
      dss[,x,with=FALSE]
    }
  })
  r <- Reduce(cbind,singles)

  
  
  if (FALSE==TRUE && normalizeByFigure == TRUE) {
    if (names == TRUE) {
      tokens <- t[,.N,
                  .(corpus,drama,Speaker.figure_surface)]
    } else {
      tokens <- t[,.N,
                  .(corpus,drama,Speaker.figure_id)]
    }
    r <- merge(r,tokens,
               by.x=c("corpus","drama","figure"),
               by.y=c("corpus","drama",ifelse(names==TRUE,"Speaker.figure_surface","Speaker.figure_id")),
               allow.cartesian = TRUE)
    r[,(ncol(r)-length(fieldnames)):ncol(r)] <- r[,(ncol(r)-length(fieldnames)):ncol(r)] / r$N
    r$N <- NULL
  }
  
  if (asList == TRUE) {
    l <- as.list(r[,1:switch(segment,Drama=3,Act=4,Scene=5)])
    l$mat <- as.matrix(r[,(ncol(r)-length(fields)+1):ncol(r)])
    rownames(l$mat) <- switch(segment, 
                              Drama=as.character(l$figure),
                              Act=paste(l$figure,utils::as.roman(l$Number.Act)),
                              Scene=paste(l$figure,l$begin.Scene))
    l
  } else {
    r
  }
}

#' @param wordfield A character vector containing the words or lemmas 
#' to be counted (only for \code{*Single}-functions)
#' @param fieldNormalizer defaults to the length of the wordfield
#' @param segment The segment level that should be used. By default, 
#' the entire play will be used. Possible values are "Drama" (default), 
#' "Act" or "Scene"
#' @param colnames The column names to be used
#' @param byFigure Logical, defaults to TRUE. If false, values will be calculated
#' for the entire segment (play, act, or scene), and not for individual characters.
#' @examples
#' # Check a single dictionary entries
#' data(rksp.0)
#' fstat <- dictionaryStatisticsSingle(rksp.0$mtext, wordfield=c("der"), names=TRUE)
#' @importFrom stats aggregate
#' @importFrom stats na.omit
#' @importFrom reshape2 melt
#' @importFrom stats as.formula
#' @rdname dictionaryStatistics
#' @export
dictionaryStatisticsSingle <- function(t, wordfield=c(), 
                                       names = FALSE, 
                                       segment=c("Drama","Act","Scene"),
                                       normalizeByFigure = FALSE, 
                                       normalizeByField = FALSE, 
                                       byFigure = TRUE,
                                       fieldNormalizer=length(wordfield), 
                                       column="Token.surface", ci=TRUE,
                                       colnames=NULL)
  {
  # we need this to prevent notes in R CMD check
  .N <- NULL
  . <- NULL
  .SD <- NULL
  
  segment <- match.arg(segment)
  bycolumns <- c("corpus",
                 switch(segment,
                        Drama=c("drama"),
                        Act=c("drama","Number.Act"),
                        Scene=c("drama","begin.Scene"))
                 )
  if (byFigure == TRUE) {
    bycolumns <- c(bycolumns, ifelse(names==TRUE,
                                     "Speaker.figure_surface",
                                     "Speaker.figure_id"))
  }
  
  bylist <- paste(bycolumns,collapse=",")
  dt <- as.data.table(t)
  if (ci) {
    wordfield <- tolower(wordfield)
    casing <- tolower
  } else {
    casing <- identity
  }
  if (normalizeByField == FALSE) {
    fieldNormalizer <- 1
  }
  
  dt$match <- casing(dt[[column]]) %in% wordfield
  form <- stats::as.formula(paste0("~ ", paste(c(bycolumns,"match"), collapse=" + ")))
  xt <- data.table::data.table(reshape2::melt(xtabs(form, data=dt)))
  if (normalizeByFigure == TRUE) {
    r <- xt[,.((sum(.SD[match==TRUE]$value)/fieldNormalizer)/sum(.SD$value)),
       keyby=bylist]
  } else {
    r <- xt[,.(sum(.SD[match==TRUE]$value)/fieldNormalizer),
            keyby=bylist]
  }
  
  colnames(r)[ncol(r)] <- "x"
  colnames(r)[ncol(r)-1] <- "figure"
  if (! is.null(colnames)) {
    colnames(r) <- colnames
  }
  
  r[is.nan(r$x)]$x <- 0
  r
}

dictionaryStatisticsSingleL <- function(...) {
  dstat <- dictionaryStatisticsSingle(...)
  as.list(dstat)
}

#' @description \code{dictionaryStatisticsL()} should not be used 
#' anymore. Please use \code{dictionaryStatistics()} with the parameter
#' \code{asList=TRUE}
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
  .Deprecated("dictionaryStatistics")
  dictionaryStatistics(..., asList=TRUE)
}


dictionary.statistics <- function(...) {
  .Deprecated("dictionaryStatistics")
  dictionaryStatistics(...)
}

#' @title regroup
#' @description This function isolates the dictionary statistics for
#' each character. The return value is a list containing lists similar
#' to the output of `dictionaryStatistics()`, but only containing 
#' the table for one character.
#' @param dstat A list generated by `dictionaryStatistics()`, 
#' using the `asList` parameter
#' @param by A character vector, either "Character" or "Field".
#' Depending on this parameter, we get a list organized by character 
#' or a list organized by field. If it's organised by character, it allows
#' comparison of fields for a single character. If organised by field, 
#' we can compare different characters for a single field.
#' @export
#' @examples
#' data(rksp.0)
#' field <- list(Liebe=c("liebe","lieben","herz"))
#' dsl <- dictionaryStatistics(rksp.0$mtext, 
#'    fields=field,
#'    normalizeByFigure=TRUE,
#'    asList=TRUE,
#'    segment="Scene")
#' dslr <- regroup(dsl, by="Field")
#' \dontrun{
#' matplot(apply(dslr$Liebe, 1, cumsum),type="l", main="Liebe", col=rainbow(14))
#' legend(x="topleft", legend=rownames(dslr$Liebe),lty=1:5,col=rainbow(14), cex = 0.4)
#' }
regroup <- function(dstat, by=c("Character","Field")) {
  by = match.arg(by)
  switch(by,
         Character={
          l <- lapply(levels(dstat$figure), function(x) {
          myLines = which(dstat$figure == x)
    
          innerList <- list()
          innerList$mat <- dstat$mat[myLines,]
          if ("Number.Scene" %in% names(dstat)) {
            innerList$Number.Scene <- dstat$Number.Scene[myLines]
          }
          if ("Number.Act" %in% names(dstat)) {
            innerList$Number.Act <- dstat$Number.Act[myLines]
          }
          innerList
        })
        names(l) <- levels(dstat$figure)
        return(l)
        },
        Field={
          l <- lapply(colnames(dstat$mat), function(x) {
            df <- data.frame(Field=dstat$mat[,x])
            df$figure <- dstat$figure
            if ("Number.Scene" %in% names(dstat)) {
              df$Number.Scene <- dstat$Number.Scene
            }
            if ("Number.Act" %in% names(dstat)) {
              df$Number.Act <- dstat$Number.Act
            }
            if ("Number.Act" %in% names(dstat) && "Number.Scene" %in% names(dstat)) {
              df$Segment <- paste(as.roman(df$Number.Act), df$Number.Scene)
            }
            
            df2 <- reshape(df, direction="wide", timevar=c("Segment"), idvar=c("figure"),drop=c("Number.Act","Number.Scene"))
            rownames(df2) <- df2$figure
            df2$figure <- NULL
            colnames(df2) <- substr(colnames(df2), 7, 100)
            df2
          })
          names(l) <- colnames(dstat$mat)
          return(l)
        });
}