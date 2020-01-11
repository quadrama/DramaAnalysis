#' This function initializes the paths to data files.
#' @param dataDirectory A path to the directory in which data and metadata are located. 
#' "~/QuaDramA/Data2" by default.
#' @param collectionDirectory A path to the directory in which collections are stored. 
#' By default, the directory is called "collection" below the data directory.
#' @return The \code{set*Directory()} functions always return \code{NULL}.
#' @rdname setup
#' @export
setDirectories <- function(dataDirectory = file.path(path.expand("~"),"QuaDramA","Data2"), 
                  collectionDirectory = file.path(dataDirectory,"collections")) {
  options(qd.datadir=dataDirectory)
  options(qd.collectionDirectory=collectionDirectory)
  NULL
}

#' @export
#' @rdname setup
setDataDirectory <- function(dataDirectory = file.path(path.expand("~"),"QuaDramA","Data2")) {
  options(qd.datadir=dataDirectory)
  NULL
}


#' @title Load drama
#' @description This function loads one or more of the installed plays and 
#' returns them as a \code{QDDrama} object.
#' @return The function returns a \code{QDDrama} object. This is essentially a 
#' list of \code{data.table}s, covering the different aspects (utterances, segments, 
#' characters, ...). If multiple ids have been supplied as arguments, the tables 
#' contain the information of multiple plays.
#' @export
#' @param ids A vector of ids.
#' @param defaultCollection If the ids do not have a collection prefix, the 
#' defaultCollection prefix is applied.
#' @rdname loadDrama
#' @exportClass QDDrama
#' @examples 
#' # both are equivalent
#' \donttest{
#' d <- loadDrama(c("test:rksp.0", "test:rjmw.0"))
#' d <- loadDrama(c("rksp.0", "rjmw.0"), defaultCollection = "test")
#' }
loadDrama <- function(ids, defaultCollection="qd") {
  drama <- list()
  drama$text     <- loadText(ids, defaultCollection = defaultCollection)
  ids <- unique(paste(drama$text$corpus, drama$text$drama, sep = ":"))
  drama$meta     <-   loadCSV(ids, 
                              variant="Metadata", 
                              defaultCollection = defaultCollection)
  
  # replace zero dates with NA
  drama$meta$Date.Written <-  ifelse(drama$meta$Date.Written == 0, NA, drama$meta$Date.Written)
  drama$meta$Date.Printed <-  ifelse(drama$meta$Date.Printed == 0, NA, drama$meta$Date.Printed)
  drama$meta$Date.Premiere <- ifelse(drama$meta$Date.Premiere == 0, NA, drama$meta$Date.Premiere)
  drama$meta$Date.Translation <- ifelse(drama$meta$Date.Translation == 0, NA, 
                                        drama$meta$Date.Translation)
  
  drama$segments <- loadSegments(ids, defaultCollection = defaultCollection)
  drama$mentions <- loadMentions(ids, defaultCollection = defaultCollection)
  drama$characters <- loadCSV(ids, 
                              variant="Characters", 
                              defaultCollection = defaultCollection)
  drama$stageDirections <- loadText(ids,
                                   variant="StageDirections",
                                   defaultCollection = defaultCollection)
  
  if (sum(is.na(drama$text$Speaker.figure_id)) > 0) {
    warning(paste(sum(is.na(drama$text$Speaker.figure_id)), "spoken words are not assigned to a character (NA values). They have been removed to prevent subsequent issues."))
    nas <- which(is.na(drama$text$Speaker.figure_id))
    drama$text <- drama$text[-nas,]
  }
  class(drama) <- append("QDDrama", class(drama))
  drama
}



#' @exportClass QDHasUtteranceBE
loadMentions <- function(ids, defaultCollection="qd") {
  mentionsTable <- loadCSV(ids, 
                           defaultCollection = defaultCollection, 
                           variant = "Mentions")
  class(mentionsTable) <- append("QDHasUtteranceBE", class(mentionsTable))
  mentionsTable
}

loadSegments <- function(ids, defaultCollection="qd") {
  # prevent notes in check
  Number.Act <- NULL
  corpus <- NULL
  drama <- NULL
  Number.Scene <- NULL
  begin.Act <- NULL
  begin.Scene <- NULL
  . <- NULL
  `:=` <- NULL
  
  merged <- loadCSV(ids, "Segments", defaultCollection = defaultCollection)
  merged[,Number.Act:=as.numeric(as.factor(data.table::frank(begin.Act, ties.method = "min"))),
         .(corpus,drama)]
  merged[,Number.Scene:=as.numeric(as.factor(data.table::frank(begin.Scene, ties.method = "min"))),
         .(corpus,drama,Number.Act)]
  
  sat <- merged
  class(sat) <- append("QDHasSegments", class(sat))
  sat
}


#' @title Load Text
#' @param ids A vector containing drama ids to be downloaded
#' @param includeTokens This argument has no meaning anymore. Tokens are always included.
#' @param defaultCollection The collection prefix is added if no prefix is found
#' @param variant The file variant to load
#' @param unifyCharacterFactors Logical value, defaults to TRUE. Controls whether columns 
#' representing characters (i.e., Speaker.* and Mentioned.*) are sharing factor levels
#' @return a data.frame that is also of class \code{QDHasUtteranceBE}.
loadText <- function(ids, includeTokens=FALSE, defaultCollection="tg", 
                     unifyCharacterFactors=FALSE, variant="UtterancesWithTokens") {
  t <- loadCSV(ids, defaultCollection = defaultCollection, variant=variant)
  colnames(t)[3:4] <- c("utteranceBegin", "utteranceEnd") 
  t$Token.pos <- factor(t$Token.pos)

  
  
  if (unifyCharacterFactors) {
    # Handling character factors
    # ids
    allids <- union(levels(factor(t$Speaker.figure_id)),
                    levels(factor(t$Mentioned.figure_id)))
    t$Speaker.figure_id <- factor(t$Speaker.figure_id, levels=allids)
    t$Mentioned.figure_id <- factor(t$Mentioned.figure_id, levels=allids)
    # names
    allids <- union(levels(factor(t$Speaker.figure_surface)),
                    levels(factor(t$Mentioned.figure_surface)))
    t$Speaker.figure_surface <- factor(t$Speaker.figure_surface, levels=allids)
    t$Mentioned.figure_surface <- factor(t$Mentioned.figure_surface, levels=allids)
  } else {
    t$Speaker.figure_id <- factor(t$Speaker.figure_id)
    t$Mentioned.figure_id <- factor(t$Mentioned.figure_id)
    t$Speaker.figure_surface <- factor(t$Speaker.figure_surface)
    t$Mentioned.figure_surface <- factor(t$Mentioned.figure_surface)
  }
  class(t) <- append("QDHasUtteranceBE", class(t))
  t
}


#' @title Character data loading
#' @description Loads a table of characters and meta data
#' @param ids a list or vector of ids
#' @param defaultCollection the default collection
#' @param dataDirectory the data directory
#' @return A data.frame extracted from the CSV file about characters
loadCharacters <- function(ids, 
                           defaultCollection="tg", 
                           dataDirectory=getOption("qd.datadir")) {
  loadCSV(ids, 
          variant="Characters", 
          defaultCollection = defaultCollection, 
          dataDirectory = dataDirectory)
}

loadCSV <- function(ids, 
                    variant=c("UtterancesWithTokens", 
                              "Segments", "Metadata", 
                              "Characters", "Mentions", 
                              "StageDirections"), 
                    defaultCollection="tg",
                    dataDirectory=getOption("qd.datadir"),
                    classes=c()) {
  
  ids <- unlist(lapply(strsplit(as.character(ids),":",fixed=TRUE),
                       function(x) { paste(c(rep(defaultCollection,2-length(x)),x),sep="",collapse=":") } ))
  splittedIds <- strsplit(ids,":",fixed=TRUE)
  cvar <- match.arg(variant)
  
  tables <- lapply(splittedIds, function(x) {
    filename <- file.path(dataDirectory,
                          x[1],
                          "csv",
                          paste(x[2],cvar,"csv",
                                sep="."))
    if (file.exists(filename)) {
      tab <- data.table::data.table(readr::read_csv(filename, 
                                                    locale = readr::locale(encoding = "UTF-8"),
                                                    col_types = readr::cols(drama = readr::col_character())))
      if (nrow(tab) == 0)
        message(paste("Table",cvar,"of",x[2],"is empty."))
      tab
    } else {
      message(paste(filename, "could not be loaded and was skipped"))
      NA
    }
  })
  tables <- tables[!is.na(tables)]
  r <- Reduce(rbind, tables)
  if (length(classes)>0) {
    class(r) <- append(class(r), classes)
  }
  
  r
}

#' @title Load meta data
#' @description helper method to load meta data about dramatic texts (E.g., author, year). 
#' Does not load the texts, so it's much faster.
#' @param ids A vector or list of drama ids
#' @return a data frame
#' @export
loadMeta <- function(ids) {
  loadCSV(ids, variant="Metadata")
}


