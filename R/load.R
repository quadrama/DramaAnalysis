
#' This function initialises the paths to data files.
#' @param dataDirectory A path to the directory in which data and metadata are located. 
#' "~/QuaDramA/Data2" by default.
#' @param collectionDirectory A path to the directory in which collections are stored. 
#' By default, the directory is called "collection" below the data directory.
#' @export
setup <- function(dataDirectory = file.path(path.expand("~"),"QuaDramA","Data2"), 
                  collectionDirectory = file.path(dataDirectory,"collections")) {
  message("Since 2.1 it is no longer necessary to call setup() if you're happy with the default paths.")
  options(qd.datadir=dataDirectory)
  options(qd.collectionDirectory=collectionDirectory)
}

#' @export
#' @rdname setup
setDataDirectory <- function(dataDirectory = file.path(path.expand("~"),"QuaDramA","Data2")) {
  options(qd.datadir=dataDirectory)
}

#' @export
#' @rdname setup
setCollectionDirectory <- function(collectionDirectory = file.path(getOption("qd.datadir"), "collections")) {
  options(qd.collectionDirectory=collectionDirectory)
}

#' @importFrom utils read.table
loadSetsInternally <- function() {
  setNames <- list.files(getOption("qd.collectionDirectory"))
  sets <- lapply(setNames, 
                 function(x) { 
                   utils::read.table(
                     file.path(getOption("qd.collectionDirectory"), x),
                     encoding = "UTF-8",
                     stringsAsFactors = FALSE
                   )$V1
                 })
  names(sets) <- setNames
  sets
}

#' @title Load Collections
#' @description Function to load a set from collection files
#' Can optionally set the set name as a genre in the returned table. 
#' \code{loadSets()} returns table of all defined collections (and the
#' number of plays in each).
#' @param setName A character vector. The name of the set(s) to retrieve.
#' @param addGenreColumn Logical. Whether to set the Genre-column in 
#' the returned table to the set name. If set to FALSE (default), a vector
#' is returned. In this case, association to collections is not returned.
#' Otherwise, it's a data.frame.
#' @export
loadSet <- function(setName, addGenreColumn=FALSE) {
  sets <- loadSetsInternally()
  s <- sets[setName]
  if (addGenreColumn == TRUE) {
    Reduce(rbind,
           mapply(function(x,y) { data.frame(id=x, Genre=rep(y,length(x))) },
                  x=s, 
                  y=names(s),
                  SIMPLIFY = FALSE)
           )
  } else {
    Reduce(c,s)
  }
}

#' @export
#' @rdname loadSet
loadSets <- function() {
  sets <- loadSetsInternally()
  data.frame(size=unlist(lapply(sets,length)))
}

scene.act.table <- function(ids, defaultCollection="tg") {
  merged <- loadCSV(ids, "Segments", defaultCollection = defaultCollection)
  merged$Number.Act <- as.numeric(as.factor(data.table::frank(merged$begin.Act, ties.method = "min")))
  merged$Number.Scene <- stats::ave(merged$begin.Scene, 
                                    merged$drama, merged$Number.Act, 
                                    FUN=function(x) {as.numeric(as.factor(x))})
  
  merged
}

#' @title Text Loading
#' @description Loads a text with its segmentation
#' @param ids The ids for which we want to get the text
#' @importFrom data.table setkey foverlaps data.table
#' @param defaultCollection The collection prefix is added if no prefix is found
#' @export
#' @examples 
#' \dontrun{
#' installData("test")
#' mtext <- loadSegmentedText("test:rksp.0")
#' }
loadSegmentedText <- function(ids,defaultCollection="tg") {
  t <- loadText(ids, includeTokens=TRUE, defaultCollection=defaultCollection)
  sat <- scene.act.table(ids=ids, defaultCollection=defaultCollection)
  
  # prevent notes in R CMD check
  begin.Scene <- NULL
  `:=` <- NULL
  begin.Act <- NULL
  end.Scene <- NULL
  end.Act <- NULL
  Number.Scene <- NULL

  # if scene begin/end field is NA, we replace it with the act begin/end
  # therefore, we don't loose any text
  sat[is.na(begin.Scene),  `:=`(begin.Scene  = begin.Act),]
  sat[is.na(end.Scene),    `:=`(end.Scene    = end.Act),]
  sat[is.na(Number.Scene), `:=`(Number.Scene = 0),]
  
  data.table::setkey(t, "corpus", "drama", "begin", "end")
  data.table::setkey(sat, "corpus", "drama", "begin.Scene", "end.Scene")
  mtext <- data.table::foverlaps(t, sat, type="any",
                                 by.x=c("corpus", "drama", "begin", "end"), 
                                 by.y=c("corpus", "drama", "begin.Scene", "end.Scene"))
  mtext
}


## @param ids A vector containing drama ids to be downloaded
## @param includeTokens This argument has no meaning anymore. Tokens are always included.
## @param defaultCollection The collection prefix is added if no prefix is found
## @param unifyCharacterFactors Logical value, defaults to TRUE. Controls whether columns 
## representing characters (i.e., Speaker.* and Mentioned.*) are sharing factor levels
## @keywords internal
loadText <- function(ids, includeTokens=FALSE, defaultCollection="tg", unifyCharacterFactors=TRUE) {
  t <- loadCSV(ids, defaultCollection = defaultCollection)
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
  }
  t
}


#' @title Load annotations
#' @description Helper method to load covered annotations. Returns a data.table.
#' @param ids A vector or list of drama ids
#' @param type The annotation type to load
#' @param coveredType The annotation type of covered annotations we want to load
#' @param columnTypes Can be used to specify column types, which are passed to readr::read.csv.
#' @param defaultCollection The collection prefix is added if no prefix is found
#' @export
#' @importFrom readr read_csv locale cols
#' @examples
#' \dontrun{
#' loadAnnotations(c("tg:rksp.0"))
#' }
loadAnnotations <- function(ids, 
                            type=NULL, 
                            coveredType=NULL,
                            defaultCollection="tg",
                            columnTypes=NULL) {
  stop("This function is no longer supported. Use loadCSV() instead.")
}

#' @title Character data loading
#' @description Loads a table of characters and meta data
#' @param ids a list or vector of ids
#' @param defaultCollection the default collection
#' @param dataDirectory the data directory
#' @export
loadCharacters <- function(ids, 
                           defaultCollection="tg", 
                           dataDirectory=getOption("qd.datadir")) {
  loadCSV(ids, 
          variant="Characters", 
          defaultCollection = defaultCollection, 
          dataDirectory = dataDirectory)
}

loadCSV <- function(ids, 
                    variant=c("UtterancesWithTokens", "Segments", "Metadata", "Characters"), 
                    defaultCollection="tg",
                    dataDirectory=getOption("qd.datadir")) {
  
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
                                                    col_types = readr::cols()))
      return(tab)
    } else {
      message(paste(filename, "could not be loaded and was skipped"))
      return(NA)
    }
  })
  tables <- tables[!is.na(tables)]
  Reduce(rbind, tables)
}

#' @title Load meta data
#' @description helper method to load meta data about dramatic texts (E.g., author, year)
#' @param ids A vector or list of drama ids
#' @param type The annotation type to load. No longer used.
#' @export
loadMeta <- function(ids,type=NULL) {
  loadCSV(ids, variant="Metadata")
}


#' @title Installed texts
#' @description Returns a list of all ids that are installed
#' @param asDataFrame Logical value. Controls whether 
#' the return value is a list (with colon-joined ids) 
#' or a data.frame with two columns (corpus, drama)
#' @param dataDirectory The directory in which precompiled 
#' drama data is installed
#' @export
#' 
loadAllInstalledIds <- function(asDataFrame=FALSE, 
                                dataDirectory=getOption("qd.datadir")) {
  files <- list.files(path=file.path(dataDirectory),pattern=".*\\.(csv|xmi)", recursive = TRUE)
  files <- strsplit(files, .Platform$file.sep, fixed=TRUE)
  files <- lapply(files, function(x) {
    parts <- unlist(strsplit(x[3],".",fixed=TRUE))
    if (data.table::last(parts)=="xmi") {
      x[3] <- paste(parts[1:(length(parts)-1)],sep=".",collapse=".")
    } else if (data.table::last(parts)=="csv") {
      x[3] <- paste(parts[1:(length(parts)-2)],sep=".",collapse=".")
    }
    x
  })
  files <- unique(files)
  if (asDataFrame) {
    data.frame(matrix(unlist(files), nrow=length(files), byrow=T))
  } else {
    unlist(lapply(files, function(x) { paste(x[c(1,3)],sep=":", collapse=":") }))
  }
}

