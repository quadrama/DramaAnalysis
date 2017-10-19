
#' This function initialises the import from XMI files.
#' @param dataDirectory A path to the directory in which data and metadata are located. "~/QuaDramA/Data" by default
#' @export
setup <- function(dataDirectory = file.path(path.expand("~"),"QuaDramA","Data")) {
  options(qd.datadir=dataDirectory)
  options(qd.dl=rJava::.jnew("de/unistuttgart/ims/drama/data/DataLoader",dataDirectory))
}

dlobject <- function() {
  if (is.null(getOption("qd.dl"))) {
    options(qd.dl=rJava::.jnew("de/unistuttgart/ims/drama/data/DataLoader",getOption("qd.datadir")))
  }
  getOption("qd.dl")
}

#' Function to load a set from QuaDramA web service.
#' Can optionally set the set name as a genre in the returned table
#' @param setName The name of the set to retrieve
#' @param addGenreColumn Whether to set the Genre-column in the returned table to the set name
#' @export
loadSet <- function(setName, addGenreColumn=FALSE) {
  dl <- dlobject()
  ds <- data.frame(id=dl$getCollectionEntries(setName))
  if (addGenreColumn == TRUE) {
    ds$Genre <- setName
  }
  ds
}

#' A function to get a list of all collections and the number of plays in that collection
#' @export
#' @importFrom rJava .jevalArray .jsimplify
loadSets <- function() {
  dl <- dlobject()
  s <- dl$getListOfSets()
  l <- unlist(lapply(rJava::.jevalArray(s[[2]]),FUN=function(x) {rJava::.jsimplify(x)}))
  data.frame(id=rJava::.jevalArray(s[[1]]),size=l)
}

scene.act.table <- function(ids, defaultCollection="tg") {
  
  # prevent notes in R CMD check
  Number <- NULL
  Number.Act <- NULL
  begin <- NULL
  drama <- NULL
  
  `:=` <- NULL
  
  acts <- loadAnnotations(ids,type=atypes$Act, coveredType=NULL, defaultCollection = defaultCollection)
  
  acts[, Number.Act := as.integer(as.numeric(as.factor(begin))), drama]
  
  #acts$Number <- ave(acts$begin, acts$drama, FUN=function(x) {as.numeric(as.factor(x))})
  scenes <- loadAnnotations(ids,type=atypes$Scene,coveredType = NULL, defaultCollection = defaultCollection)
  merged <- merge(acts, scenes, by=c("drama","corpus"), 
                  suffixes=c(".Act", ".Scene"), 
                  allow.cartesian = TRUE)
  merged <- merged[merged$begin.Act <= merged$begin.Scene & merged$end.Act >= merged$end.Scene,]
  #merged <- subset(merged, select=c(-5,-9))
  merged$Number.Scene <- stats::ave(merged$begin.Scene, 
                                    merged$drama, merged$Number.Act, 
                                    FUN=function(x) {as.numeric(as.factor(x))})
  merged
}

#' Similar to load.text(), but the table also includes scene and act markings.
#' @param ids The ids for which we want to get the text
#' @importFrom data.table setkey foverlaps data.table
#' @param defaultCollection The collection prefix is added if no prefix is found
#' @export
#' @examples 
#' \dontrun{
#' mtext <- loadSegmentedText("tg:rksp.0")
#' }
loadSegmentedText <- function(ids,defaultCollection="tg") {
  t <- data.table::data.table(loadText(ids, includeTokens=TRUE, defaultCollection=defaultCollection))
  sat <- data.table::data.table(scene.act.table(ids=ids, defaultCollection=defaultCollection))
  data.table::setkey(t, "corpus", "drama", "begin", "end")
  data.table::setkey(sat, "corpus", "drama", "begin.Scene", "end.Scene")
  mtext <- data.table::foverlaps(t, sat, type="any",
                                 by.x=c("corpus", "drama", "begin", "end"), 
                                 by.y=c("corpus", "drama", "begin.Scene", "end.Scene"))
  mtext
}


load.text <- function(...) {
  .Deprecated("loadText")
  loadText(...)
}

load.text2 <- function(...) {
  .Deprecated("loadSegmentedText")
  loadSegmentedText(...)
}


#' Loads a CSV-formatted text from the server,
#' assuming the main server url has been set correctly.
#'
#'
#' @param ids A vector containing drama ids to be downloaded
#' @param includeTokens If set to true, the table also contains each token in an utterance
#' @param defaultCollection The collection prefix is added if no prefix is found
#' @export
loadText <- function(ids, includeTokens=FALSE, defaultCollection="tg") {
  `:=` <- NULL
  .N <- NULL
  corpus <- NULL
  drama <- NULL
  if (includeTokens == TRUE) {
    r <- loadAnnotations(ids, 
                     type=atypes$Utterance, 
                     coveredType=atypes$Token,
                     defaultCollection=defaultCollection,
                     columnTypes = "cciiccccc")
    
  } else
    r <- loadAnnotations(ids, 
                         type=atypes$Utterance, 
                         coveredType=NULL,
                         defaultCollection=defaultCollection,
                         columnTypes = "cciicci")
  r$Speaker.figure_surface <- factor(r$Speaker.figure_surface)
  r[, length:=.N, by=list(corpus,drama) ][]
  r
}


#' @title Load annotations
#' @description Helper method to load covered annotations. Returns a data.table.
#' @param ids A vector or list of drama ids
#' @param type The annotation type to load
#' @param coveredType The annotation type of covered annotations we want to load
#' @param columnTypes Can be used to specify column types, which are passed to readr::read.csv.
#' @param defaultCollection The collection prefix is added if no prefix is found
#' @export
#' @importFrom data.table fread
#' @importFrom rJava .jnew .jarray .jnull
#' @importFrom readr read_csv locale
#' @examples
#' \dontrun{
#' loadAnnotations(c("tg:rksp.0"))
#' }
loadAnnotations <- function(ids, 
                            type=atypes$Utterance, 
                            coveredType=atypes$Token,
                            defaultCollection="tg",
                            columnTypes=NULL) {
  dl <- dlobject()
  
  ids <- unlist(lapply(strsplit(as.character(ids),":",fixed=TRUE),
                function(x) { paste(c(rep(defaultCollection,2-length(x)),x),sep="",collapse=":") } ))
  if (is.null(coveredType)) {
    s <- dl$getAnnotations(rJava::.jarray(as.character(ids)),type,rJava::.jnull())
  } else {
    s <- dl$getAnnotations(rJava::.jarray(as.character(ids)),type,coveredType)
  }
  df <- data.table::data.table(readr::read_csv(s, locale = readr::locale(encoding = "UTF-8"),
                                                  col_types = columnTypes))
  colnames(df) <- make.names(colnames(df))
  df
}

#' @title Load meta data
#' @description helper method to load meta data about dramatic texts (E.g., author, year)
#' @param ids A vector or list of drama ids
#' @param type The annotation type to load
#' @export
loadMeta <- function(ids,type=atypes$Author) {
  dl <- dlobject()
  s <- dl$getDramaMetaData(rJava::.jarray(as.character(ids)))
  df <- data.table::data.table(readr::read_csv(s, locale = readr::locale(encoding = "UTF-8"),
                                               col_types = NULL))
  colnames(df) <- make.names(colnames(df))
  df
}

#' Function to count the annotations of a certain type in selected texts.
#' @param ids A vector or list of drama ids
#' @param type A string, the fully qualified type name we want to count
#' @param debug Logical value, whether to print debug information
#' @param shortname Logical value, whether to only use the local name of type in the returned data frame.
#' @export
countAnnotations <- function(ids, 
                             type=atypes$Utterance,
                             debug=FALSE,
                             shortname=TRUE) {
  r <- data.frame(c())
  s <- ""
  if (shortname == TRUE) {
    lname <- utils::tail(unlist(strsplit(type, split=".",fixed=TRUE)),n=1)
  } else {
    lname <- type
  }
  for (a in ids) {
    tryCatch({
      data2 <- loadAnnotations(ids,type,NULL)
      r[a,lname] = nrow(data2)
    }, finally=function(w) {print()}, error=function(w){}, warning=function(w){})
  }
  r
}

#' This function loads the number of annotations of different types for several 
#' dramas at once.
#' @param ids A vector containing drama ids
#' @param types A character vector containing the annotation types we want to count
#' @param debug Logical value, whether to print out debug info
#' @export
#' @examples 
#' \dontrun{
#' loadNumbers(c("rksp.0", "vndf.0"))
#' }
loadNumbers <- function(ids=c(),
                        types=c(atypes$Act,
                                atypes$Scene, 
                                atypes$Utterance,
                                atypes$Token,
                                atypes$Sentence,
                                atypes$DramatisPersonae),
                        debug=FALSE) {
  df <- data.frame(ids)
  rownames(df) <- df$ids
  for (a in types) {
    annos <- countAnnotations(ids, type=a, debug=debug)
    df <- cbind(df, annos)
  }
  subset(df,select=c(-1))
}

#' Returns a list of all ids that are installed
#' @export
#' 
loadAllInstalledIds <- function() {
  getOption("qd.dl")$getAllIds()
}

#' @title Download preprocessed drama data
#' @description This function downloads pre-processed dramatic texts via http and stores them locally in your data directory
#' @param dataSource Currently, only "tg" (textgrid) is supported
#' @param dataDirectory The directory in which the data is to be stored
#' @param downloadSource The server from which to download
#' @param removeZipFile If true (the default), the downloaded zip file is removed after unpacking
#' @importFrom utils download.file unzip
#' @export
installData <- function(dataSource="tg", dataDirectory=getOption("qd.datadir"),downloadSource="ims", removeZipFile = TRUE) {
  dir.create(dataDirectory, recursive = TRUE, showWarnings = FALSE) 
  sourceFilename <- switch(dataSource,tg="tg.zip")

  
  if (downloadSource == "ims") {
    sourceUrl <- createIMSUrl(sourceFilename)
  } else if (downloadSource == "zenodo") {
    sourceUrl <- createZenodoUrl(803280, sourceFilename)
  }
  lm <- lastModifiedDate(sourceUrl)
  message("Version on server: ", lm)
  installedV <- getInstalledDate(dataDirectory,sourceFilename)
    
  message("Locally installed version: ", as.character(installedV))
  
  
  if (is.na(installedV) | installedV < lm) {
    message("Downloading new version.")
    tf <- tempfile()
    utils::download.file(sourceUrl,destfile = tf)
    utils::unzip(tf,exdir=file.path(dataDirectory,"xmi"))
    if (removeZipFile == TRUE) {
      file.remove(tf)
    }
    saveInstalledDate(dataDirectory, sourceFilename, lm)
  } else {
    message("No download necessary.")
  }
}




#' @importFrom utils read.csv
getInstalledDate <- function(dataDirectory,filename) {
  versionsFilename <- file.path(dataDirectory,"versions.csv")
  if (file.exists(versionsFilename)) {
    versions <- utils::read.csv(versionsFilename)
    v <- versions[versions$file == filename,2]
    if (length(v)>0) {
      as.Date(v)
    } else {
      NA
    }
  } else {
    NA
  }
}

#' @importFrom utils write.csv read.csv
saveInstalledDate <- function(dataDirectory, filename, date) {
  versionsFilename <- file.path(dataDirectory,"versions.csv")
  if (file.exists(versionsFilename)) {
    versions <- utils::read.csv(versionsFilename,stringsAsFactors = FALSE)
    if (length(versions[versions$file==filename,"date"])>0) {
      versions[versions$file==filename,"date"] <- date
    } else {
      versions[nrow(versions) + 1,] = c(filename,date)
    }
  } else {
    versions <- data.frame(file=c(filename),date=c(date))
  }
  utils::write.csv(versions,file=versionsFilename,row.names=FALSE)
  
}

#' @importFrom httr HEAD headers
lastModifiedDate <- function(url) {
  h <- httr::HEAD(url)
  lm <- httr::headers(h)$`last-modified`
  as.Date(lm, "%a, %d %b %Y %H:%M:%S")
}

createIMSUrl <- function(filename) {
  paste0("https://www2.ims.uni-stuttgart.de/gcl/reiterns/quadrama/res/",filename)
}

createZenodoUrl <- function(id,filename) {
  paste0("https://zenodo.org/record/",id,"/files/",filename)
}
