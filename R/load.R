
#' This function initialises the import from XMI files.
#' @param datadir A path to the directory in which data and metadata are located
#' @export
setup <- function(datadir = "/Users/reiterns/Documents/QuaDramA/Data") {
  options(qd.datadir=datadir)
  options(qd.dl=.jnew("de/unistuttgart/ims/drama/data/DataLoader",datadir))
}

dlobject <- function() {
  if (is.null(getOption("qd.dl"))) {
    options(qd.dl=.jnew("de/unistuttgart/ims/drama/data/DataLoader",getOption("qd.datadir")))
  }
  getOption("qd.dl")
}

#' Function to load a set from QuaDramA web service.
#' Can optionally set the set name as a genre in the returned table
#' @param setName The name of the set to retrieve
#' @param add.genre.column Whether to set the Genre-column in the returned table to the set name
#' @export
load.set <- function(setName, add.genre.column=FALSE) {
  dl <- dlobject()
  ds <- data.frame(id=dl$getCollectionEntries(setName))
  if (add.genre.column == TRUE) {
    ds$Genre <- setName
  }
  ds
}

#' A function to get a list of all collections and the number os plays in that collection
#' @export
#' @importFrom rJava .jevalArray .jsimplify
load.sets <- function() {
  dl <- dlobject()
  s <- dl$getListOfSets()
  l <- unlist(lapply(.jevalArray(s[[2]]),FUN=function(x) {.jsimplify(x)}))
  data.frame(id=.jevalArray(s[[1]]),size=l)
}

scene.act.table <- function(ids) {
  acts <- load.annotations(ids,type="de.unistuttgart.ims.drama.api.Act",coveredType=NULL)
  acts$Number <- ave(acts$begin, acts$drama, FUN=function(x) {as.numeric(as.factor(x))})
  scenes <- load.annotations(ids,type="de.unistuttgart.ims.drama.api.Scene",coveredType = NULL)
  merged <- merge(acts, scenes, by="drama", suffixes=c(".Act", ".Scene"))
  merged <- merged[merged$begin.Act <= merged$begin.Scene & merged$end.Act >= merged$end.Scene,]
  #merged <- subset(merged, select=c(-5,-9))
  merged$Number.Scene <- ave(merged$begin.Scene, merged$drama, merged$Number.Act, FUN=function(x) {as.numeric(as.factor(x))})
  merged
}

#' Loads a CSV-formatted text from the given server url.
#' This function is an incredible bad idea.
load.text2 <- function(ids) {
  text <- load.text(ids, tokens=TRUE)
  satable <- scene.act.table(ids=ids)
  mtext <- merge(text, satable, by="drama")
  mtext <- mtext[mtext$begin >= mtext$begin.Scene & mtext$end <= mtext$end.Scene,]
  mtext
}


#' Loads a CSV-formatted text from the server,
#' assuming the main server url has been set correctly.
#'
#'
#' @param ids A vector containing drama ids to be downloaded
#' @param tokens If set to true, the table also contains each token in an utterance
#' @export
load.text <- function(ids, tokens=FALSE) {
  if (tokens == TRUE) {
    load.annotations(ids, 
                     type="de.unistuttgart.ims.drama.api.Utterance", 
                     coveredType="de.tudarmstadt.ukp.dkpro.core.api.segmentation.type.Token")
  } else
    load.annotations(ids, type="de.unistuttgart.ims.drama.api.Utterance", coveredType=NULL)}

#' @title Load annotations
#' Helper method to load covered annotations.
#' @param ids A vector or list of drama ids
#' @param type The annotation type to load
#' @param coveredType The annotation type of covered annotations we want to load
#' @export
#' @importFrom utils read.csv
#' @importFrom rJava .jnew .jarray .jnull
#' @examples
#' \dontrun{
#' load.annotations(c("rksp.0"))
#' }
load.annotations <- function(ids, 
                             type="de.unistuttgart.ims.drama.api.Utterance", 
                             coveredType="de.tudarmstadt.ukp.dkpro.core.api.segmentation.type.Token") {
  dl <- dlobject()
  if (is.null(coveredType)) {
    s <- dl$getAnnotations(.jarray(ids),type,.jnull())
  } else {
    s <- dl$getAnnotations(.jarray(ids),type,coveredType)
  }
  df <- read.csv(text=s)
  df
}

#' Function to count the annotations of a certain type in selected texts.
#' @param ids A vector or list of drama ids
#' @param type A string, the fully qualified type name we want to count
#' @param debug Logical value, whether to print debug information
#' @param shortname Logical value, whether to only use the local name of type in the returned data frame.
#' @export
count.annotations <- function(ids, 
                             type="de.unistuttgart.ims.drama.api.Utterance",
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
      data2 <- load.annotations(ids,type,NULL)
      r[a,lname] = nrow(data2)
    }, finally=function(w) {print()}, error=function(w){}, warning=function(w){})
  }
  r
}

#' This function loads the number of annotations of different types for several 
#' dramas at once.
#' @param ids A vector containing drama ids
#' @param annotations A character vector containing the annotation types we want to count
#' @param debug Logical value, whether to print out debug info
#' @export
#' @examples 
#' \dontrun{
#' load.numbers(c("rksp.0", "vndf.0"))
#' }
load.numbers <- function(ids=c(),
                         annotations=c("de.unistuttgart.ims.drama.api.Act",
                                       "de.unistuttgart.ims.drama.api.Scene", 
                                       "de.unistuttgart.ims.drama.api.Utterance", 
                                       "de.tudarmstadt.ukp.dkpro.core.api.segmentation.type.Token",
                                       "de.tudarmstadt.ukp.dkpro.core.api.segmentation.type.Sentence",
                                       "de.unistuttgart.ims.drama.api.DramatisPersonae"), 
                         debug=FALSE) {
  df <- data.frame(ids)
  rownames(df) <- df$ids
  for (a in annotations) {
    annos <- count.annotations(ids, type=a, debug=debug)
    df <- cbind(df, annos)
  }
  subset(df,select=c(-1))
}


