

id2url <- function(id, url = "http://localhost:8080/drama.web") {
  paste(url, "annotations", id, sep="/")
}

#' Function to load a set from QuaDramA web service.
#' Can optionally set the set name as a genre in the returned table
#' @param setName The name of the set to retrieve
#' @param url The base url of the web service
#' @param setGenre Whether to set the Genre-column in the returned table to the set name
#' @export
load.set <- function(setName, url = "http://localhost:8080/drama.web3",setGenre=FALSE) {
  ds <- load_from_url(paste(url,"set",setName,sep="/"))
  if (setGenre == TRUE) {
    ds$Genre <- setName
  }
  ds
}

scene.act.table <- function(ids, url) {
  acts <- load.annotations(ids,type="de.unistuttgart.ims.drama.api.Act",coveredType=NULL,url=url)
  acts$Number <- ave(acts$begin, acts$drama, FUN=function(x) {as.numeric(as.factor(x))})
  scenes <- load.annotations(ids,type="de.unistuttgart.ims.drama.api.Scene",coveredType = NULL,url=url)
  merged <- merge(acts, scenes, by="drama", suffixes=c(".Act", ".Scene"))
  merged <- merged[merged$begin.Act <= merged$begin.Scene & merged$end.Act >= merged$end.Scene,]
  #merged <- subset(merged, select=c(-5,-9))
  merged$Number.Scene <- ave(merged$begin.Scene, merged$drama, merged$Number.Act, FUN=function(x) {as.numeric(as.factor(x))})
  merged
}

#' Loads a CSV-formatted text from the given server url.
#' @param ids A list or vector of ids
#' @param url the URL to reach the server.
#' @export
#' 
load.text2 <- function(ids, url="http://localhost:8080/drama.web") {
  text <- load.text(ids, tokens=TRUE, url=url)
  satable <- scene.act.table(ids=ids, url=url)
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
#' @param url The url we load the text from
#' @export
load.text <- function(ids, tokens=FALSE, url="http://localhost:8080/drama.web") {
  if (tokens == TRUE) {
    load.annotations(ids, 
                     type="de.unistuttgart.ims.drama.api.Utterance", 
                     coveredType="de.tudarmstadt.ukp.dkpro.core.api.segmentation.type.Token",
                     url=url)
  } else
    load.annotations(ids, type="de.unistuttgart.ims.drama.api.Utterance", coveredType=NULL,
                     url=url)}

#' Helper method to load covered annotations.
#' @param ids A vector or list of drama ids
#' @param type The annotation type to load
#' @param coveredType The annotation type of covered annotations we want to load
#' @param url The base url the web service can be reached with
#' @export
#' @examples
#' \dontrun{
#' load.annotations(c("rksp.0"))
#' }
load.annotations <- function(ids, 
                             type="de.unistuttgart.ims.drama.api.Utterance", 
                             coveredType="de.tudarmstadt.ukp.dkpro.core.api.segmentation.type.Token",
                             url="http://localhost:8080/drama.web") {
  r <- data.frame(c())
  s <- ""
  if (! is.null(coveredType)) {
    s <- paste("/", coveredType, sep="")
  }
  for (a in ids) {
    myurl <- paste(id2url(a, url=url), "/", type, s, sep="")
    print(myurl)
    tryCatch({
      data <- load_from_url(myurl)
      #data$drama <- a
      #data$length <- nrow(data)
      r <- rbind(r,data)
    }, finally=function(w) {print()}, error=function(w){print(w)}, warning=function(w){print(w)})
  }
  r
}

#' Function to count the annotations of a certain type in selected texts.
#' @param ids A vector or list of drama ids
#' @param type A string, the fully qualified type name we want to count
#' @param url The base url the web service can be reached with
#' @param debug Logical value, whether to print debug information
#' @param shortname Logical value, whether to only use the local name of type in the returned data frame.
#' @export
count.annotations <- function(ids, 
                             type="de.unistuttgart.ims.drama.api.Utterance",
                             url="http://localhost:8080/drama.web",
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
    myurl <- paste(id2url(a, url=url), "/", type, s, sep="")
    if (debug == TRUE) {
      print(myurl)
    }
    tryCatch({
      data2 <- load_from_url(myurl)
      r[a,lname] = nrow(data2)
    }, finally=function(w) {print()}, error=function(w){}, warning=function(w){})
  }
  r
}

#' This function loads the number of annotations of different types for several 
#' dramas at once.
#' @param ids A vector containing drama ids
#' @param annotations A character vector containing the annotation types we want to count
#' @param url The url to the server
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
                         url="http://localhost:8080/drama.web3", debug=FALSE) {
  df <- data.frame(ids)
  rownames(df) <- df$ids
  for (a in annotations) {
    annos <- count.annotations(ids, type=a, url=url, debug=debug)
    df <- cbind(df, annos)
  }
  subset(df,select=c(-1))
}

#' @importFrom utils read.csv
load_from_url <- function(url) {
  read.csv(url, header=TRUE, fileEncoding="UTF-8", encoding="UTF-8")
}

