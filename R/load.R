

id2url <- function(id, url = "http://localhost:8080/drama.web") {
  paste(url, "annotations", id, sep="/")
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
      data$drama <- a
      data$length <- nrow(data)
      r <- rbind(r,data)
    }, finally=function(w) {print()}, error=function(w){}, warning=function(w){})
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

#' @importFrom utils read.csv
load_from_url <- function(url) {
  read.csv(url, header=TRUE, fileEncoding="UTF-8", encoding="UTF-8")
}

