#' @export
qd.url <- function() {
  environment$url
}

id2url <- function(id) {
  paste(environment$url, "annotations/", id, sep="")
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
    load.annotations(ids, type="de.unistuttgart.ims.drama.api.Utterance", coveredType="de.tudarmstadt.ukp.dkpro.core.api.segmentation.type.Token")
  } else
    load.annotations(ids, type="de.unistuttgart.ims.drama.api.Utterance", coveredType=NULL)}

#' Helper method to load covered annotations.
#' @param ids A vector or list of drama ids
#' @param type The annotation type to load
#' @param coveredType The annotation type of covered annotations we want to load
#' @export
#' @examples
#' load.annotations(c("rksp.0"))
load.annotations <- function(ids, type="de.unistuttgart.ims.drama.api.Utterance", coveredType="de.tudarmstadt.ukp.dkpro.core.api.segmentation.type.Token") {
  r <- data.frame(c())
  s <- ""
  if (! is.null(coveredType)) {
    s <- paste("/", coveredType, sep="")
  }
  for (a in ids) {
    myurl <- paste(id2url(a), "/", type, s, sep="")
    print(myurl)
    tryCatch({
      data <- load_from_url(myurl)
      r <- rbind(r,data)
    }, finally=function(w) {print()}, error=function(w){}, warning=function(w){})
  }
  r
}

#' @importFrom utils read.csv
load_from_url <- function(url) {
  read.csv(url, header=TRUE, fileEncoding="UTF-8")
}

