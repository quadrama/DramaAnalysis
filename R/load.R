

id2url <- function(id, url = qd.baseurl) {
  paste(environment$url, "annotations", id, sep="/")
}

#' Loads a CSV-formatted text from the server,
#' assuming the main server url has been set correctly.
#'
#'
#' @param ids A vector containing drama ids to be downloaded
#' @param tokens If set to true, the table also contains each token in an utterance
#' @param url The url we load the text from
#' @export
load.text <- function(ids, tokens=FALSE, url=qd.baseurl) {
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
                             url=qd.baseurl) {
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
      r <- rbind(r,data)
    }, finally=function(w) {print()}, error=function(w){}, warning=function(w){})
  }
  r
}

#' @importFrom utils read.csv
load_from_url <- function(url) {
  read.csv(url, header=TRUE, fileEncoding="UTF-8", encoding="UTF-8")
}

