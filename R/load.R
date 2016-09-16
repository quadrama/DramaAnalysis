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
load_text <- function(ids, tokens=FALSE) {
  load_internal(ids, tokens=tokens)
}

#' @export
load_covered <- function(ids, type="de.unistuttgart.ims.drama.api.Utterance", coveredType="de.tudarmstadt.ukp.dkpro.core.api.segmentation.type.Token") {
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

#' @export
load_internal <- function(ids, type="de.unistuttgart.ims.drama.api.Utterance", tokens=FALSE) {
  r <- data.frame(c())
  s <- ""
  if (tokens == TRUE) {
    s <- "/de.tudarmstadt.ukp.dkpro.core.api.segmentation.type.Token"
  }
  for (a in ids) {
    myurl <- paste(id2url(a), "/", type, s, sep="")
    print(myurl)
    data <- load_from_url(myurl)
    r <- rbind(r,data)
  }
  r
}

load_from_url <- function(url) {
  read.csv(url, header=TRUE, fileEncoding="UTF-8")
}

#' Loads a data frame of drama ids from the server.
#' Please see https://github.com/quadrama/webservice/blob/master/README.md
#' for the string format.
#' @export
search_drama <- function(s) {
  url <- paste(environment$url, "set/q/", s, sep="")
  r <- read.csv(url, header=FALSE, fileEncoding="UTF-8", encoding="UTF-8")
  colnames(r) <- c("drama")
  r
}
