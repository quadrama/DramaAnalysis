id2url <- function(id) {
  paste(environment$url, "annotations/", id, sep="")
}

load_figures <- function(...) {
  args <- list(...)
  r <- data.frame(c())
  for (a in args) {
    myurl <- paste(id2url(a), "/de.unistuttgart.ims.drama.api.Figure", sep="")
    data <- load_from_url(myurl)
    r <- rbind(r,data)
  }
  r
}

#' Loads a CSV-formatted text from the server,
#' assuming the main server url has been set correctly.
#'
#' @export
load_text <- function(ids, tokens=FALSE) {
  r <- data.frame(c())
  s <- ""
  if (tokens == TRUE) {
    s <- "/de.tudarmstadt.ukp.dkpro.core.api.segmentation.type.Token"
  }
  for (a in ids) {
    myurl <- paste(id2url(a), "/de.unistuttgart.ims.drama.api.Utterance", s, sep="")
    print(myurl)
    data <- load_from_url(myurl)
    r <- rbind(r,data)
  }
  r
}

load_from_url <- function(url) {
  read.csv(url, sep="\t", header=TRUE, fileEncoding="UTF-8")
}
