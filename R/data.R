url <- "http://localhost:8080/drama.web/"

id2url <- function(id) {
  paste(url, "drama/", id, sep="")
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

load_text <- function(tokens=FALSE, ...) {
  args <- list(...)
  r <- data.frame(c())
  s <- ""
  if (tokens == TRUE) {
    s <- "/de.tudarmstadt.ukp.dkpro.core.api.segmentation.type.Token"
  }
  for (a in args) {
    myurl <- paste(id2url(a), "/de.unistuttgart.ims.drama.api.Utterance", s, sep="")
    data <- load_from_url(myurl)
    r <- rbind(r,data)
  }
  r
}

load_from_url <- function(url) {
  read.csv(url, sep="\t", header=TRUE)
}
