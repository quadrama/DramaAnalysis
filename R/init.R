environment <- new.env()

# This needs to change at some point
environment$url <- "http://zwergdrossel.ims.uni-stuttgart.de:8080/drama.web/"

# environment$url <- "http://localhost:8080/drama.web/"

#' Sets the base url to load from
#' @param s The new url to use
#' @export
qd.setUrl <- function(s) {
  environment$url <- s
}

#' The base url to retrieve data files from
#' @export
qd.url <- function() {
  environment$url
}