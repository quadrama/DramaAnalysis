#' @export
load.set2 <- function(setName, setGenre=FALSE) {
  dl <- .jnew("de/unistuttgart/ims/drama/R/DataLoader","/Users/reiterns/Documents/QuaDramA/Data")
  ds <- data.frame(id=dl$getCollectionEntries(setName))
  if (setGenre == TRUE) {
    ds$Genre <- setName
  }
  ds
}

#' @export
load.sets <- function() {
  dl <- .jnew("de/unistuttgart/ims/drama/R/DataLoader","/Users/reiterns/Documents/QuaDramA/Data")
  s <- dl$getListOfSets()
  l <- unlist(lapply(.jevalArray(s[[2]]),FUN=function(x) {.jsimplify(x)}))
  data.frame(id=.jevalArray(s[[1]]),size=l)
}