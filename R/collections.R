#' @export
#' @rdname setup
setCollectionDirectory <- function(collectionDirectory = file.path(getOption("qd.datadir"), "collections")) {
  options(qd.collectionDirectory=collectionDirectory)
}


#' @title Installed texts
#' @description Returns a list of all ids that are installed
#' @param asDataFrame Logical value. Controls whether 
#' the return value is a list (with colon-joined ids) 
#' or a data.frame with two columns (corpus, drama)
#' @param dataDirectory The directory in which precompiled 
#' drama data is installed
#' @export
#' 
loadAllInstalledIds <- function(asDataFrame=FALSE, 
                                dataDirectory=getOption("qd.datadir")) {
  files <- list.files(path=file.path(dataDirectory),pattern=".*\\.(csv|xmi)", recursive = TRUE)
  files <- strsplit(files, .Platform$file.sep, fixed=TRUE)
  files <- lapply(files, function(x) {
    parts <- unlist(strsplit(x[3],".",fixed=TRUE))
    if (data.table::last(parts)=="xmi") {
      x[3] <- paste(parts[1:(length(parts)-1)],sep=".",collapse=".")
    } else if (data.table::last(parts)=="csv") {
      x[3] <- paste(parts[1:(length(parts)-2)],sep=".",collapse=".")
    }
    x
  })
  files <- unique(files)
  if (asDataFrame) {
    data.frame(matrix(unlist(files), nrow=length(files), byrow=T))
  } else {
    unlist(lapply(files, function(x) { paste(x[c(1,3)],sep=":", collapse=":") }))
  }
}


#' @importFrom utils read.table
loadSetsInternally <- function() {
  setNames <- list.files(getOption("qd.collectionDirectory"))
  sets <- lapply(setNames, 
                 function(x) { 
                   utils::read.table(
                     file.path(getOption("qd.collectionDirectory"), x),
                     encoding = "UTF-8",
                     stringsAsFactors = FALSE
                   )$V1
                 })
  names(sets) <- setNames
  sets
}

#' @title Load Collections
#' @description Function to load a set from collection files
#' Can optionally set the set name as a genre in the returned table. 
#' \code{loadSets()} returns table of all defined collections (and the
#' number of plays in each).
#' @param setName A character vector. The name of the set(s) to retrieve.
#' @param addGenreColumn Logical. Whether to set the Genre-column in 
#' the returned table to the set name. If set to FALSE (default), a vector
#' is returned. In this case, association to collections is not returned.
#' Otherwise, it's a data.frame.
#' @export
loadSet <- function(setName, addGenreColumn=FALSE) {
  sets <- loadSetsInternally()
  s <- sets[setName]
  if (addGenreColumn == TRUE) {
    Reduce(rbind,
           mapply(function(x,y) { data.frame(id=x, Genre=rep(y,length(x))) },
                  x=s, 
                  y=names(s),
                  SIMPLIFY = FALSE)
    )
  } else {
    Reduce(c,s)
  }
}

#' @export
#' @rdname loadSet
loadSets <- function() {
  sets <- loadSetsInternally()
  data.frame(size=unlist(lapply(sets,length)))
}