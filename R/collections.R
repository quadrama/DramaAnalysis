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

#' @title Replace corpus prefix
#' @description This function can be used to replace corpus prefixes.
#' If a list of play ids contains textgrid prefixes, for instance, this 
#' function can be used to map them onto GerDraCor prefixes. Please note
#' that the function does \emph{not} check whether the play actually exists 
#' in the corpus.
#' @param idList The list of ids in which we want to replace.
#' @param map A list containing the old prefix as name and the new one as
#' values.
#' @export
#' @examples
#' 
#' # returns c("corpus2:play1", "corpus2:play2")
#' mapPrefix(c("corpus1:play1", "corpus1:play2"), list(corpus1="corpus2"))
#' 
mapPrefix <- function(idList, map) {
  r <- idList
  for (x in names(map)) {
    r <- sub(paste0(x, ":"), paste0(map[[x]], ":"), r, fixed=TRUE)
  }
  r
}

#' @title Create or Extend a Collection
#' @description \code{newCollection()} can be used to create new collections 
#' or add dramas to existing collection files.
#' @param drama A text (or multiple texts, as data.frame or data.table), or a character 
#' vector containing the drama IDs to be collected
#' @param name The name of the collection and its filename (default = concatenated drama IDs)
#' @param writeToFile = Whether to write the collection to a file (default = TRUE)
#' @param dir The directory into which the collection file will be written (default = collection directory)
#' @param append Whether to extend the collection file if it already exists. 
#' If FALSE, the file will be overwritten. (default = TRUE)
#' @export
#' @examples
#' \dontrun{
#' t <- combine(rksp.0, rjmw.0)
#' newCollection(t)
#' newCollection(c("rksp.0", "rjmw.0"), append=FALSE) # produces identical file
#' newCollection(c("a", "b"), name="rksp.0_rjmw.0") # adds "a" and "b" to the file
#' }
newCollection <- function(drama, 
                          name=ifelse(inherits(drama, "QDDrama"),
                                      paste(unique(drama$meta$drama)),
                                      paste(drama,collapse="_")),
                          writeToFile=TRUE,
                          dir=getOption("qd.collectionDirectory"), 
                          append=TRUE) {
  stopifnot(inherits(drama, "QDDrama") || is.character(drama))
  
  fn <- paste0(dir, "/", name)
  if (inherits(drama, "QDDrama")) {
    t <- unique(drama$meta$drama)
  } else {
    t <- drama
  }
  
  if (writeToFile) {
    if (append && file.exists(fn)) {
      t <- unique(c(readLines(fn), t))
    }
    cat(t, sep="\n", file=fn)
    message(name, " was written to ", dir)
  }
  
  t
}

