#' @title Save Isolated Figure Speech
#' @description \code{saveFigureSpeech()} isolates the speeches
#' of individual figures and saves them in seperate text files.
#' @param t A text (or multiple texts, as data.frame or data.table)
#' @param segment "Drama", "Act", or "Scene". Determines on what segment-level the speech is isolated.
#' @param min_token_count The minimal token count for a speech to be saved in a file (default = 0)
#' @param count_punctuation Whether to include punctuation in min_token_count
#' @param dir The directory into which the files will be written (default: current working directory)
#' @export
#' @examples
#' \dontrun{
#' data(rksp.0)
#' saveFigureSpeech(rksp.0$mtext, segment="Scene")
#' }
saveFigureSpeech <- function(t,
                             segment=c("Drama", "Act", "Scene"),
                             min_token_count=0,
                             count_punctuation=TRUE,
                             dir=getwd()) {
  segment = match.arg(segment)
  switch(segment,
         Drama = {r <- split(t[, Token.surface, by=.(drama, Speaker.figure_id)], by=c("drama", "Speaker.figure_id"), keep.by = FALSE, drop=TRUE)},
         Act = {r <- split(t[, Token.surface, by=.(drama, Speaker.figure_id, Number.Act)], by=c("drama", "Speaker.figure_id", "Number.Act"), keep.by=FALSE, drop=TRUE)},
         Scene = {r <- split(t[, Token.surface, by=.(drama, Speaker.figure_id, Number.Act, Number.Scene)], by=c("drama", "Speaker.figure_id", "Number.Act", "Number.Scene"), keep.by=FALSE, drop=TRUE)},
         stop("Please enter valid string-value for argument 'segment' (default = 'Drama', 'Act' or 'Scene').")
  )
  if (count_punctuation) {
    o <- capture.output(lapply(names(r), function(x) {
      if (length(r[[x]]$Token.surface) >= min_token_count){
        fn <- paste(dir, "/", x, sep="")
        if (file.exists(fn)) {file.remove(fn)}
        lapply(r[[x]], cat, sep="\n", file=fn, append=TRUE)
      }
    }))
  } else {
    o <- capture.output(lapply(names(r), function(x) {if (length(r[[x]][!grep(pattern="[[:punct:]]", x=r[[x]]$Token.surface)]$Token.surface) >= min_token_count) 
      fn <- paste(dir, "/", x, sep="")
      if (file.exists(fn)) {file.remove(fn)}
      lapply(r[[x]], cat, sep="\n", file=fn, append=TRUE)
    }))
  }
  message(length(grep(".*\\$Token\\.surface", x=o)), " files were written to ", dir)
}

#' @title Save or Extend a Collection
#' @description \code{saveCollection()} can be used to create new collections 
#' or add dramas to existing collection files.
#' @param t A text (or multiple texts, as data.frame or data.table), or a character 
#' vector containing the drama IDs to be saved
#' @param name The name of the collection file (default: concatenated drama IDs)
#' @param dir The directory into which the collection file will be written (uses the 
#' collection directory as initialized by \code{setup()} by default)
#' @param append Whether to extend the collection file if it already exists. 
#' If FALSE, the file will be overwritten (default: TRUE).
#' @export
#' @examples
#' \dontrun{
#' data(rksp.0, rjmw.0)
#' saveCollection(rbind(rksp.0$mtext, rjmw.0$mtext))
#' saveCollection(c("rksp.0", "rjmw.0"), append=FALSE) # produces identical file
#' saveCollection(c("a", "b"), name="rksp.0_rjmw.0") # adds "a" and "b" to the file
#' }
saveCollection <- function(t, 
                           name=ifelse(typeof(t) == "character",
                                       paste(t, collapse="_"),
                                       paste(unique(t$drama), collapse="_")), 
                           dir=getOption("qd.collectionDirectory"), 
                           append=TRUE) {
  fn <- paste0(dir, "/", name)
  if (typeof(t) !="character") {t <- unique(t$drama)}
  if (append == TRUE && file.exists(fn)) {t <- unique(c(readLines(fn), t))}
  cat(t, sep="\n", file=fn)
  message(name, " was written to ", dir)
}
