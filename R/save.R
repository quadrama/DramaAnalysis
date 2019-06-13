#' @title Isolates Figure Speech
#' @description \code{isolateFigureSpeech()} isolates the speeches
#' of individual figures and optionally saves them in seperate text files.
#' @param t A text (or multiple texts, as data.frame or data.table)
#' @param segment "Drama", "Act", or "Scene". Determines on what segment-level the speech is isolated.
#' @param min_token_count The minimal token count for a speech to be considered (default = 0)
#' @param count_punctuation Whether to include punctuation in min_token_count (default = TRUE)
#' @param write_to_files Whether to write each isolated speech into a new text file (default = TRUE)
#' @param dir The directory into which the files will be written (default = data directory)
#' @export
#' @examples
#' \dontrun{
#' data(rksp.0)
#' isolateFigureSpeech(rksp.0, segment="Scene")
#' }
isolateFigureSpeech <- function(drama,
                             segment=c("Drama", "Act", "Scene"),
                             min_token_count=0,
                             count_punctuation=TRUE,
                             write_to_files=TRUE,
                             dir=getOption("qd.datadir")) {
  stopifnot(inherits(drama, "QDDrama"))
  
  # we need this to prevent notes in R CMD check
  . <- NULL
  Token.surface <- NULL
  Speaker.figure_id <- NULL
  Number.Act <- NULL
  Number.Scene <- NULL
  `:=` <- NULL
  fn <- NULL
  
  t <- segment(drama$text, drama$segments)
  
  t <- t[, Token.surface, by=.(drama, Speaker.figure_id, Number.Act, Number.Scene)]
  t$drama <- gsub("_", ".", t[,drama])
  t$Speaker.figure_id <- gsub("_", ".", t[,Speaker.figure_id])
  segment = match.arg(segment)
  switch(segment,
         Drama = {
           t <- t[,fn:=paste(drama, Speaker.figure_id, sep="_")]
         },
         Act = {
           if (length(unique(t$drama))==1) {
             t <- t[,fn:=paste(Speaker.figure_id, Number.Act, sep="_")]
           } else {
             t <- t[,fn:=paste(drama, Speaker.figure_id, Number.Act, sep="_")]
           }
         },
         Scene = {
           if (length(unique(t$drama))==1) {
             t <- t[,fn:=paste(Speaker.figure_id, Number.Act, Number.Scene, sep="_")]
           } else {
             t <- t[,fn:=paste(drama, Speaker.figure_id, Number.Act, Number.Scene, sep="_")]
           }
         },
         stop("Please enter valid string-value for argument 'segment' (default = 'Drama', 'Act' or 'Scene').")
  )
  r <- split(t[, c("drama", "Speaker.figure_id", "Number.Act", "Number.Scene"):=NULL], by="fn", keep.by=FALSE, drop=TRUE)
  
  if (count_punctuation) {
    r <- lapply(names(r), function(x) {
      if (length(r[[x]]$Token.surface) >= min_token_count){
        r[x]
      }
    })
  } else {
    r <- lapply(names(r), function(x) {
      if (length(r[[x]][!grep(pattern="[[:punct:]]", x=r[[x]]$Token.surface)]$Token.surface) >= min_token_count){
        r[x]
      }
    })
  }
  r <- unlist(r, recursive=FALSE)
  
  if (write_to_files) {
    o <- capture.output(lapply(names(r), function(x) {
      fn <- paste(dir, "/", x, sep="")
      if (file.exists(fn)) {file.remove(fn)}
      lapply(r[[x]], cat, sep="\n", file=fn, append=TRUE)
    }))
    message(length(grep(".*\\$Token\\.surface", x=o)), " files were written to ", dir)
  }
  
  r
}

