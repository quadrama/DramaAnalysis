
#' @title Basic Character Statistics
#' @description This function extracts character statistics from a drama object.
#' @return A data frame with the additional classes 
#' \code{QDCharacterStatistics} and \code{QDHasCharacter}. It has following 
#' columns and one row for each character:
#' tokens: The number of tokens spoken by that character
#' types : The number of different tokens (= types) spoken by each character
#' utterances: The number of utterances
#' utteranceLengthMean: The mean length of utterances
#' utteranceLengthSd: The standard deviation in utterance length
#' @param drama A \code{QDDrama} object
#' @param normalize Normalizing the individual columns
#' @param segment "Drama", "Act", or "Scene". Allows calculating statistics on segments of the play
#' @param filterPunctuation Whether to exclude all punctuation from token counts
#' @importFrom stats sd
#' @importFrom data.table as.data.table uniqueN
#' @examples
#' data(rksp.0)
#' stat <- characterStatistics(rksp.0)
#' @seealso \code{\link{characterNames}}
#' @export
characterStatistics <- function(drama, 
                             normalize = FALSE, 
                             segment=c("Drama","Act","Scene"), 
                             filterPunctuation = FALSE) {
  stopifnot(inherits(drama, "QDDrama"))
  
  # prevent notes in R CMD check
  . <- NULL
  Token.surface <- NULL
  begin <- NULL
  end <- NULL
  `:=` <- NULL
  corpus <- NULL
  utteranceBegin <- NULL
  utteranceEnd <- NULL
  begin.Act <- NULL
  begin.Scene <- NULL 
  
  segment <- match.arg(segment)
  
  text <- switch(segment,
              Drama=drama$text,
              Act=segment(drama$text, drama$segments),
              Scene=segment(drama$text, drama$segments))

  if (filterPunctuation == TRUE) {
    text <- text[!grep(pattern="[[:punct:]]", x=text$Token.surface)]
    text$length <- nrow(text)
  }
  
  text <- data.table::data.table(text)
  
  b <- quote(Speaker.figure_id)
  data.table::setkey(text, corpus, drama)
  
  if (segment == "Scene") {
    r <- text[,list(tokens     = as.double(length(Token.surface)),
                    types      = as.double(data.table::uniqueN(Token.surface)),
                    utterances = as.double(data.table::uniqueN(utteranceBegin)),
                    utteranceLengthMean = mean(rle(utteranceBegin)$lengths),
                    utteranceLengthSd   = sd(rle(utteranceBegin)$lengths),
                    firstBegin = as.double(min(utteranceBegin)),
                    lastEnd    = as.double(max(utteranceEnd))),
           by=.(corpus,
                drama,
                begin.Act,
                begin.Scene,
                eval(b))][,begin.Scene:=as.double(as.factor(begin.Scene)),begin.Act]
    r$begin.Act <- as.roman(as.integer(as.factor(r$begin.Act)))
    colnames(r)[3:4] <- c("Act","Scene")
    fcol <- 5
  } else if (segment == "Act") {
    r <- text[,list(tokens=length(Token.surface),
                 types=data.table::uniqueN(Token.surface),
                 utterances=data.table::uniqueN(utteranceBegin),
                 utteranceLengthMean=mean(rle(utteranceBegin)$lengths),
                 utteranceLengthSd=sd(rle(utteranceBegin)$lengths),
                 firstBegin=min(utteranceBegin),
                 lastEnd=max(utteranceEnd)
    ),by=.(corpus,drama,begin.Act,eval(b))]
    r$begin.Act <- as.roman(as.integer(as.factor(r$begin.Act)))
    colnames(r)[3] <- "Act"
    fcol <- 4
  } else {
    r <- text[,list(tokens=length(Token.surface),
                 types=data.table::uniqueN(Token.surface),
                 utterances=data.table::uniqueN(utteranceBegin),
                 utteranceLengthMean=mean(rle(utteranceBegin)$lengths),
                 utteranceLengthSd=sd(rle(utteranceBegin)$lengths),
                 firstBegin=min(utteranceBegin),
                 lastEnd=max(utteranceEnd)
    ),by=.(corpus,drama,eval(b))]
    fcol <- 3
  }
  
  colnames(r)[fcol] <- "character"
  if (normalize == TRUE) {
    r$tokens <-     ave(r$tokens, r$drama, FUN=function(x) {x/sum(x)})
    r$utterances <- ave(r$utterances, r$drama, FUN=function(x) {x/sum(x)})
    r$firstBegin <- r$firstBegin / ave(r$lastEnd, r$drama, FUN=max)
    r$lastEnd <- ave(r$lastEnd, r$drama, FUN=function(x) {x/max(x)})
  }
  class(r) <- c("QDCharacterStatistics", "QDHasCharacter", "data.frame")
  r
}

figureStatistics <- function(...) {
  .Deprecated("characterStatistics()")
  characterStatistics(...)
}


#' @title Isolate Character Speech
#' @description \code{isolateCharacterSpeech()} isolates the speeches
#' of individual characters and optionally saves them in separate text files.
#' @param drama A text (or multiple texts, as a QDDrama object)
#' @param segment "Drama", "Act", or "Scene". Determines on what segment-level the speech is isolated.
#' @param minTokenCount The minimal token count for a speech to be considered (default = 0)
#' @param countPunctuation Whether to include punctuation in minTokenCount (default = TRUE)
#' @param writeToFiles Whether to write each isolated speech into a new text file (default = TRUE)
#' @param dir The directory into which the files will be written (default = data directory)
#' @return A named list of character vectors, each corresponding to character speeches as defined by \code{segment}
#' @export
#' @importFrom utils head
#' @examples
#' data(rksp.0)
#' isolateCharacterSpeech(rksp.0, segment="Scene", writeToFiles=FALSE)
isolateCharacterSpeech <- function(drama,
                                   segment = c("Drama", "Act", "Scene"),
                                   minTokenCount = 0,
                                   countPunctuation = TRUE,
                                   writeToFiles = TRUE,
                                   dir = getOption("qd.datadir")) {
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
  r <- split(t[, c("drama", "Speaker.figure_id", "Number.Act", "Number.Scene"):=NULL], 
             by="fn", keep.by=FALSE, drop=TRUE)
  
  if (countPunctuation) {
    lapply(names(r), function(x) {
      if (length(r[[x]]$Token.surface) >= minTokenCount){
        r[[x]] <<- r[[x]]$Token.surface
      } else {
        r[[x]] <<- NULL
      }
    })
  } else {
    lapply(names(r), function(x) {
      if (length(r[[x]][!grep(pattern="[[:punct:]]", x=r[[x]]$Token.surface)]$Token.surface) >= minTokenCount){
        r[[x]] <<- r[[x]]$Token.surface
      } else {
        r[[x]] <<- NULL
      }
    })
  }
  
  if (writeToFiles) {
    lapply(names(r), function(x) {
      fn <- paste(dir, "/", x, sep="")
      if (file.exists(fn)) {file.remove(fn)}
      lapply(r[[x]], cat, sep="\n", file=fn, append=TRUE)
    })
    message(length(r), " files were written to ", dir)
  }
  
  r
}
