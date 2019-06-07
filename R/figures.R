
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
#' @seealso \code{\link{format.QDHasCharacter}}
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


# @title Rank figures by the dramatis personae
# @description Adds a column to the figures data frame, containing the rank in the dramatis personae.
# @param figures The figures to rank
# @param columnTitle The title for the rank column
rankFiguresByDramatisPersonae <- function(figures, columnTitle="Rank (dramatis personae)") {
  figures[[columnTitle]] <- ave(seq(1:nrow(figures)),figures$drama, FUN=rank)
  figures
}

# @title Rank figures by their 1st appearance
# @description
# Given a dramatic text and a table of figures, ranks the figures by 
# their first appearance. The lower the rank number, the earlier the figure appears.
# "appears": speaks for the first time.
# @param figures A data frame containing the figures
# @param text A text data frame
# @param columnTitle The title for the rank column
rankFiguresByAppearance <- function(figures, text, columnTitle="Rank (1st appearance)") {
  minimal.utterance.begin <- aggregate(text$begin, by=list(text$drama,
                                                           text$Speaker.figure_surface),
                                       min)
  colnames(minimal.utterance.begin) <- c("drama", "figure", "begin")
  minimal.utterance.begin[[columnTitle]] <- ave(minimal.utterance.begin$begin, 
                                                         minimal.utterance.begin$drama, FUN=rank)
  minimal.utterance.begin <- subset(minimal.utterance.begin, select=c("figure","drama",columnTitle))
  merge(figures, minimal.utterance.begin, by.x=c("drama","Figure.surface"), by.y=c("drama","figure"))
}

figures.first.appearances <- function(texts, acts) {
  acts$Number <- ave(acts$begin, acts$drama, FUN=rank)
  minimal.utterance.begin <- aggregate(texts$begin, by=list(texts$drama, 
                                                            texts$Speaker.figure_surface), 
                                       min)
  mub.at <- merge(minimal.utterance.begin, acts, by.x="Group.1", by.y="drama")
  mub.at <- mub.at[mub.at$x>=mub.at$begin& mub.at$x<=mub.at$end,]
  mub.at
}

