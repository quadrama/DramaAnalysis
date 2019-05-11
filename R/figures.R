
#' @title Basic Character Statistics
#' @description This function extracts figure statistics from a drama text table.
#' @return A data frame with the following columns and one row for each figure:
#' tokens: The number of tokens spoken by that figure
#' types : The number of different tokens (= types) spoken by each figure
#' utterances: The number of utterances
#' utteranceLengthMean: The mean length of utterances
#' utteranceLengthSd: The standard deviation in utterance length
#' @param t A data.table containing the text. Will be coerced into a data.table,
#' if necessary.
#' @param normalize Normalising the individual columns
#' @param segment "Drama", "Act", or "Scene". Allows calculating statistics on segments of the play
#' @param filter_punctuation Whether to exclude all punctuation from token counts
#' @importFrom stats sd
#' @importFrom stats aggregate
#' @importFrom data.table as.data.table
#' @examples
#' data(rksp.0)
#' stat <- characterStatistics(rksp.0)
#' @export
characterStatistics <- function(drama, 
                             normalize = FALSE, 
                             segment=c("Drama","Act","Scene"), 
                             filter_punctuation = FALSE) {
  stopifnot(inherits(drama, "QDDrama"))
  
  # prevent notes in R CMD check
  . <- NULL
  Token.surface <- NULL
  begin <- NULL
  end <- NULL
  `:=` <- NULL
  corpus <- NULL
  segment <- match.arg(segment)
  
  text <- switch(segment,
              Drama=drama$text,
              Act=segment(drama$text, drama$segments),
              Scene=segment(drama$text, drama$segments))

  if (filter_punctuation == TRUE) {
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



#' @title Stacked matrix
#' @description This function takes a data frame describing various metrics of figures in dramas 
#' and creates a matrix that can be used to create a stacked bar plot.
#' @param fstat The figure statistics table, i.e., the output of figureStatistics(). Coerced to a data.table if needed.
#' @param column A column name found in the statistics table. This count is used 
#' as a basis for the plot.
#' @param order If set to -1 (default), figures are ranked descending 
#' (i.e., figure with most spoken words first). If set to 1, 
#' figures are ranked ascending.
#' @importFrom reshape2 dcast
characterMatrix <- function(fstat,column="tokens",order=-1) {
  stopifnot(inherits(fstat, "QDCharacterStatistics"))
  # prevent note in R CMD check
  drama <- NULL
  `:=` <- NULL

  fs <- as.data.table(fstat)
  fs[,rank:=as.double(rank( get(column) *order,ties.method = "first")),drama]
  mat_values <- as.matrix(dcast(data=fs,rank ~ drama, value.var=column)[,-1])
  mat_labels <- as.matrix(dcast(data=fs,rank ~ drama, value.var="character")[,-1])
  mat_cs <- apply(mat_values, 2,cumsum)
  mat_cs <- rbind(matrix(0,ncol=ncol(mat_cs)),mat_cs)
  mat_values <- rbind(mat_values,matrix(NA,ncol=ncol(mat_values)))
  list(values=mat_values,labels=mat_labels,cs=mat_cs)
}

#' @title Rank figures by the dramatis personae
#' @description Adds a column to the figures data frame, containing the rank in the dramatis personae.
#' @param figures The figures to rank
#' @param columnTitle The title for the rank column
#' @export
rankFiguresByDramatisPersonae <- function(figures, columnTitle="Rank (dramatis personae)") {
  figures[[columnTitle]] <- ave(seq(1:nrow(figures)),figures$drama, FUN=rank)
  figures
}

#' @title Rank figures by their 1st appearance
#' @description
#' Given a dramatic text and a table of figures, ranks the figures by 
#' their first appearance. The lower the rank number, the earlier the figure appears.
#' "appears": speaks for the first time.
#' @param figures A data frame containing the figures
#' @param text A text data frame
#' @param columnTitle The title for the rank column
#' @export
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

