
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
#' @param names If set to true, the table will contains figure names instead of ids
#' @param normalize Normalising the individual columns
#' @importFrom stats sd
#' @importFrom stats aggregate
#' @importFrom data.table as.data.table
#' @examples
#' data(rksp.0)
#' stat <- figureStatistics(rksp.0$mtext, names = FALSE)
#' @export
figureStatistics <- function(t, names = FALSE, normalize = FALSE, segment=c("Drama","Act","Scene")) {
  
  # prevent notes in R CMD check
  . <- NULL
  Token.surface <- NULL
  begin <- NULL
  end <- NULL
  drama <- NULL
  `:=` <- NULL
  corpus <- NULL
  
  t <- as.data.table(t)
  
  b <- quote(Speaker.figure_id)
  if (names == TRUE) {
    b <- quote(Speaker.figure_surface)
  }
  segment <- match.arg(segment)

  setkey(t, "corpus", "drama")
  if (segment == "Scene") {
    r <- t[,list(tokens=length(Token.surface),
         types=data.table::uniqueN(Token.surface),
         utterances=data.table::uniqueN(begin),
         utteranceLengthMean=mean(rle(begin)$lengths),
         utteranceLengthSd=sd(rle(begin)$lengths),
         firstBegin=min(begin),
         lastEnd=max(end)
         ),by=.(corpus,drama,begin.Act,begin.Scene,length,eval(b))][,begin.Scene:=as.integer(as.factor(begin.Scene)),begin.Act]
    r$begin.Act <- as.roman(as.integer(as.factor(r$begin.Act)))
    colnames(r)[3:4] <- c("Act","Scene")
    fcol <- 6
  } else if (segment == "Act") {
    r <- t[,list(tokens=length(Token.surface),
                 types=data.table::uniqueN(Token.surface),
                 utterances=data.table::uniqueN(begin),
                 utteranceLengthMean=mean(rle(begin)$lengths),
                 utteranceLengthSd=sd(rle(begin)$lengths),
                 firstBegin=min(begin),
                 lastEnd=max(end)
    ),by=.(corpus,drama,begin.Act,length,eval(b))]
    r$begin.Act <- as.roman(as.integer(as.factor(r$begin.Act)))
    colnames(r)[3] <- "Act"
    fcol <- 5
  } else {
    r <- t[,list(tokens=length(Token.surface),
                 types=data.table::uniqueN(Token.surface),
                 utterances=data.table::uniqueN(begin),
                 utteranceLengthMean=mean(rle(begin)$lengths),
                 utteranceLengthSd=sd(rle(begin)$lengths),
                 firstBegin=min(begin),
                 lastEnd=max(end)
    ),by=.(corpus,drama,length,eval(b))]
    fcol <- 4
  }
  
  #colnames(r)[fcol] <- "figure"
  if (normalize == TRUE) {
    r$tokens <- r$tokens / r$length
    r$utterances <- ave(r$utterances, r$drama, FUN=function(x) {x/sum(x)})
    r$firstBegin <- r$firstBegin / ave(r$lastEnd, r$drama, FUN=max)
    r$lastEnd <- ave(r$lastEnd, r$drama, FUN=function(x) {x/max(x)})
  }
  r
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
#' @examples
#' data(rksp.0,rjmw.0)
#' text <- rbind(rksp.0$mtext,rjmw.0$mtext)
#' stat <- figureStatistics(text, names = TRUE)
#' mat <- figurematrix(stat)
#' # Plot a stacked bar plot
#' b <- barplot(mat$values,col=qd.colors)
#' # Add figure names (if needed/wanted)
#' text(x=b,y=t(mat$cs+(mat$values/2)),labels=t(substr(mat$labels,0,20)))
#' @export
figurematrix <- function(fstat,column="tokens",order=-1) {
  
  # prevent note in R CMD check
  drama <- NULL
  `:=` <- NULL

  fs <- as.data.table(fstat)
  fs[,rank:=as.double(rank( get(column) *order,ties.method = "first")),drama]
  mat_values <- as.matrix(dcast(data=fs,rank ~ drama, value.var=column)[,-1])
  mat_labels <- as.matrix(dcast(data=fs,rank ~ drama, value.var="figure")[,-1])
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

