#' This function extracts figure statistics from a drama text table.
#' @return A data frame with the following columns and one row for each figure:
#' tokens: The number of tokens spoken by that figure
#' types : The number of different tokens (= types) spoken by each figure
#' utterances: The number of utterances
#' utteranceLengthMean: The mean length of utterances
#' utteranceLengthSd: The standard deviation in utterance length
#' @param t The drama text
#' @param names If set to true, the table will contains figure names instead of ids
#' @param normalize Normalising the individual columns
#' @importFrom stats sd
#' @importFrom stats aggregate
#' @examples
#' data(rksp.0.text)
#' stat <- figureStatistics(rksp.0.text, names = FALSE)
#' @export
figureStatistics <- function(t, names = FALSE, normalize = FALSE) {
  dup <- tapply(t$begin, paste(t$drama, t$Speaker.figure_id), function(x) {
    dup <- duplicated(x)
    diffs <- dup[-1L] != dup[-length(dup)]
    idx <- c(which(diffs), length(dup))
    diff(c(0, idx))
  })
  indexes <- paste(t$drama, t$Speaker.figure_id)
  bylist <- list(t$drama, t$Speaker.figure_id)
  if (names == TRUE) {
    indexes <- paste(t$drama, t$Speaker.figure_surface)
    bylist <- list(t$drama, t$Speaker.figure_surface)
  }

  r <- as.data.frame(cbind(
    aggregate(t$Token.surface, by=bylist, function(x) { length(x) }),
    aggregate(t$Token.surface, by=bylist, function(x) { length(unique(x)) })[,3],
    aggregate(t$begin, by=bylist, function(x) { length(unique(x)) })[,3],
    aggregate(t$begin, by=bylist, function(x) { mean(rle(x)$lengths) })[,3],
    aggregate(t$begin, by=bylist, function(x) { sd(rle(x)$lengths) })[,3],
    aggregate(t$begin, by=bylist, min)[,3],
    aggregate(t$end, by=bylist, max)[,3],
    aggregate(t$length, by=bylist, function(x) { unique(x) })[,3]
  ))
  colnames(r) <- c("drama", "figure","tokens", "types", "utterances", "utteranceLengthMean", "utteranceLengthSd", "firstBegin", "lastEnd", "length")
  if (normalize == TRUE) {
    r$tokens <- r$tokens / r$length
    r$utterances <- ave(r$utterances, r$drama, FUN=function(x) {x/sum(x)})
    r$firstBegin <- r$firstBegin / ave(r$lastEnd, r$drama, FUN=max)
    r$lastEnd <- ave(r$lastEnd, r$drama, FUN=function(x) {x/max(x)})
  }
  r
}


#' This function takes a data frame describing various metrics of figures in dramas 
#' and creates a matrix that can be used to create a stacked bar plot.
#' @param fstat The figure statistics table, i.e., the output of figure.statistics()
#' @param column A column name found in the statistics table. This count is used 
#' as a basis for the plot.
#' @param order If set to -1 (default), figures are ranked descending 
#' (i.e., figure with most spoken words first). If set to 1, 
#' figures are ranked ascending.
#' @importFrom reshape2 dcast
#' @examples
#' data(rksp.0.text,vndf.0.text)
#' text <- rbind(rksp.0.text,vndf.0.text)
#' stat <- figureStatistics(text, names = TRUE)
#' mat <- figurematrix(stat)
#' # Plot a stacked bar plot
#' b <- barplot(mat$values,col=qd.colors)
#' # Add figure names (if needed/wanted)
#' text(x=b,y=t(mat$cs+(mat$values/2)),labels=t(substr(mat$labels,0,20)))
#' @export
figurematrix <- function(fstat,column="tokens",order=-1) {
  fs <- fstat
  fs$rank <- ave(fs[[column]], fs$drama, FUN=function(x) {rank(order*x, ties.method = "first")})
  mat_values <- as.matrix(dcast(data=fs,rank ~ drama, value.var=column)[,-1])
  mat_labels <- as.matrix(dcast(data=fs,rank ~ drama, value.var="figure")[,-1])
  mat_cs <- apply(mat_values, 2,cumsum)
  mat_cs <- rbind(matrix(0,ncol=ncol(mat_cs)),mat_cs)
  mat_values <- rbind(mat_values,matrix(NA,ncol=ncol(mat_values)))
  list(values=mat_values,labels=mat_labels,cs=mat_cs)
}

#' Adds a column to the figures data frame, containing the rank in the dramatis personae.
#' @param figures The figures to rank
#' @param columnTitle The title for the rank column
#' @export
#' @examples 
#' data(rksp.0.figures)
#' rankFiguresByDramatisPersonae(rksp.0.figures)
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
#' @examples 
#' data(rksp.0.text, rksp.0.figures)
#' rankFiguresByAppearance(rksp.0.figures, rksp.0.text)
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

