#' This function extracts figure statistics from a drama text table.
#' @return A data frame with the following columns and one row for each figure:
#' tokens: The number of tokens spoken by that figure
#' types : The number of different tokens (= types) spoken by each figure
#' utterances: The number of utterances
#' utterance_length_mean: The mean length of utterances
#' utterance_length_sd: The standard deviation in utterance length
#' @param t The drama text
#' @param names If set to true, the table will contains figure names instead of ids
#' @param normalize Normalising the individual columns
#' @importFrom stats sd
#' @importFrom stats aggregate
#' @examples
#' data(rksp.0)
#' stat <- figure.statistics(rksp.0.text, names = FALSE)
#' @export
figure.statistics <- function(t, names = FALSE, normalize = FALSE) {
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
  colnames(r) <- c("drama", "figure","tokens", "types", "utterances", "utterance_length_mean", "utterance_length_sd", "first_begin", "last_end", "length")
  if (normalize == TRUE) {
    r$tokens <- r$tokens / r$length
    r$utterances <- ave(r$utterances, r$drama, FUN=function(x) {x/sum(x)})
    r$first_begin <- r$first_begin / ave(r$last_end, r$drama, FUN=max)
    r$last_end <- ave(r$last_end, r$drama, FUN=function(x) {x/max(x)})
  }
  r
}

#' Adds a column to the figures data frame, containing the rank in the dramatis personae.
#' @param figures The figures to rank
#' @export
#' @examples 
#' data(rksp.0)
#' rank.figures.by.dp(rksp.0.figures)
rank.figures.by.dp <- function(figures, columnTitle="Rank (dramatis personae)") {
  figures[[columnTitle]] <- ave(seq(1:nrow(figures)),figures$drama, FUN=rank)
  figures
}

#' Given a dramatic text and a table of figures, ranks the figures by 
#' their first appearance. The lower the rank number, the earlier the figure appears.
#' "appears": speaks for the first time.
#' @param figures A data frame containing the figures
#' @param text A text data frame
#' @export
#' @examples 
#' data(rksp.0)
#' rank.figures.by.appearance(rksp.0.figures, rksp.0.text)
rank.figures.by.appearance <- function(figures, text, columnTitle="Rank (1st appearance)") {
  minimal.utterance.begin <- aggregate(text$begin, by=list(text$drama,
                                                           text$Speaker.figure_surface),
                                       min)
  colnames(minimal.utterance.begin) <- c("drama", "figure", "begin")
  minimal.utterance.begin[[columnTitle]] <- ave(minimal.utterance.begin$begin, 
                                                         minimal.utterance.begin$drama, FUN=rank)
  minimal.utterance.begin <- subset(minimal.utterance.begin, select=c("figure",columnTitle))
  merge(figures, minimal.utterance.begin, by.x="Figure.surface", by.y="figure")
}

#' @export
figures.first.appearances <- function(texts, acts) {
  acts$Number <- ave(acts$begin, acts$drama, FUN=rank)
  minimal.utterance.begin <- aggregate(texts$begin, by=list(texts$drama, 
                                                            texts$Speaker.figure_surface), 
                                       min)
  mub.at <- merge(minimal.utterance.begin, acts, by.x="Group.1", by.y="drama")
  mub.at <- mub.at[mub.at$x>=mub.at$begin& mub.at$x<=mub.at$end,]
  mub.at
}

