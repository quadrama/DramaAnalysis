#' color scheme to be used for QuaDramA plots
#' Taken from http://google.github.io/palette.js/, tol-rainbow, 10 colors
#' @export
qd.colors <- c(rgb(120,28,129, maxColorValue = 255),
               rgb(67, 50, 141, maxColorValue = 255),
               rgb(65, 111, 184, maxColorValue = 255),
               rgb(81, 156, 184, maxColorValue = 255),
               rgb(112, 180, 132, maxColorValue = 255),
               rgb(153, 189, 92, maxColorValue = 255),
               rgb(195, 186, 69, maxColorValue = 255),
               rgb(224, 162, 57, maxColorValue = 255),
               rgb(230, 107, 45, maxColorValue = 255),
               rgb(217, 33, 32, maxColorValue = 255)
              );



#' This method removes the spoken tokens of all but the most frequent n figures
#' @param t The text, a data frame listing each token for each figure
#' @param maxRank Up to maxRank figures remain in the data set
#' @export
#' @importFrom utils head
#' @examples
#' data(rksp.0)
#' t <- limit.figures.by.rank(rksp.0)
limit.figures.by.rank <- function(t, maxRank=10) {
  counts <- aggregate(t$Speaker.figure_surface, by=list(t$drama, t$Speaker.figure_id), length)
  counts <- counts[order(counts$x, decreasing = TRUE),]
  rcounts <- Reduce(rbind, by(counts, counts["Group.1"], head, n=maxRank))
  t[paste(t$drama, t$Speaker.figure_id) %in% paste(rcounts$Group.1, rcounts$Group.2),]
}

#' This method removes the spoken tokens by all figures that speak infrequently.
#' @param t The text, a data frame listing each token for each figure
#' @param minTokens The minimal amount of tokens a figure has to speak
#' @export
#' @examples
#' data(rksp.0)
#' limit.figures.by.tokens(rksp.0)
limit.figures.by.tokens <- function(t, minTokens=100) {
    counts <- tapply(t$Speaker.figure_surface, paste(t$drama, t$Speaker.figure_id), length)
    write(paste(length(counts[counts > minTokens]), "remaining."),stderr())
    subset(t, counts[paste(t$drama, t$Speaker.figure_id)] > minTokens )
}






