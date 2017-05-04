#' This method calculates the length of each utterance, organised by figure and drama.
#' @param t The dramatic text(s)
#' @param numberOfFigures The maximal number of figures per drama to include. Default: 10. Set to FALSE to include all figures.
#' @param normalizeByDramaLength Logical value. If true, the resulting values will be normalized by the length of the drama.
#' @export
#' @examples
#' data(rksp.0.text)
#' num_figures <- 5
#' ustat <- utteranceStatistics(rksp.0.text, numberOfFigures = num_figures)
#' \dontrun{
#' boxplot(ustat$utterance_length ~ ustat$figure,
#'    col=qd.colors[1:num_figures],
#'    las=2, frame=FALSE)
#' }
#' @importFrom stats aggregate
utteranceStatistics <- function(t, numberOfFigures=10, normalizeByDramaLength = TRUE) {

  if (typeof(numberOfFigures) == "double") {
    t <- limit.figures.by.rank(t, maxRank = numberOfFigures)
  }
  # utterance statistics
  ulength <- aggregate(t$Token.surface, by=list(t$drama, t$Speaker.figure_surface, t$begin, t$length), length)

  colnames(ulength) <- c("drama", "figure", "begin", "drama_length","utterance_length")

  # normalize by drama length
  if (normalizeByDramaLength == TRUE) {
    ulength$utterance_length <- ulength$utterance_length / ulength$drama_length
  }
  # skip empty factor levels
  ulength <- droplevels(ulength)

  # make nicer names
  ulength$figure <- sapply(strsplit(x = as.character(ulength$figure), split="[,.]"),"[", 1)

  # order them by drama and alphabetically
  ulength$figure <- factor(ulength$figure, levels=unique(ulength[order(ulength$drama, ulength$figure),]$figure))

  ulength
}
