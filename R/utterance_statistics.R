#' This method calculates the length of each utterance, organised by figure and drama.
#' @param t The dramatic text(s)
#' @param num.figures The maximal number of figures per drama to include. Default: 10. Set to FALSE to include all figures.
#' @param normalize.by.drama.length Logical value. If true, the resulting values will be normalized by the length of the drama.
#' @export
#' @examples
#' data(rksp.0)
#' num_figures <- 5
#' ustat <- utterance_statistics(rksp.0, num.figures = num_figures)
#' boxplot(ustat$utterance_length ~ ustat$figure,col=qd.colors[1:num_figures], las=2,frame=FALSE)
#' @importFrom stats aggregate
utterance_statistics <- function(t, num.figures=10, normalize.by.drama.length = TRUE) {

  if (typeof(num.figures) == "double") {
    t <- limit.figures.by.rank(t, maxRank = num.figures)
  }
  # utterance statistics
  ulength <- aggregate(t$Token.surface, by=list(t$drama, t$Speaker.figure_surface, t$begin, t$length), length)

  colnames(ulength) <- c("drama", "figure", "begin", "drama_length","utterance_length")

  # normalize by drama length
  if (normalize.by.drama.length == TRUE) {
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
