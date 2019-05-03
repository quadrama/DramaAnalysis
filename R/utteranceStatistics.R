#' @title Utterance Statistics
#' @description This method calculates the length of each utterance, organised by figure and drama.
#' @param t The dramatic text(s)
#' @param normalizeByDramaLength Logical value. If true, the resulting values will be normalized by the length of the drama.
#' @export
#' @examples
#' data(rksp.0)
#' ustat <- utteranceStatistics(rksp.0$mtext, numberOfFigures = 5)
#' \dontrun{
#' boxplot(ustat$utteranceLength ~ ustat$figure,
#'    col=qd.colors[1:5],
#'    las=2, frame=FALSE)
#' }
utteranceStatistics <- function(drama, normalizeByDramaLength = TRUE) {
  stopifnot(inherits(drama, "QDDrama"))
  
  text <- drama$text

  # normalize by drama length
  if (normalizeByDramaLength == TRUE) {
    ulength <- text[,dl:=.N,.(corpus,drama)][,.N/dl,.(corpus,drama,Speaker.figure_id,utteranceBegin,dl)][,-"dl"]
  } else {    
    ulength <- text[,.N,.(corpus, drama, Speaker.figure_id, utteranceBegin)]
  }
  colnames(ulength) <- c("corpus", "drama", "character", "utteranceBegin", "utteranceLength")

  # skip empty factor levels
  ulength <- droplevels(ulength)
  class(ulength) <- c(c("QDUtteranceStatistics", "QDHasCharacter", "data.frame"))
  ulength
}


utterance_statistics <- function(...) {
  .Deprecated("utteranceStatistics")
  utteranceStatistics(...)
}
