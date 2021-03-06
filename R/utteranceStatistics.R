#' @title Utterance Statistics
#' @description This method calculates the length of each utterance, organized by 
#' character and drama. 
#' @param drama The dramatic text(s)
#' @param normalizeByDramaLength Logical value. If true, the resulting values will be normalized by the length of the drama.
#' @export
#' @exportClass QDUtteranceStatistics
#' @exportClass QDHasCharacter
#' @seealso \code{\link{characterNames}}
#' @return Returns an object of class \code{QDUtteranceStatistics}, 
#' which is essentially a data.frame.
#' @examples
#' data(rksp.0)
#' ustat <- utteranceStatistics(rksp.0)
#' \donttest{
#' boxplot(ustat$utteranceLength ~ ustat$character,
#'    col=qd.colors[1:5],
#'    las=2, frame=FALSE)
#' }
utteranceStatistics <- function(drama, normalizeByDramaLength = TRUE) {
  stopifnot(inherits(drama, "QDDrama"))

  # prevent note in check
  `:=` <- NULL
  dl <- NULL
  .N <- NULL
  . <- NULL
  corpus <- NULL
  Speaker.figure_id <- NULL
  utteranceBegin <- NULL
  
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
  class(ulength) <- c("QDUtteranceStatistics", "QDHasCharacter", "data.frame")
  ulength
}
