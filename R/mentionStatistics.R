#' @title Mention Statistics
#' @description This method calculates the length of each mention, organized by 
#' speaker, entity and drama. 
#' @param drama The dramatic text(s)
#' @param normalizeByDramaLength Logical value. If true, the resulting values will be normalized by the length of the drama.
#' @param unit Specifies the unit in which length should be measured. Should be either words (default) or characters.
#' @export
#' @exportClass QDMentionStatistics
#' @exportClass QDHasCharacter
#' @seealso \code{\link{characterNames}}
#' @return Returns an object of class \code{QDMentionStatistics}, 
#' which is essentially a data.frame.
#' @examples
#' data(rksp.0)
#' mstat <- mentionStatistics(rksp.0)
#' \donttest{
#' boxplot(mstat$mentionLength ~ mstat$speaker,
#'    col=qd.colors,
#'    las=2, frame=FALSE)
#' }
mentionStatistics <- function(drama, normalizeByDramaLength = TRUE, unit = c("words", "characters")) {
  stopifnot(inherits(drama, "QDDrama"))

  # prevent note in check
  `:=` <- NULL
  dl <- NULL
  .N <- NULL
  . <- NULL
  corpus <- NULL
  utteranceSpeakerId <- NULL
  entityId <- NULL
  mentionBegin <- NULL
  
  unit <- match.arg(unit)
  
  text <- drama$mentions

  # Define in which unit length should be measured
  splitBy <- switch(unit,
                    characters = "",
                    words = " ")
  text <- text[, list(mentionSurface = unlist(strsplit(mentionSurface, splitBy))), 
               by=eval(names(text)[names(text) != "mentionSurface"])]
  # normalize by drama length
  if (normalizeByDramaLength == TRUE) {
    mlength <- text[,dl:=.N,.(corpus,drama)][,.N/dl,.(corpus,drama,utteranceSpeakerId,entityId,mentionBegin,dl)][,-"dl"]
  } else {    
    mlength <- text[,.N,.(corpus, drama, utteranceSpeakerId, entityId, mentionBegin)]
  }
  colnames(mlength) <- c("corpus", "drama", "speaker", "entity", "mentionBegin", "mentionLength")

  # skip empty factor levels
  mlength <- droplevels(mlength)
  class(mlength) <- c("QDMentionStatistics", "QDHasCharacter", "data.frame")
  mlength
}
