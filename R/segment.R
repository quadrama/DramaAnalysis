#' @title Segment
#' @description This function takes two tables and combines them. The first table is of the 
#' class QDHasUtteranceBE and contains text spans that are designated with begin and end character positions.
#' The second table of class QDHasSegments contains information about acts and scenes in the play.
#' @param hasUtteranceBE Table with utterances
#' @param hasSegments Table with segment info
#' @examples
#' data(rksp.0)
#' segmentedText <- segment(rksp.0$text, rksp.0$segments)
#' @export
segment <- function(hasUtteranceBE, hasSegments) {
  stopifnot(inherits(hasUtteranceBE, "QDHasUtteranceBE"))
  stopifnot(inherits(hasSegments,   "QDHasSegments"))
  
  # prevent notes in check
  begin.Scene <- NULL
  begin.Act <- NULL
  . <- NULL
  `:=` <- NULL
  end.Act <- NULL
  end.Scene <- NULL
  Number.Scene <- NULL
  
  # if scene begin/end field is NA, we replace it with the act begin/end
  # therefore, we don't loose any text
  hasSegments[is.na(begin.Scene),  `:=`(begin.Scene  = begin.Act),]
  hasSegments[is.na(end.Scene),    `:=`(end.Scene    = end.Act),]
  hasSegments[is.na(Number.Scene), `:=`(Number.Scene = 0),]
  
  data.table::setkey(hasUtteranceBE, "corpus", "drama", "utteranceBegin", "utteranceEnd")
  data.table::setkey(hasSegments, "corpus", "drama", "begin.Scene", "end.Scene")
  
  mtext <- data.table::foverlaps(hasUtteranceBE, hasSegments, type="any",
                                 by.x=c("corpus", "drama", "utteranceBegin", "utteranceEnd"), 
                                 by.y=c("corpus", "drama", "begin.Scene", "end.Scene"))
  mtext
}