#' @title Character Configuration
#' @description The function \code{configuration(...)} Creates drama configuration matrix as a \code{QDConfiguration} object, which is also a data.frame. The S3 function \code{as.matrix()} can be used to extract a numeric or logical matrix containing the core.
#' @param d A \code{QDDrama} object
#' @param segment A character vector, either "Act" or "Scene". Partial matching allowed.
#' @param onlyPresence If TRUE, the function only records whether a character was presence. If FALSE (which is the default), the function counts the number of tokens spoken (active) or referenced (passive).
#' @param mode Character vector, should be either "Active" or "Passive".
#' Passive configurations express when characters are mentioned, active
#' ones when they speak themselves. Please note that extracting passive 
#' configuration only makes sense if some form of coreference resolution 
#' has taken place on the text, either manually or automatic. If not, 
#' only very basic references (first person pronouns and proper names) are 
#' represented, which usually gives a very wrong impression.
#' @importFrom stats reshape
#' @section Active and Passive Configurations:
#' By default, we generate active matrices that are based on 
#' the character speech. A character is present in a scene or 
#' act, if they make an utterance. 
#' Using the argument \code{mode}, we can also create passive
#' configuration matrices. They look very similar, but are based
#' on who's mentioned in a scene or an act. 
#' @return Drama configuration matrix as a \code{QDConfiguration} object (of type \code{data.frame}).
#' @export
#' @rdname configuration
#' @exportClass QDConfiguration
#' @seealso \code{\link{characterNames}}
#' @examples
#' # Active configuration matrix
#' data(rksp.0)
#' cfg <- configuration(rksp.0)
#' 
#' # Passive configuration matrix
#' cfg <- configuration(rksp.0, mode="Passive")
#' 
configuration <- function(d, 
                          segment=c("Act", "Scene"), 
                          mode=c("Active", "Passive"),
                          onlyPresence=FALSE) {

  # check incoming object type
  stopifnot(inherits(d, "QDDrama"))
  stopifnot(numberOfPlays(d) == 1)
  
  # match arguments
  segment <- match.arg(segment)
  mode <- match.arg(mode)
  
  # prevent notes in R CMD check
  . <- NULL
  .N <- NULL
  corpus <- NULL
  Speaker.figure_surface <- NULL
  Number.Act <- NULL
  drama <- NULL
  
  # create a segmented version of the text
  segmented <- switch(mode,
                      Active=segment(d$text, d$segments),
                      Passive=segment(d$mentions, d$segments))
  
  # set the column to separate the segments 
  segmentColumn <- switch(segment,
                          Act=quote(Number.Act),
                          Scene=quote(begin.Scene))
  
  # set the column to identify characters
  characterColumn <- switch(mode,
                            Active=quote(Speaker.figure_id),
                            Passive=quote(entityId))
  
  # extract words per segment
  words.per.segment <- segmented[,.N,
                                 .(corpus, drama, eval(characterColumn), eval(segmentColumn))]

  # reshape the words per segment
  cfg <- stats::reshape(words.per.segment, direction="wide", 
                        idvar = c("corpus","drama","characterColumn"), 
                        timevar = "segmentColumn")
  
  # replace NA values with zero
  for (col in 4:ncol(cfg)) data.table::set(cfg, which(is.na(cfg[[col]])), col, 0)
  
  # set column names
  colnames(cfg)[3:(ncol(cfg))] <- c("character",seq(1,ncol(cfg)-3))
  
  # follow onlyPresence argument
  if (onlyPresence) {
    for (col in 4:ncol(cfg)) {
      cfg[[col]] <- as.logical(cfg[[col]])
    }
  }
  
  # Remove non-characters
  if (mode == "Passive") {
    cfg <- cfg[character %in% d$characters$figure_id]
  }
  
  # set class names
  class(cfg) <- c("QDConfiguration", "QDHasCharacter", "data.frame")
  
  cfg  
}

#' @export
#' @rdname configuration
#' @param x An object of class QDConfiguration
#' @param ... All other arguments are passwd to \code{as.matrix.data.frame}.
as.matrix.QDConfiguration <- function(x, ...) {
  stopifnot(inherits(x, "QDConfiguration"))
  as.matrix.data.frame(x[,4:ncol(x)], ...)
}
