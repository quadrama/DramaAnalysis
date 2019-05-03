#' @title Character Configuration
#' @description Creates classic drama configuration matrix. Returns either a list with 
#' the three components matrix, drama, and figure, or a data.frame containing everything.
#' @param mtext The text including Act and Scene markings
#' @param by A string, either "Act" or "Scene". Partial matching allowed.
#' @param onlyPresence If TRUE, the resulting matrix only contains 
#' logical values for stage presence
#' @param useCharacterId Logical. If true, characters are represented 
#' by their id instead of their surface form.
#' @param mode Character vector, should be either "Active" or "Passive".
#' Passive configurations express when characters are mentioned, active
#' ones when they speak themselves.
#' @importFrom stats reshape
#' @section Active and Passive Configurations:
#' By default, we generate active matrices that are based on 
#' the character speech. A character is present in a scene or 
#' act, if they make an utterance. 
#' Using the argument \code{mode}, we can also create passive
#' configuration matrices. They look very similar, but are based
#' on who's mentioned in a scene or an act. 
#' @export
#' @exportClass QDConfiguration
#' @examples
#' # Active configuration matrix
#' data(rksp.0)
#' cfg <- configuration(rksp.0)
#' # Passive configuration matrix
#' cfg <- configuration(rksp.0, mode="Passive")
#' 
configuration <- function(d, 
                          by=c("Act", "Scene"), 
                          mode=c("Active", "Passive"),
                          onlyPresence=FALSE) {
  stopifnot(inherits(d, "QDDrama"))
  
  by <- match.arg(by)
  mode <- match.arg(mode)
  
  # prevent notes in R CMD check
  . <- NULL
  .N <- NULL
  corpus <- NULL
  Speaker.figure_surface <- NULL
  Number.Act <- NULL
  
  
  segmented <- switch(mode,
                      Active=segment(d$text, d$segments),
                      Passive=segment(d$mentions, d$segments))
  segmentColumn <- switch(by,
                          Act=quote(Number.Act),
                          Scene=quote(begin.Scene))
  characterColumn <- switch(mode,
                            Active=quote(Speaker.figure_id),
                            Passive=quote(entityId))
  
  words.per.segment <- segmented[,.N,
                                 .(corpus,drama, eval(characterColumn), eval(segmentColumn))]
  #if (mode == "Passive") {
  #  words.per.segment <- na.omit(words.per.segment)
  #}
  cfg <- stats::reshape(words.per.segment, direction="wide", 
                        idvar = c("corpus","drama","characterColumn"), 
                        timevar = "segmentColumn")
  
  # replace NA values with zero
  for (col in 4:ncol(cfg)) data.table::set(cfg, which(is.na(cfg[[col]])), col, 0)
  colnames(cfg)[3:(ncol(cfg))] <- c("character",seq(1,ncol(cfg)-3))
  
  if (onlyPresence) {
    for (col in 4:ncol(cfg)) {
      cfg[[col]] <- as.logical(cfg[[col]])
    }
  }
  
  class(cfg) <- c(Configuration, "data.frame")
  
  cfg  
}



#' @export
as.list.QDConfiguration <- function(configuration) {
  stopifnot(inherits(configuration, Configuration))
  ret <- list()
  ret$matrix <- as.matrix(configuration[,4:ncol(configuration)])
  ret$drama <- configuration[,1:2]
  ret$character <- configuration[[3]]
  ret
}
