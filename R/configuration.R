#' Creates classic drama configuration matrix. Returns a list with 
#' the three components matrix, drama, and figure
#' @param mtext The text including Act and Scene markings
#' @param by A string, either "Act" or "Scene". Partial matching allowed.
#' @param onlyPresence If TRUE, the resulting matrix only contains 
#' logical values for stage presence
#' @param useCharacterId Logical. If true, characters are represented 
#' by their id instead of their surface form.
#' @param asList Logical, defaults to TRUE. Whether to return as a list 
#' instead of a data frame.
#' @param mode Character vector, should be either "Active" or "Passive".
#' Passive configurations express when characters are mentioned, active
#' ones when they speak themselves.
#' @importFrom stats reshape
#' @export
#' @examples
#' data(rksp.0)
#' cfg <- configuration(rksp.0$mtext)
#' 
configuration <- function(mtext, 
                          by=c("Act", "Scene"), 
                          mode=c("Active", "Passive"),
                          onlyPresence=FALSE, 
                          useCharacterId=FALSE, 
                          asList=TRUE) {
  by <- match.arg(by)
  mode <- match.arg(mode)
  
  # prevent notes in R CMD check
  . <- NULL
  .N <- NULL
  corpus <- NULL
  drama <- NULL
  Speaker.figure_surface <- NULL
  Number.Act <- NULL
  
  t <- mtext

  segmentColumn <- switch(by,
                          Act=quote(Number.Act),
                          Scene=quote(begin.Scene))
  
  
  characterColumn <- switch(mode,
                            Active=quote(Speaker.figure_surface),
                            Passive=quote(Mentioned.figure_surface))
  if (useCharacterId) {
    characterColumn <- switch(mode,
                              Active=quote(Speaker.figure_id),
                              Passive=quote(Mentioned.figure_id))
  }
  
  
  words.per.segment <- t[,.N,.(corpus,drama, eval(characterColumn), eval(segmentColumn))]
  if (mode == "Passive") {
    words.per.segment <- na.omit(words.per.segment)
  }
  cfg <- stats::reshape(words.per.segment, direction="wide", 
                        idvar = c("corpus","drama","characterColumn"), 
                        timevar = "segmentColumn")
  
  cfg[is.na(cfg)] <- 0
  colnames(cfg)[3:(ncol(cfg))] <- c("figure",seq(1,ncol(cfg)-3))
  
  if (onlyPresence) {
    for (col in 4:ncol(cfg)) {
      cfg[[col]] <- as.logical(cfg[[col]])
    }
  }
  
  if (asList) {
    return(list(matrix=as.matrix(cfg[,4:ncol(cfg)]),drama=cfg[,1:2],figure=cfg[[3]]))
  } else {
    #cfg <- data.table::data.table(cfg)
    #data.table::setkey(cfg, "corpus","drama","figure")
    return(cfg)
  }
}

