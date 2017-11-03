#' Creates classic drama configuration matrix. Returns a list with 
#' the three components matrix, drama, and figure
#' @param mtext The text including Act and Scene markings
#' @param by A string, either "Act" or "Scene". Partial matching allowed.
#' @param onlyPresence If TRUE, the resulting matrix only contains 
#' logical values for stage presence
#' @param useCharacterId Logical. If true, characters are represented 
#' by their id instead of their surface form.
#' @export
#' @examples
#' data(rksp.0)
#' cfg <- configuration(rksp.0$mtext)
#' 
configuration <- function(mtext, by=c("Act", "Scene"), onlyPresence=FALSE, useCharacterId=FALSE) {
  by <- match.arg(by)
  c <- switch(by, 
              Scene=configuration.scene(mtext, .useCharacterId=useCharacterId),
              Act=configuration.act(mtext, .useCharacterId=useCharacterId))
    
  if (onlyPresence)
    c$matrix <- c$matrix>0
  c
}

#' @importFrom stats reshape
configuration.act <- function(mtext, .useCharacterId=FALSE) {
  # prevent notes in R CMD check
  . <- NULL
  .N <- NULL
  corpus <- NULL
  drama <- NULL
  Speaker.figure_surface <- NULL
  Number.Act <- NULL
  
  
  t <- mtext
  characterColumn <- quote(Speaker.figure_surface)
  if (.useCharacterId) {
    characterColumn <- quote(Speaker.figure_id)
  }
  words.per.segment <- t[,.N,.(corpus,drama,eval(characterColumn), Number.Act)]
  cfg <- stats::reshape(words.per.segment, direction="wide", 
                        idvar = c("corpus","drama","characterColumn"), 
                        timevar = "Number.Act")
  cfg[is.na(cfg)] <- 0
  colnames(cfg)[3:(ncol(cfg))] <- c(as.character(characterColumn),seq(1,ncol(cfg)-3))
  list(matrix=as.matrix(cfg[,4:ncol(cfg)]),drama=cfg[,1:2],figure=cfg[[3]])
}

#' @importFrom stats reshape
configuration.scene <- function(text, .useCharacterId=FALSE) {
  # prevent notes in R CMD check
  . <- NULL
  .N <- NULL
  corpus <- NULL
  drama <- NULL
  Speaker.figure_surface <- NULL
  begin.Scene <- NULL
  
  
  t <- text
  characterColumn <- quote(Speaker.figure_surface)
  if (.useCharacterId) {
    characterColumn <- quote(Speaker.figure_id)
  }
  words.per.segment <- t[,.N,.(corpus,drama,eval(characterColumn), begin.Scene)]
  cfg <- stats::reshape(words.per.segment, direction="wide", 
                        idvar = c("corpus","drama","characterColumn"), 
                        timevar = "begin.Scene")
  cfg[is.na(cfg)] <- 0
  #cfg <- cfg[order(as.character(cfg$Speaker.figure_surface)),]
  if (length(4:ncol(cfg)) > 0)
    colnames(cfg)[3:ncol(cfg)] <- c(as.character(characterColumn),seq(1,length(4:ncol(cfg))))
  list(matrix=as.matrix(cfg[,4:ncol(cfg)]),drama=cfg[,1:2],figure=as.character(cfg[[3]]))
}


