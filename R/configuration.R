#' Creates classic drama configuration matrix. Returns a list with 
#' the three components matrix, drama, and figure
#' @param mtext The text including Act and Scene markings
#' @param by A string, either "Act" or "Scene". Partial matching allowed.
#' @param onlyPresence If TRUE, the resulting matrix only contains 
#' logical values for stage presence
#' @seealso DramaAnalysis::load.text2()
#' @export
#' @examples
#' data(rksp.0)
#' cfg <- configuration(rksp.0$mtext)
#' 
configuration <- function(mtext, by=c("Act", "Scene"), onlyPresence=FALSE) {
  by <- match.arg(by)
  c <- switch(by, 
              Scene=configuration.scene(mtext),
              Act=configuration.act(mtext))
    
  if (onlyPresence)
    c$matrix <- c$matrix>0
  c
}

#' @importFrom stats reshape
configuration.act <- function(mtext) {
  t <- mtext
  words.per.segment <- t[,.N,.(corpus,drama,Speaker.figure_surface, Number.Act)]
  cfg <- stats::reshape(words.per.segment, direction="wide", idvar = c("corpus","drama","Speaker.figure_surface"), timevar = "Number.Act")
  cfg[is.na(cfg)] <- 0
  colnames(cfg)[4:(ncol(cfg))] <- seq(1,ncol(cfg)-3)
  list(matrix=as.matrix(cfg[,4:ncol(cfg)]),drama=cfg[,1:2],figure=as.character(cfg[[3]]))
}

#' @importFrom stats reshape
configuration.scene <- function(text) {
  t <- text
  words.per.segment <- t[,.N,.(corpus,drama,Speaker.figure_surface, begin.Scene)]
  cfg <- stats::reshape(words.per.segment, direction="wide", idvar = c("corpus","drama","Speaker.figure_surface"), timevar = "begin.Scene")
  cfg[is.na(cfg)] <- 0
  colnames(cfg)[4:ncol(cfg)] <- seq(1,(ncol(cfg)-3))
  list(matrix=as.matrix(cfg[,4:ncol(cfg)]),drama=cfg[,1:2],figure=as.character(cfg[[3]]))
}


