#' Creates classic drama configuration matrix. Returns a list with 
#' the three components matrix, drama, and figure
#' @param mtext The text including Act and Scene markings
#' @param by A string, either "Act" or "Scene"
#' @param onlyPresence If TRUE, the resulting matrix only contains 
#' logical values for stage presence
#' @seealso DramaAnalysis::load.text2()
#' @export
#' @examples
#' data(rksp.0)
#' cfg <- configuration(rksp.0$mtext)
#' 
configuration <- function(mtext, by="Act", onlyPresence=FALSE) {
  
  if (by=="Scene") {
    c <- configuration.scene(mtext)
  } else {
    c <- configuration.act(mtext)
  }
  if (onlyPresence)
    c$matrix <- c$matrix>0
  c
}

#' @importFrom stats reshape
configuration.act <- function(mtext) {
  t <- mtext
  words.per.segment <- aggregate(Token.surface ~ drama + Speaker.figure_surface + Number.Act, 
                                 data=t, length)
  cfg <- stats::reshape(words.per.segment, direction="wide", idvar = c("drama","Speaker.figure_surface"), timevar = "Number.Act")
  cfg[is.na(cfg)] <- 0
  colnames(cfg) <- c("drama", "Speaker.figure_surface",seq(1,(ncol(cfg)-2)))
  list(matrix=as.matrix(cfg[,3:ncol(cfg)]),drama=cfg[,1],figure=cfg[,2])
}

#' @importFrom stats reshape
configuration.scene <- function(text) {
  t <- text
  bylist = list(t$drama, t$Speaker.figure_surface, paste0(t$Number.Act,"-", formatC(t$Number.Scene, width=2)))
  words.per.segment <- aggregate(t$Token.surface, 
                                 by=bylist, 
                                 length)
  cfg <- stats::reshape(words.per.segment, direction="wide", idvar = c("Group.1","Group.2"), timevar = "Group.3")
  cfg[is.na(cfg)] <- 0
  colnames(cfg) <- c("drama", "Speaker.figure_surface",seq(1,(ncol(cfg)-2)))
  list(matrix=as.matrix(cfg[,3:ncol(cfg)]),drama=cfg[,1],figure=cfg[,2])
  
}