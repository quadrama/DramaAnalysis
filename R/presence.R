#' @title Active and Passive Presence
#' @description This function should be called for a single text. It returns 
#' a data.frame with one row for each character in the play. The 
#' data.frame contains 
#' information about the number of scenes in which a character is actively 
#' speaking or passively mentions. Please note that the information about
#' passive presence is derived from coreference resolved texts, which is a
#' difficult task and not entirely reliable. The plays included in the package
#' feature manually annotated coreferences (and thus, the presence is calculated on
#' the basis of very well data).
#' @param drama A single drama
#' @param passiveOnlyWhenNotActive Logical. If true (default), passive presence is only 
#' counted if a character is not actively present in the scene.
#' @return QDHasCharacter, data.frame. Columns \code{actives}, \code{passives} and 
#' \code{scenes} show the 
#' absolute number of scenes in which a character is actively/passively present, or the
#' total number of scenes in the play. The column \code{presence} is calculated as 
#' \eqn{\frac{actives-passives}{scenes}}{(actives-passives)/scenes}.
#' 
#' @export
#' @examples 
#' data(rksp.0)
#' presence(rksp.0)
presence <- function(drama, 
                     passiveOnlyWhenNotActive = TRUE) {
  # prevent notes in R CMD check
  corpus <- NULL
  actives <- NULL
  passives <- NULL
  figure <- NULL
  fref <- NULL
  begin.Scene <- NULL
  .N <- NULL
  . <- NULL
  
  stopifnot(inherits(drama, "QDDrama"))

  if (nrow(drama$mentions) == 0) {
    warning("Mentions table was empty, no presence calculation possible.")
    return(NA)
  }
  
  conf.active <- configuration(drama, segment="Scene", 
                               mode="Active",
                               onlyPresence = TRUE)
  conf.passive <- configuration(drama, segment="Scene", 
                                mode="Passive",
                                onlyPresence = TRUE)
  
  meta <- conf.active[,1:3]
  
  conf.passive <- merge(meta, conf.passive, all.x=TRUE)
  for (j in seq_len(ncol(conf.passive)))
    data.table::set(conf.passive, which(is.na(conf.passive[[j]])),j,FALSE)
  
  rownames(conf.active) <- conf.active$character
  rownames(conf.passive) <- conf.passive$character
  r <- merge(meta, drama$segments[,.(scenes=length(unique(begin.Scene))),.(corpus,drama)], by=c("corpus","drama"))


  # active
  conf.active$actives <- rowSums(conf.active[,4:ncol(conf.active)])
  # passive
  conf.passive$passives <- rowSums(conf.passive[,4:ncol(conf.passive)])

  #conf.active <- conf.active[order(conf.active$character)]
  #conf.passive <- conf.passive[order(conf.passive$character)]  
  r <- merge(r, 
             conf.active[,c("corpus","drama","character","actives")],
             by=c("corpus","drama","character"), all.x = TRUE)
  r <- merge(r, 
             conf.passive[,c("corpus","drama","character","passives")],
             by=c("corpus","drama","character"), all.x = TRUE)
  
  
  if (passiveOnlyWhenNotActive) {
    actives.mat <- as.matrix(conf.active[,4:(ncol(conf.active)-1)])
    passives.mat <- as.matrix(conf.passive[,4:(ncol(conf.passive)-1)])
    actives.which <- lapply(split(actives.mat, seq(nrow(actives.mat))), 
                            function(x) {which(unlist(x))})
    passives.which <- lapply(split(passives.mat, seq(nrow(passives.mat))), 
                            function(x) {which(unlist(x))})
    
    overlaps <- mapply(function(x,y) { intersect(x,y) }, actives.which, passives.which )
    overlaps.cnt <- as.vector(Reduce(rbind,lapply(overlaps, length)))
    names(overlaps.cnt) <- names(overlaps)
    r$passives <- r$passives - overlaps.cnt
  }
  
  r$presence <- ( (r$actives - r$passives) / r$scenes )
  class(r) <- c("QDHasCharacter", class(r))
  r
}
