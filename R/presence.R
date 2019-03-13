presenceCore <- function(activeM,passiveM,N) {
  ( rowSums(activeM) - rowSums(passiveM) ) / N
}

#' @title Active and Passive Presence
#' @description This function should be called for a single text. It returns 
#' a data.frame with one row for each character in the play. The 
#' data.frame contains 
#' information about the number of scenes in which a character is actively 
#' speaking or passively mentions.
#' @param mtext A single segmented text
#' @param passiveOnlyWhenNotActive Logical. If true (default), passive presence is only 
#' counted if a character is not actively present in the scene.
#' @export
#' @examples 
#' data(rksp.0)
#' presence(rksp.0$mtext)
presence <- function(drama, passiveOnlyWhenNotActive=TRUE) {
  # prevent notes in R CMD check
  corpus <- NULL
  actives <- NULL
  passives <- NULL
  figure <- NULL
  drama <- NULL
  fref <- NULL
  begin.Scene <- NULL
  .N <- NULL
  . <- NULL
  
  stopifnot(inherits(drama, Drama))
  mtext <- segment(drama$text, drama$segments)

  conf.active <- configuration(mtext, by="Scene", 
                               onlyPresence = TRUE, 
                               useCharacterId = TRUE)
  conf.passive <- passiveConfiguration(drama, by="Scene", 
                                       onlyPresence = TRUE)
  
  meta <- conf.active[,1:3]
  
  conf.passive <- merge(meta, conf.passive, all.x=TRUE)
  for (j in seq_len(ncol(conf.passive)))
    data.table::set(conf.passive, which(is.na(conf.passive[[j]])),j,FALSE)
  
  rownames(conf.active) <- conf.active$figure
  rownames(conf.passive) <- conf.passive$figure
  agg.scenes <- mtext[,.(scenes=length(unique(begin.Scene))),.(corpus,drama)]
  r <- merge(meta, agg.scenes, by=c("corpus","drama"))
  

  # active
  conf.active$actives <- rowSums(conf.active[,4:ncol(conf.active)])
  # passive
  conf.passive$passives <- rowSums(conf.passive[,4:ncol(conf.passive)])

  conf.active <- conf.active[order(conf.active$character)]
  conf.passive <- conf.passive[order(conf.passive$character)]  
  
  r <- merge(r, 
             conf.active[,.(corpus,drama,character,actives)],
             by=c("corpus","drama","character"), all.x = TRUE)
  r <- merge(r, 
             conf.passive[,.(corpus,drama,character,passives)],
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
  
  #r <- merge( r, data.frame(figure=names(passives), passive=passives), by="figure")
  
  r$presence <- ( (r$actives - r$passives) / r$scenes )
  r
}
