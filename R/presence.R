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
presence <- function(mtext, passiveOnlyWhenNotActive=TRUE) {
  
  # prevent notes in R CMD check
  corpus <- NULL
  drama <- NULL
  fref <- NULL
  begin.Scene <- NULL
  .N <- NULL
  . <- NULL
  

  conf.active <- configuration(mtext, by="Scene", 
                               onlyPresence = TRUE, 
                               useCharacterId = TRUE, 
                               asList = FALSE)
  conf.passive <- configuration(mtext, by="Scene", 
                                mode="Passive", 
                                onlyPresence=TRUE, 
                                useCharacterId = TRUE, 
                                asList = FALSE)
  meta <- conf.active[,1:3]
  
  conf.passive <- merge(meta, conf.passive, all.x=TRUE)
  for (j in seq_len(ncol(conf.passive)))
    data.table::set(conf.passive,which(is.na(conf.passive[[j]])),j,FALSE)
  
  rownames(conf.active) <- conf.active$figure
  rownames(conf.passive) <- conf.passive$figure
  agg.scenes <- mtext[,.(scenes=length(unique(begin.Scene))),.(corpus,drama)]
  r <- merge(meta, agg.scenes, by=c("corpus","drama"))
  

  # active
  conf.active$actives <- rowSums(conf.active[,4:ncol(conf.active)])
  # passive
  conf.passive$passives <- rowSums(conf.passive[,4:ncol(conf.passive)])

  r <- merge(r, 
             conf.active[,.(corpus,drama,figure,actives)],
             by=c("corpus","drama","figure"), all.x = TRUE)
  r <- merge(r, 
             conf.passive[,.(corpus,drama,figure,passives)],
             by=c("corpus","drama","figure"), all.x = TRUE)
  
  
  if (passiveOnlyWhenNotActive) {
    actives.which <- apply(conf.active[,4:(ncol(conf.active)-1)], 1, which)
    passives.which <- apply(conf.passive[,4:(ncol(conf.passive)-1)], 1, which)
    
    overlaps <- mapply(function(x,y) { intersect(x,y) }, actives.which, passives.which )
    overlaps.cnt <- as.vector(Reduce(rbind,lapply(overlaps, length)))
    names(overlaps.cnt) <- names(overlaps)
    r$passives <- r$passives - overlaps.cnt
  }
  
  #r <- merge( r, data.frame(figure=names(passives), passive=passives), by="figure")
  
  r$presence <- ( (r$actives - r$passives) / r$scenes )
  r
}
