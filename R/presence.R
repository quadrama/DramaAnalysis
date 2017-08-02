passiveConfiguration <- function(mtext,
                                 matchingFunction=pmatch(tolower(mtext$Token.lemma), 
                                                         tolower(levels(mtext$Speaker.figure_surface)),
                                                         duplicates.ok = TRUE),
                                 onlyPresence=TRUE) {
  t <- mtext
  t$fref <- matchingFunction
  t$fref_n <- levels(t$Speaker.figure_surface)[t$fref]
  agg.passive <- t[,.N,.(corpus,drama,fref,begin.Scene)]
  
  cfg <- stats::reshape(agg.passive, 
                        direction="wide", 
                        idvar = c("corpus","drama","fref"), 
                        timevar = "begin.Scene")
  cfg <- cfg[!is.na(cfg$fref),]
  cfg[is.na(cfg)] <- 0
  colnames(cfg)[3:ncol(cfg)] <- c("figure",1:(ncol(cfg)-3))
  cfg$figure <- levels(t$Speaker.figure_surface)[cfg$figure]
  cfg <- cfg[order(cfg$figure),]
  m <- list(matrix=as.matrix(cfg[,4:ncol(cfg)]),figure=cfg[,3],meta=cfg[,1:3])
  
  if (onlyPresence == TRUE)
    m$matrix <- m$matrix > 0
  m
}

presenceCore <- function(activeM,passiveM,N) {
  ( rowSums(activeM) - rowSums(passiveM) ) / N
}


presence <- function(mtext,presenceFunction=presenceCore) {
  conf.active <- configuration(mtext,by="Scene",onlyPresence = TRUE)
  conf.passive <- passiveConfiguration(mtext)
  
  agg.scenes <- mtext[,.(scenes=length(unique(begin.Scene))),.(corpus,drama)]
  r <- data.table::data.table(conf.passive$meta)
  r <- merge(r,agg.scenes,by.x=c("corpus","drama"),by.y=c("corpus","drama"))
  r$presence <- presenceCore(conf.active$matrix, conf.passive$matrix,r$scenes)
  r
}
