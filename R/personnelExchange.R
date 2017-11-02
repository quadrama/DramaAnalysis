#' @name personnelExchange
#' @title Measuring Personnel Exchange over Boundaries
#' @aliases hamming
#' @aliases scenicDifference
#' @description There are multiple ways to quantify the number of characters that are 
#' exchanged over a scene or act boundary. 
#' @param mtext The segmented drama text
#' @param variant For hamming(), variants are "Trilcke" (default), "NormalizedHamming", and "Hamming"
#' @param norm For scenicDifference(), specifies the normalization constant
#' @rdname personnelExchange
#' @export
#' @examples 
#' data(rksp.0)
#' dist_trilcke  <- hamming(rksp.0$mtext)
#' dist_hamming  <- hamming(rksp.0$mtext, variant = "Hamming")
#' dist_nhamming <- hamming(rksp.0$mtext, variant = "NormalizedHamming")
hamming <- function(mtext, variant=c("Trilcke","Hamming","NormalizedHamming")) {
  mtext <- na.omit(mtext)
  numberOfFigures <- length(unique(mtext$Speaker.figure_surface))
  scenes <- length(unique(mtext$begin.Scene))
  pm <- configuration(mtext, by="Scene", onlyPresence = TRUE)
  vec <- vector(mode="integer",length=scenes-1)
  variant <- match.arg(variant)
  for (i in 1:(ncol(pm$matrix)-1)) {
    allFigures <- switch(variant,
                         Trilcke=sum(pm$matrix[,i] | pm$matrix[,i+1]),
                         Hamming=1,
                         NormalizedHamming=numberOfFigures)
    edits <- sum(xor(pm$matrix[,i],pm$matrix[,i+1]))
    vec[i] <- edits/allFigures
  }
  vec
}

#' @rdname personnelExchange
#' @export
scenicDifference <- function(mtext, norm=length(unique(mtext$Speaker.figure_surface))) {
  mtext <- na.omit(mtext)
  numberOfFigures <- length(unique(mtext$Speaker.figure_surface))
  scenes <- length(unique(mtext$begin.Scene))
  
  pm <- configuration(mtext, by="Scene", onlyPresence = TRUE)
  vec <- vector(mode="integer",length=scenes-1)
  for (i in 1:(ncol(pm$matrix)-1)) {
    same <- sum(pm$matrix[,i] & pm$matrix[,i+1])
    vec[i] <- numberOfFigures - same
  }
  vec/norm
}
