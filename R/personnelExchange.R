#' @name personnelExchange
#' @title Measuring Personnel Exchange over Boundaries
#' @aliases hamming
#' @aliases scenicDifference
#' @description There are multiple ways to quantify the number of characters that are 
#' exchanged over a scene or act boundary. 
#' @param drama The QDDrama Object
#' @param variant For \code{hamming()}, variants are "Trilcke" (default), "NormalizedHamming", and "Hamming"
#' @param norm For \code{scenicDifference()}, specifies the normalization constant
#' @rdname personnelExchange
#' @return A QDHamming object, which is a list of values, one for each scene change.
#' The values indicate the (potentially) normalized number of characters that are exchanged.
#' @export
#' @examples 
#' data(rksp.0)
#' dist_trilcke  <- hamming(rksp.0)
#' dist_hamming  <- hamming(rksp.0, variant = "Hamming")
#' dist_nhamming <- hamming(rksp.0, variant = "NormalizedHamming")
hamming <- function(drama, variant=c("Trilcke","Hamming","NormalizedHamming")) {
  stopifnot(inherits(drama, "QDDrama"))
  stopifnot(numberOfPlays(drama) == 1)
  
  numberOfFigures <- nrow(drama$characters)
  scenes <- nrow(drama$segments)
  pm <- as.matrix(configuration(drama, segment="Scene", onlyPresence = TRUE))
  vec <- vector(mode="integer",length=scenes-1)
  variant <- match.arg(variant)
  for (i in 1:(ncol(pm)-1)) {
    allFigures <- switch(variant,
                         Trilcke=sum(pm[,i] | pm[,i+1]),
                         Hamming=1,
                         NormalizedHamming=numberOfFigures)
    edits <- sum(xor(pm[,i],pm[,i+1]))
    vec[i] <- edits/allFigures
  }
  class(vec) <- append("QDHamming", class(vec))
  vec
}

#' @rdname personnelExchange
#' @export
scenicDifference <- function(drama, norm=length(unique(drama$text$Speaker.figure_id))) {
  stopifnot(inherits(drama, "QDDrama"))
  
  numberOfFigures <- nrow(drama$characters)
  scenes <- nrow(drama$segments)
  pm <- as.matrix(configuration(drama, segment="Scene", onlyPresence = TRUE))
  vec <- vector(mode="integer",length=scenes-1)
  for (i in 1:(ncol(pm)-1)) {
    same <- sum(pm[,i] & pm[,i+1])
    vec[i] <- numberOfFigures - same
  }
  vec/norm
}
