#' This function generates a table to be used by stylo
#' @param t The text table, potentially covering multiple texts
#' @param accepted.pos A list of accepted pos tags
#' @param names Whether to use figure names or ids
#' @param by.figure Wether the count is by figure or by text
#' @param column The column name we should use (should be either Token.surface or Token.lemma)
#' @export
#' @examples
#' data(rksp.0)
#' st <- frequency.table(rksp.0)
#' @examples
#' t <- load.text(read.csv("http://localhost:8080/drama.web/dramas", header=FALSE)[,], tokens=T)
#' tl <- limit.figures.by.tokens(t, minTokens=1000)
#' stylo_table <- frequency.table(tl, names=TRUE, by.figure=TRUE)
#' stylo(gui=F, frequencies = stylo_table, network=T, write.png.file=T, analysis.type="BCT")
frequency.table <- function(t, accepted.pos = postags$de$words, names=FALSE, column="Token.surface", by.figure=FALSE) {
  ft <- t
  if (length(accepted.pos) > 0)
    ft <- t[t$Token.pos %in% accepted.pos,]
  if (by.figure == FALSE)
    index <- paste(ft$drama)
  else if (names == TRUE)
    index <- paste(ft$drama, ft$Speaker.figure_surface)
  else
    index <- paste(ft$drama, ft$Speaker.figure_id)
  r <- do.call(rbind, tapply(ft[[column]], index, function(x){prop.table(table(x))}))
  r[,order(colSums(r),decreasing=TRUE)]
}
