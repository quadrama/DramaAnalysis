#' This function generates a matrix of word frequencies by figure or drama.
#' @param t The text table, potentially covering multiple texts
#' @param accepted.pos A list of accepted pos tags
#' @param names Whether to use figure names or ids
#' @param by.figure Wether the count is by figure or by text
#' @param column The column name we should use (should be either Token.surface or Token.lemma)
#' @param sep The separation character that goes between drama name and figure (if applicable)
#' @examples
#' data(rksp.0.text)
#' st <- frequencytable(rksp.0.text)
#' @examples
#' \dontrun{
#' require(stylo)
#' data(vndf.0.text)
#' tl <- limit.figures.by.tokens(vndf.0.text, minTokens=1000)
#' stylo_table <- frequencytable(tl, names=TRUE, by.figure=TRUE)
#' stylo(gui=F, frequencies = stylo_table)
#' }
#' @export
frequencytable <- function(t, accepted.pos = postags$de$words, names=FALSE, column="Token.surface", by.figure=FALSE, sep="|") {
  ft <- t
  if (length(accepted.pos) > 0)
    ft <- t[t$Token.pos %in% accepted.pos,]
  if (by.figure == FALSE) {
    index <- list(ft$drama)
  } else if (names == TRUE) {
    index <- paste(ft$drama, ft$Speaker.figure_surface,sep=sep)
  } else
    index <- list(ft$drama, ft$Speaker.figure_id)
  r <- do.call(rbind, tapply(ft[[column]], index, function(x){prop.table(table(x))}))
  as.matrix(r[,order(colSums(r),decreasing=TRUE)])
}


#' Extract bigrams instead of words (currently not taking utterance boundaries into account)
#' @export
#' @param t The text
#' @param accepted.pos A list of accepted pos tags
#' @param names Whether to use figure names or ids
#' @param by.figure Wether the count is by figure or by text
#' @param cols The column names we should use (should be either Token.surface or Token.lemma)
frequencytable2 <- function(t, accepted.pos = postags$de$words, names=FALSE, cols=c("Token.surface", "Token.surface"), by.figure=FALSE) {
  ft <- t
  if (length(accepted.pos) > 0)
    ft <- t[t$Token.pos %in% accepted.pos,]
  if (by.figure == FALSE)
    index <- paste(ft$drama)
  else if (names == TRUE)
    index <- paste(ft$drama, ft$Speaker.figure_surface)
  else
    index <- paste(ft$drama, ft$Speaker.figure_id)
  r <- do.call(rbind, tapply(paste(ft[[cols[1]]], ft[[cols[2]]][-1]), index, function(x){prop.table(table(x))}))
  r[,order(colSums(r),decreasing=TRUE)]
}