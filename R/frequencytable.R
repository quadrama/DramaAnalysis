#' This function generates a matrix of word frequencies by figure or drama.
#' @param t The text table, potentially covering multiple texts
#' @param acceptedPOS A list of accepted pos tags
#' @param names Whether to use figure names or ids
#' @param byFigure Wether the count is by figure or by text
#' @param column The column name we should use (should be either Token.surface or Token.lemma)
#' @param sep The separation character that goes between drama name and figure (if applicable)
#' @param normalize Whether to normalize values or not
#' @examples
#' data(rksp.0.text)
#' st <- frequencytable(rksp.0.text)
#' @examples
#' \dontrun{
#' require(stylo)
#' data(vndf.0.text)
#' tl <- limitFiguress(vndf.0.text, by="tokens", threshold=1000)
#' stylo_table <- frequencytable(tl, names=TRUE, byFigure=TRUE)
#' stylo(gui=F, frequencies = stylo_table)
#' }
#' @export
frequencytable <- function(t, acceptedPOS = postags$de$words, names=FALSE, column="Token.surface", byFigure=FALSE, sep="|", normalize=TRUE) {
  ft <- t
  if (length(acceptedPOS) > 0)
    ft <- t[t$Token.pos %in% acceptedPOS,]
  
  if (byFigure == FALSE) {
    xt <- xtabs(~drama + ft[,get(column)], data=ft)
    r <- as.matrix(ftable(xt, row.vars = c(), col.vars = c()))
  } else if (names == TRUE) {
    xt <- xtabs(~ paste(drama,Speaker.figure_surface,sep=sep) + ~ft[,get(column)], data=ft)
    r <- as.matrix(ftable(xt, row.vars = c(), col.vars = c()))
  } else {
    xt <- xtabs(~paste(drama,Speaker.figure_id,sep=sep)+ft[,get(column)], data=ft)
    r <- as.matrix(ftable(xt, row.vars = c(), col.vars = c()))
  }
  
  
  if (normalize==TRUE) {
    t(apply(r,1,function(x) { x / sum(r)}))
  } else {
    r
  }
}


#' Extract bigrams instead of words (currently not taking utterance boundaries into account)
#' @export
#' @param t The text
#' @param acceptedPOS A list of accepted pos tags
#' @param names Whether to use figure names or ids
#' @param byFigure Wether the count is by figure or by text
#' @param cols The column names we should use (should be either Token.surface or Token.lemma)
frequencytable2 <- function(t, acceptedPOS = postags$de$words, names=FALSE, cols=c("Token.surface", "Token.surface"), byFigure=FALSE) {
  ft <- t
  if (length(acceptedPOS) > 0)
    ft <- t[t$Token.pos %in% acceptedPOS,]
  if (byFigure == FALSE)
    index <- paste(ft$drama)
  else if (names == TRUE)
    index <- paste(ft$drama, ft$Speaker.figure_surface)
  else
    index <- paste(ft$drama, ft$Speaker.figure_id)
  r <- do.call(rbind, tapply(paste(ft[[cols[1]]], ft[[cols[2]]][-1]), index, function(x){prop.table(table(x))}))
  r[,order(colSums(r),decreasing=TRUE)]
}