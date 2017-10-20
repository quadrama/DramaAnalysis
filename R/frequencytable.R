#' This function generates a matrix of word frequencies by figure or drama.
#' @param t The text table, potentially covering multiple texts
#' @param acceptedPOS A list of accepted pos tags
#' @param names Whether to use figure names or ids
#' @param byFigure Wether the count is by figure or by text
#' @param byScene Wether the count is by scene or by drama
#' @param byAct Whether the count is by act or by drama
#' @param column The column name we should use (should be either Token.surface or Token.lemma)
#' @param sep The separation character that goes between drama name and figure (if applicable)
#' @param normalize Whether to normalize values or not
#' @param sortResult Logical. If true, the columns with the highest sum are ordered left (i.e., frequent words are visible first)
#' @importFrom stats xtabs ftable
#' @examples
#' data(rksp.0)
#' st <- frequencytable(rksp.0$mtext)
#' @examples
#' \dontrun{
#' require(stylo)
#' data(vndf.0.text)
#' tl <- limitFigures(vndf.0.text, by="tokens", threshold=1000)
#' stylo_table <- frequencytable(tl, names=TRUE, byFigure=TRUE)
#' stylo(gui=F, frequencies = stylo_table)
#' }
#' @export
frequencytable <- function(t, acceptedPOS = postags$de$words, names=FALSE, column="Token.surface", byFigure=FALSE, sep="|", normalize=TRUE, sortResult=FALSE, byAct=FALSE, byScene=FALSE) {
  ft <- t
  if (length(acceptedPOS) > 0)
    ft <- t[t$Token.pos %in% acceptedPOS,]
  
  
  if (byFigure == FALSE) {
    
    if (byScene == TRUE) {
      xt <- stats::xtabs(~ paste(drama,Number.Act,Number.Scene,sep=sep) + ~ft[,get(column)], data=ft)
      r <- as.matrix(stats::ftable(xt, row.vars = c(), col.vars = c()))
    } else if (byAct == TRUE) {
      xt <- stats::xtabs(~ paste(drama,Number.Act,sep=sep) + ~ft[,get(column)], data=ft)
      r <- as.matrix(stats::ftable(xt, row.vars = c(), col.vars = c()))
    } else {
      xt <- stats::xtabs(~drama + ft[,get(column)], data=ft)
      r <- as.matrix(stats::ftable(xt, row.vars = c(), col.vars = c()))
    }
    
  } else if (names == TRUE) {
    
    if (byScene == TRUE) {
      xt <- stats::xtabs(~ paste(drama,Number.Act,Number.Scene,Speaker.figure_surface,sep=sep) + ~ft[,get(column)], data=ft)
      r <- as.matrix(stats::ftable(xt, row.vars = c(), col.vars = c()))
    } else if (byAct == TRUE) {
      xt <- stats::xtabs(~ paste(drama,Number.Act,Speaker.figure_surface,sep=sep) + ~ft[,get(column)], data=ft)
      r <- as.matrix(stats::ftable(xt, row.vars = c(), col.vars = c()))
    } else {
      xt <- stats::xtabs(~ paste(drama,Speaker.figure_surface,sep=sep) + ~ft[,get(column)], data=ft)
      r <- as.matrix(stats::ftable(xt, row.vars = c(), col.vars = c()))
    }
    
  } else {
    
    if (byScene == TRUE) {
      xt <- stats::xtabs(~ paste(drama,Number.Act,Number.Scene,Speaker.figure_id,sep=sep) + ~ft[,get(column)], data=ft)
      r <- as.matrix(stats::ftable(xt, row.vars = c(), col.vars = c()))
    } else if (byAct == TRUE) {
      xt <- stats::xtabs(~ paste(drama,Number.Act,Speaker.figure_id,sep=sep) + ~ft[,get(column)], data=ft)
      r <- as.matrix(stats::ftable(xt, row.vars = c(), col.vars = c()))
    } else {
      xt <- stats::xtabs(~ paste(drama,Speaker.figure_id,sep=sep) + ~ft[,get(column)], data=ft)
      r <- as.matrix(stats::ftable(xt, row.vars = c(), col.vars = c()))
    }
    
  }
  
  
  if (normalize == TRUE) {
    r <- t(apply(r,1,function(x) { x / sum(x)}))
  } 
  
  if (sortResult == TRUE) {
    r <- r[,order(colSums(r),decreasing = TRUE)]
  }
  
  r
}