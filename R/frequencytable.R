#' This function generates a matrix of word frequencies by drama, act or scene and/or by figure.
#' @param t The text table, potentially covering multiple texts
#' @param acceptedPOS A list of accepted pos tags
#' @param names Whether to use figure names or ids
#' @param byFigure Wether the count is by figure or by text
#' @param by Whether the count is by drama (default), act or scene
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
frequencytable <- function(t, acceptedPOS = postags$de$words, names=FALSE, column="Token.surface", byFigure=FALSE, sep="|", normalize=FALSE, sortResult=FALSE, by="drama") {
  ft <- t
  if (length(acceptedPOS) > 0)
    ft <- t[t$Token.pos %in% acceptedPOS,]
  
  switch(by,
         drama = { 
           if (byFigure == FALSE) { xt <- stats::xtabs(~drama + ft[,get(column)], data=ft) }
           else if (names == TRUE) { xt <- stats::xtabs(~ paste(drama,Speaker.figure_surface,sep=sep) + ~ft[,get(column)], data=ft) }
           else { xt <- stats::xtabs(~ paste(drama,Speaker.figure_id,sep=sep) + ~ft[,get(column)], data=ft) }
         },
         act = {
           if (byFigure == FALSE) { xt <- stats::xtabs(~ paste(drama,Number.Act,sep=sep) + ~ft[,get(column)], data=ft) }
           else if (names == TRUE) { xt <- stats::xtabs(~ paste(drama,Number.Act,Speaker.figure_surface,sep=sep) + ~ft[,get(column)], data=ft) }
           else { xt <- stats::xtabs(~ paste(drama,Number.Act,Speaker.figure_id,sep=sep) + ~ft[,get(column)], data=ft) }
         },
         scene = {
           if (byFigure == FALSE) { xt <- stats::xtabs(~ paste(drama,Number.Act,Number.Scene,sep=sep) + ~ft[,get(column)], data=ft) }
           else if (names == TRUE) { xt <- stats::xtabs(~ paste(drama,Number.Act,Number.Scene,Speaker.figure_surface,sep=sep) + ~ft[,get(column)], data=ft) }
           else { xt <- stats::xtabs(~ paste(drama,Number.Act,Number.Scene,Speaker.figure_id,sep=sep) + ~ft[,get(column)], data=ft) }
         },
         stop("Please enter valid string-value for argument 'by' (default = 'drama', 'act' or 'scene').")
  )
  
  r <- as.matrix(stats::ftable(xt, row.vars = c(), col.vars = c()))
  
  if (normalize == TRUE) {
    r <- t(apply(r,1,function(x) { x / sum(x)}))
  } 
  
  if (sortResult == TRUE) {
    r <- r[,order(colSums(r),decreasing = TRUE)]
  }
  
  r
}


#' Extract bigrams instead of words (currently not taking utterance boundaries into account)
#' @param t The text
#' @param acceptedPOS A list of accepted pos tags
#' @param names Whether to use figure names or ids
#' @param byFigure Wether the count is by figure or by text
#' @param by Whether the count is by drama (default), act or scene
#' @param cols The column names we should use (should be either Token.surface or Token.lemma)
#' @keywords internal
frequencytable2 <- function(t, acceptedPOS = postags$de$words, names=FALSE, cols=c("Token.surface", "Token.surface"), byFigure=FALSE, by="drama") {
  ft <- t
  if (length(acceptedPOS) > 0) {
    ft <- t[t$Token.pos %in% acceptedPOS,]
  }
  
  switch(by,
         drama = { 
           if (byFigure == FALSE) { index <- paste(ft$drama) }
           else if (names == TRUE) { index <- paste(ft$drama, ft$Speaker.figure_surface) }
           else { index <- paste(ft$drama, ft$Speaker.figure_id) }
         },
         act = {
           if (byFigure == FALSE) { index <- paste(ft$drama, ft$Number.Act) }
           else if (names == TRUE) { index <- paste(ft$drama, ft$Number.Act, ft$Speaker.figure_surface) }
           else { index <- paste(ft$drama, ft$Number.Act, ft$Speaker.figure_id) }
         },
         scene = {
           if (byFigure == FALSE) { index <- paste(ft$drama, ft$Number.Act, ft$Number.Scene) }
           else if (names == TRUE) { index <- paste(ft$drama, ft$Number.Act, ft$Number.Scene, ft$Speaker.figure_surface) }
           else { index <- paste(ft$drama, ft$Number.Act, ft$Number.Scene, ft$Speaker.figure_id) }
         },
         stop("Please enter valid string-value for argument 'by' (default = 'drama', 'act' or 'scene').")
  )
  
  r <- do.call(rbind, tapply(paste(ft[[cols[1]]], ft[[cols[2]]][-1]), index, function(x){prop.table(table(x))}))
  r[,order(colSums(r),decreasing=TRUE)]
  
}
