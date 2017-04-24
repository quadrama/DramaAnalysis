
#' Calculates correlation of a frequency table with an outcome list according to given method
#' @param text.ft A matrix, containing words in columns and figures (or plays) in rows
#' @param outcomes A list of outcomes, will be cast as numeric
#' @param minimal.frequency An integer. Words that appear less than x times can be filtered in advance
#' @export
#' @importFrom stats cor
#' @examples
#' data(rksp.0.text)
#' rksp.0.ft <- frequencytable(rksp.0.text, by.figure=TRUE, names=TRUE)
#' g <- factor(c("m","m","m","m","f","f","m","m","m","f","m","m","m","f","m"))
#' rksp.0.cor <- corr.analysis(rksp.0.ft,g)
corr.analysis <- function(text.ft, outcomes,  method="spearman",minimal.frequency=10) {
  text.ft.filtered <- data.frame(text.ft[,colSums(text.ft!=0) > minimal.frequency])
  outcomes <- as.numeric(outcomes)
  text.cor <- cor( text.ft.filtered, y=outcomes, method=method)
  colnames(text.cor) <- c("cor")
  text.cor
} 
