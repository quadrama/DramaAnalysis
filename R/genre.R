
#' @title Correlation analysis
#' @description Calculates correlation of a frequency table with an outcome list according to given method
#' @param text.ft A matrix, containing words in columns and figures (or plays) in rows. 
#' This can be the result of the \code{\link{frequencytable}} function.
#' @param outcomes A list of outcomes, will be cast as numeric
#' @param minimalFrequency An integer. Words that appear less than x times can be filtered in advance
#' @param method The correlation method, passed on to cor()
#' @export
#' @importFrom stats cor
#' @examples
#' data(rksp.0)
#' rksp.0.ft <- frequencytable(rksp.0, byFigure=TRUE)
#' g <- c("m","m","m","m","f","m","m","m","f","m","m","f","m")
#' rksp.0.cor <- correlationAnalysis(rksp.0.ft, g)
correlationAnalysis <- function(text.ft, 
                                outcomes,  
                                method="spearman", 
                                minimalFrequency = 10) {
  text.ft.filtered <- data.frame(text.ft[,colSums(text.ft!=0) > minimalFrequency])
  outcomes <- as.numeric(outcomes)
  text.cor <- data.frame(cor( text.ft.filtered, y=outcomes, method=method))
  colnames(text.cor) <- c("cor")
  text.cor$word <- rownames(text.cor)
  text.cor <- text.cor[order(text.cor$cor),]
  text.cor
} 
