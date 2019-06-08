
#' @title Correlation analysis
#' @description Calculates correlation of a frequency table with an outcome list according to given method. The function currently only works for pairwise correlation, i.e., two categories.
#' @param text.ft A matrix, containing words in columns and figures (or plays) in rows. 
#' This can be the result of the \code{\link{frequencytable}()} function.
#' @param categories A factor or numeric vector that represents a list of categories. 
#' @param culling An integer. Words that appear in less items are removed. 
#' Defaults to 0 which doesn't remove anything.
#' @param method The correlation method, passed on to cor()
#' @param ... Arguments passed to \code{\link{cor}()}
#' @return The function returns a data.frame with three columns: The word, 
#' it's correlation score, and the category it is correlated to. The latter is 
#' mainly for an easier use of the results.
#' @export
#' @importFrom stats cor
#' @examples
#' data(rksp.0)
#' ft <- frequencytable(rksp.0, byFigure=TRUE)
#' g <- factor(c("m","m","m","m","f","m","m","m","f","m","m","f","m"))
#' rksp.0.cor <- correlationAnalysis(ft, g)
#' 
#' # to pre-filter by the total frequency of a word
#' ft <- frequencytable(rksp.0, byFigure=TRUE)
#' ft <- ft[,colSums(ft) > 5]
#' correlationAnalysis(ft, g)
correlationAnalysis <- function(text.ft, 
                                categories,  
                                method = "spearman", 
                                culling = 0, 
                                ...) {
  # filter rare words
  if (culling > 0) {
    text.ft.filtered <- data.frame(text.ft[,colSums(text.ft!=0) > culling])
  } else {
    text.ft.filtered <- text.ft
  }
  
  # make categories numeric
  nCategories <- as.numeric(categories)
  
  # calculate correlation
  text.cor <- data.frame( stats::cor( text.ft.filtered, y=nCategories, method=method, ...) )
  colnames(text.cor) <- c("cor")
  text.cor$word <- rownames(text.cor)
  
  # determine direction
  if (is.factor(categories)) {
    text.cor$category <- ifelse(text.cor$cor > 0, 
                                levels(categories)[max(nCategories)], 
                                levels(categories)[min(nCategories)])
  } else if (is.numeric(categories)) {
    text.cor$category <- ifelse(text.cor$cor > 0, 
                                max(categories), 
                                min(categories))
  }
  
  # order by correlation
  text.cor <- text.cor[order(text.cor$cor),]
  text.cor
} 
