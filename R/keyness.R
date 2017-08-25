#' @title Keywords
#' @description  Given a frequency table (with texts as rows and words as columns),
#' this function calculates log-likelihood and log ratio of one set of rows against the other rows. 
#' The return value is a list containing scores for each word
#' @param ft The frequency table
#' @param row The row number we want to compare to the others, can be a vector of row numbers
#' @param epsilon null values are replaced by this value, in order to avoid division by zero
#' @param siglevel Return only the keywords above the significance level. Set to 1 to get all words
#' @param method Either "logratio" or "loglikelihood" (default)
#' @param minimalFrequency Words less frequent than this value are not considered at all
#' @return A list of keywords, sorted by their log-likelihood value, calculated according to http://ucrel.lancs.ac.uk/llwizard.html
#' @export
#' @importFrom stats pchisq chisq.test
#' @examples 
#' data("rksp.0")
#' ft <- frequencytable(rksp.0$mtext,byFigure = TRUE,names=TRUE,normalize = FALSE)
#' # Calculate log ratio for all words
#' keywords <- keyness(ft, method="logratio", row=7, minimalFrequency = 5)
#' # Remove words that are not significantly different
#' keywords <- keywords[names(keywords) %in% names(keyness(ft, row=1,siglevel=0.01))]
#' 
keyness <- function(ft, row=1, epsilon=1e-100,siglevel=0.05,method="loglikelihood",minimalFrequency=10) {
  f1 <- colSums(matrix(ft[row,],nrow=length(row),dimnames=list(NULL,colnames(ft))))
  f2 <- colSums(matrix(ft[-1*row,],nrow=nrow(ft)-length(row),dimnames=list(NULL,colnames(ft))))

  total1 <- sum(f1)
  total2 <- sum(f2)
  
  if (method=="loglikelihood") {
    f1[f1==0] <- epsilon
    f2[f2==0] <- epsilon
    other1 <- sum(f1) - f1
    other2 <- sum(f2) - f2
    #print(paste("total1",total1))
    #print(paste("total2",total2))
  
    rf <- (f1 + f2) / ( total1 + total2 )
    
    e1 <- total1 * rf
    e2 <- total2 * rf

    l <- 2 * ( ( f1 * log(f1/e1)  ) + (f2 * log(f2/e2)  ))

  
    l <- sort(l,decreasing = TRUE)
    pvalues <- 1-stats::pchisq(l, df=1)
  
    l[pvalues<siglevel]
  } else if (method=="logratio") {
    
    f1[f1<=minimalFrequency] <- 0
    f2[f2<=minimalFrequency] <- 0
    
    rf1 <- f1 / total1
    rf2 <- f2 / total2
    
    r <- sort(log2(rf1/rf2),decreasing = TRUE)
    
    r[is.finite(r)]
  }
}

# R function for calculating LL, by Andrew Hardie, Lancaster University.
# (with apologies for the inevitable R-n00b blunders)

loglikelihood.test = function(O)
{
  DNAME <- deparse(substitute(O))
  
  E = suppressWarnings(chisq.test(O)$expected)
  
  sum = 0;
  
  for(i in 1:length(O[,1]))
  {
    for(j in 1:length(O[1,]))
    {
      if (O[i,j] == 0 || E[i,j] == 0)
        next
      sum = sum + (O[i,j] * log(O[i,j]/E[i,j]))
    }
  }
  STAT = sum * 2;
  
  DF = (length(O[1,]) - 1) * (length(O[,1]) - 1)
  
  P = 1 - pchisq(STAT, df=DF)
  
  names(DF) = "df"
  names(STAT) = "Log-likelihood"
  
  obj =  list(statistic=STAT, parameter=DF, p.value=P, method="Log-Likelihood test", 
              data.name=DNAME, observed=O, expected=E)
  
  attr(obj, "class") <- "htest"
  
  return (obj)
}

