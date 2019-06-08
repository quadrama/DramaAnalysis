#' @title Keywords
#' @description  Given a frequency table (with texts as rows and words as columns),
#' this function calculates log-likelihood and log ratio of one set of rows against the other rows. 
#' The return value is a list containing scores for each word. If the method 
#' is \code{loglikelihood}, the returned scores are unsigned G2 values. To estimate the 
#' \emph{direction} of the keyness, the \code{log ratio} is more informative. A nice introduction 
#' into log ratio can be found \href{http://cass.lancs.ac.uk/log-ratio-an-informal-introduction/}{here}.
#' @param ft The frequency table
#' @param categories A factor or numeric vector that represents an assignment of categories. 
#' @param epsilon null values are replaced by this value, in order to avoid division by zero
#' @param siglevel Return only the keywords above the significance level. Set to 1 to get all words
#' @param method Either "logratio" or "loglikelihood" (default)
#' @param minimalFrequency Words less frequent than this value are not considered at all
#' @return A list of keywords, sorted by their log-likelihood or log ratio value, calculated according to \href{http://ucrel.lancs.ac.uk/llwizard.html}{http://ucrel.lancs.ac.uk/llwizard.html}.
#' @export
#' @importFrom stats pchisq chisq.test
#' @examples 
#' data("rksp.0")
#' ft <- frequencytable(rksp.0, byFigure = TRUE, normalize = FALSE)
#' # Calculate log ratio for all words
#' genders <- factor(c("m", "m", "m", "m", "f", "m", "m", "m", "f", "m", "m", "f", "m"))
#' keywords <- keyness(ft, method = "logratio", 
#'                     categories = genders, 
#'                     minimalFrequency = 5)
#' # Remove words that are not significantly different
#' keywords <- keywords[names(keywords) %in% names(keyness(ft, siglevel = 0.01))]
#' 
keyness <- function(ft, 
                    categories = c(1, rep(2, nrow(ft)-1)),
                    epsilon = 1e-100,
                    siglevel = 0.05,
                    method = c("loglikelihood", "logratio"),
                    minimalFrequency = 10) {

  # match method agument
  method <- match.arg(method)
  
  # check that categories is a good factor
  if (is.factor(categories)) {
    if (length(levels(categories)) != 2) {
      stop("You need to supply a factor with two levels.")
    }
  } else if (is.numeric(categories)) {
    if (length(unique(categories)) != 2) {
      stop("You need to supply a categories vector with two categories.")
    }
  }
  
  nCat <- as.numeric(categories)
  
  f <- lapply(unique(nCat), function(x) {
    wh <- which(nCat == x)
    colSums(matrix(ft[wh,], nrow=length(wh),dimnames=list(NULL, colnames(ft))))
  })
  
  switch(method,
         loglikelihood=keyness.ll(f[[1]], f[[2]], 
                                  minimalFrequency, 
                                  epsilon, 
                                  siglevel),
         logratio=keyness.logratio(f[[1]], f[[2]], 
                                   minimalFrequency))
  
}

keyness.ll <- function(f1, f2, 
                       minimalFrequency, 
                       epsilon,
                       siglevel) {
  
  total1 <- sum(f1)
  total2 <- sum(f2)
  
  f1[f1==0] <- epsilon
  f2[f2==0] <- epsilon
  other1 <- total1 - f1
  other2 <- total2 - f2

  rf <- (f1 + f2) / ( total1 + total2 )
  
  e1 <- total1 * rf
  e2 <- total2 * rf
  
  l <- 2 * ( ( f1 * log(f1/e1)  ) + (f2 * log(f2/e2)  ))
  
  
  l <- sort(l, decreasing = TRUE)
  pvalues <- 1 - stats::pchisq(l, df=1)
  
  l[pvalues<siglevel]
}

keyness.logratio <- function(f1, f2, minimalFrequency) {
  f1[f1<=minimalFrequency] <- 0
  f2[f2<=minimalFrequency] <- 0
  
  rf1 <- f1 / sum(f1)
  rf2 <- f2 / sum(f2)
  
  r <- sort(log2(rf1/rf2),decreasing = TRUE)
  
  r[is.finite(r)]
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

