#' color scheme to be used for QuaDramA plots
#' Taken from http://google.github.io/palette.js/, tol-rainbow, 10 colors
#' @export
qd.colors <- c(rgb(120,28,129, maxColorValue = 255),
               rgb(67, 50, 141, maxColorValue = 255),
               rgb(65, 111, 184, maxColorValue = 255),
               rgb(81, 156, 184, maxColorValue = 255),
               rgb(112, 180, 132, maxColorValue = 255),
               rgb(153, 189, 92, maxColorValue = 255),
               rgb(195, 186, 69, maxColorValue = 255),
               rgb(224, 162, 57, maxColorValue = 255),
               rgb(230, 107, 45, maxColorValue = 255),
               rgb(217, 33, 32, maxColorValue = 255)
              );

#'
#' Generates a word cloud based on a frequency table
#' @param freq_table A single frequency table
#' @param min.freq The minimal frequency a token should have to be in the word cloud
#' @export
#'
generate_word_cloud <- function(freq_table, min.freq=10,column=1, colors="black", max.words=100) {
  wordcloud(words=rownames(freq_table), freq=freq_table[[column]], min.freq=min.freq,scale=c(5,0.3),random.order = FALSE, colors=colors, max.words  = max.words)
  #wordcloud(dimnames(freq_table)[[1]],as.vector(freq_table), min.freq=min.freq, scale=c(5,0.2),random.order = FALSE)
}


#' This method removes the spoken tokens of all but the most frequent n figures
#' @param t The text, a data frame listing each token for each figure
#' @param maxTokens Up to maxTokens figures remain in the data set
#' @export
#' @importFrom utils head
#' @examples
#' data(rksp.0)
#' t <- limit.figures.by.rank(rksp.0)
limit.figures.by.rank <- function(t, maxRank=10) {
  counts <- aggregate(t$Speaker.figure_surface, by=list(t$drama, t$Speaker.figure_id), length)
  counts <- counts[order(counts$x, decreasing = TRUE),]
  rcounts <- Reduce(rbind, by(counts, counts["Group.1"], head, n=maxRank))
  t[paste(t$drama, t$Speaker.figure_id) %in% paste(rcounts$Group.1, rcounts$Group.2),]
}

#' This method removes the spoken tokens by all figures that speak infrequently.
#' @param t The text, a data frame listing each token for each figure
#' @param minTokens The minimal amount of tokens a figure has to speak
#' @export
#' @examples
#' data(rksp.0)
#' limit.figures.by.tokens(rksp.0)
limit.figures.by.tokens <- function(t, minTokens=100) {
    counts <- tapply(t$Speaker.figure_surface, paste(t$drama, t$Speaker.figure_id), length)
    write(paste(length(counts[counts > minTokens]), "remaining."),stderr())
    subset(t, counts[paste(t$drama, t$Speaker.figure_id)] > minTokens )
}





#' Classification using k nearest neighbor
#' Evaluation is done using cross validation.
#' @param ft A frequency table. Words/lemmas in columns, texts/figures in rows
#' @param gold A list of gold labels in the same order as in the frequency table
#' @param k The number of neighbors for kNN
#' @param num.folds The number of folds for CV
#' @export
#' @examples
#' \dontrun{
#' baseurl <- "http://zwergdrossel.ims.uni-stuttgart.de:8080/"
#' url.tragedies <- paste(baseurl, "drama.web/set/trag%C3%B6die", sep="")
#' url.comedies <- paste(baseurl, "drama.web/set/kom%C3%B6die", sep="")
#' meta.comedies <- read.csv(url.comedies)
#' meta.tragedies <- read.csv(url.tragedies)
#' text.all.comedies <- load.text(meta.comedies$id, tokens=TRUE)
#' text.all.tragedies <- load.text(meta.tragedies$id, tokens=TRUE)
#' }
cv.knn <- function(ft, labels, k=5, num.folds=10) {

  folds <- createFolds(as.vector(labels), k = num.folds)

  accuracy <- data.frame(row.names=c("Accuracy","Kappa","AccuracyLower","AccuracyUpper","AccuracyNull","AccuracyPValue","McnemarPValue"))
  for (fold in folds) {
    pred <- knn(ft[-unlist(fold),], ft[unlist(fold),], unlist(labels[-unlist(fold)]), k=k)
    res <- confusionMatrix(pred, unlist(labels[unlist(fold)]))
    accuracy <- rbind(accuracy, as.list(res$overall))
  }
  print(mean(accuracy$Accuracy))
  accuracy
}
