#' @title QuaDramA colors
#' @description color scheme to be used for QuaDramA plots
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

#' @title Filtering figures
#' @description This function can be used to remove speech content of certain figures. 
#' Currently, it offers two ways of filtering: By rank or by spoken words. Filtering by 
#' rank is an upper criterion, i.e., up to $threshold$ figures are included. Filtering 
#' by tokens is a lower limit: Every figure that speaks more than $threshold$ figures is
#' included.
#' @param text The dramatic text in table form
#' @param by A character vector, either "rank" or "tokens" (or unambigious sub string)
#' @param threshold A number specifying the limit
#' @export
#' @examples 
#' data(rksp.0)
#' text.top10 <- limitFigures(rksp.0$mtext)
limitFigures <- function(text, by=c("rank","tokens"), threshold=ifelse(by=="tokens",500,10)) {
  by <- match.arg(by)
  switch(by,
         tokens=limitFiguresByTokens(text, minTokens=threshold),
         rank=limitFiguresByRank(text, maxRank = threshold),
         stop("Invalid filtering criterion"))
}

#' This method removes the spoken tokens of all but the most frequent n figures
#' @param t The text, a data frame listing each token for each figure
#' @param maxRank Up to maxRank figures remain in the data set
#' @keywords internal
#' @importFrom utils head
limitFiguresByRank <- function(t, maxRank=10) {
  r <- t[,n:=.N,.(corpus,drama,Speaker.figure_surface)][,.SD[n%in%maxN(unique(n),maxRank)], by=.(corpus,drama)][,n:=NULL,][]
  r$Speaker.figure_id <- droplevels(r$Speaker.figure_id)
  r$Speaker.figure_surface <- droplevels(r$Speaker.figure_surface)
  r
}

#' This method removes the spoken tokens by all figures that speak infrequently.
#' @param t The text, a data frame listing each token for each figure
#' @param minTokens The minimal amount of tokens a figure has to speak
#' @keywords internal
limitFiguresByTokens <- function(t, minTokens=100) {
  r <- t[,n:=.N,.(corpus,drama,Speaker.figure_surface)][,.SD[n>=minTokens],by=.(corpus,drama)][,n:=NULL][]
  r$Speaker.figure_id <- droplevels(r$Speaker.figure_id)
  r$Speaker.figure_surface <- droplevels(r$Speaker.figure_surface)
  r
}


limit.figures.by.rank <- function(...) {
  .Deprecated("limitFigures(by=\"rank\"")
  limitFiguresByRank(...)
}


limit.figures.by.tokens <- function(...) {
  .Deprecated("limitFigures(by=\"tokens\"")
  limitFiguresByTokens(...)
}

tfidf1 <- function(word) {
  docfreq <- sum(word>0)
  docfreq <- log((length(word)+1) / (sum(word>0)))
  if (docfreq==0)
    rep(0,length(word))
  else
    word*docfreq
}

#' @title TF-IDF
#' @description This function calculates a variant TF-IDF. 
#' The input is assumed to contain relative frequencies.
#' IDF is calculated as follows: \eqn{idf_t = \log\frac{N+1}{n_t}}, with \eqn{N} being 
#' the total number of documents (i.e., rows) and \eqn{n_t} the number of documents
#' containing term \eqn{t}. We add one to the denominator to prevent terms that appear
#' in every document to become 0.
#' 
#' @param ftable A matrix, containing "document" as rows and "terms" as columns. 
#' Values are assumed to be normalized by document, i.e., contain relative frequencies.
#' @export
#' @examples
#' data(rksp.0)
#' rksp.0.ftable <- frequencytable(rksp.0$mtext,byFigure=TRUE,normalize=TRUE)
#' rksp.0.tfidf <- tfidf(rksp.0.ftable)
#' @examples
#' mat <- matrix(c(0.10,0.2, 0,
#'                 0,   0.2, 0,
#'                 0.1, 0.2, 0.1,
#'                 0.8, 0.4, 0.9),
#'               nrow=3,ncol=4)
#' mat2 <- tfidf(mat)
#' print(mat2)
tfidf <- function(ftable) {
  if (mean(apply(ftable,1,sum))!=1)
    stop("Matrix should contain relative frequencies")
  r <- apply(ftable, 2, tfidf1)
  rownames(r) <- rownames(ftable)
  r[is.na(r)] <- 0
  r
}


extractTopTerms <- function(mat, top=10) {
  r <- apply(mat, 1, function(x) { list(head(x[order(x, decreasing=TRUE)],n=top)) })
  lapply(r, unlist)
}



#' @title Report
#' @description generates a report for a specific dramatic text
#' @param id The id of the text
#' @param of The output file
#' @param colors A list of colors to be used for plots
#' @importFrom rmarkdown render
#' @importFrom igraph graph_from_adjacency_matrix plot.igraph layout_ on_grid
#' @export
report <- function(id="test:rksp.0", of=file.path(getwd(),paste0(unlist(strsplit(id,":",fixed=TRUE))[2], ".html")), ...) {
  force(of)
  rmarkdown::render(system.file("rmd/Report.Rmd", package="DramaAnalysis"), 
                    params=list(id=id, ...), 
                    output_format = "html_document", 
                    output_file = of)
}

#' @title Extract section
#' @description Extracts a sub segment of the text(s).
#' The result is an empty table if more scenes or acts
#' are given than exist in the play. In this case, a
#' warning is printed.
#' @param input Segmented text (can be multiple texts)
#' @param op Whether to extract exactly one or more than one
#' @param by Act or Scene, or matching substring
#' @param n The number of segments to extract
#' @export
#' @examples 
#' data(rksp.0)
#' # Extract the second last scene
#' dramaTail(rksp.0$mtext, by="Scene", op="==", n=2)
dramaTail <- function(input, by=c("Act","Scene"), op="==", n=1) {
  
  # prevent notes in R CMD check
  corpus <- NULL
  drama <- NULL
  begin.Act <- NULL
  begin.Scene <- NULL
  .SD <- NULL
  . <- NULL
  
  oper <- match.fun(FUN=op)
  by <- match.arg(by)
  
  switch(by,
         Act=ifelse(n>length(unique(input$begin.Act)), 
                    warning(paste("Play has only", length(unique(input$begin.Act)) , "acts."), call. = FALSE),
                    NA),
         Scene=ifelse(n>length(unique(input$begin.Scene)), 
                      warning(paste("Play has only", length(unique(input$begin.Scene)) , "scenes."), call. = FALSE),
                      NA))
  
  switch(by,
         Act=input[,.SD[oper(begin.Act,last(unique(begin.Act), n))],.(corpus,drama)][],
         Scene=input[,.SD[oper(begin.Scene,last(unique(begin.Scene), n))],.(corpus,drama)][])
}

#' @title Extract section
#' @export
#' @description Extracts a sub segment of the text(s). 
#' The result is an empty table if more scenes or acts
#' are given than exist in the play. In this case, a
#' warning is printed.
#' @param input Segmented text (can be multiple texts)
#' @param op Whether to extract exactly one or more than one
#' @param by Act or Scene, or matching substring
#' @param n The number of segments to extract
#' @examples 
#' data(rksp.0)
#' # Extract everything before the 4th scene
#' dramaHead(rksp.0$mtext, by="Scene", op="<", n=4)
dramaHead <- function(input, by=c("Act", "Scene"), op="==", n=1) {
  
  # prevent notes in R CMD check
  corpus <- NULL
  drama <- NULL
  begin.Act <- NULL
  begin.Scene <- NULL
  .SD <- NULL
  . <- NULL
  
  
  
  oper <- match.fun(FUN=op)
  by <- match.arg(by)
  switch(by,
         Act=ifelse(n>length(unique(input$begin.Act)), 
                    warning(paste("Play has only", length(unique(input$begin.Act)) , "acts."), call. = FALSE),
                    NA),
         Scene=ifelse(n>length(unique(input$begin.Scene)), 
                      warning(paste("Play has only", length(unique(input$begin.Scene)) , "scenes."), call. = FALSE),
                      NA))
  
  switch(by,
         Act=input[,.SD[oper(begin.Act,first(unique(begin.Act), n))],.(corpus,drama)][],
         Scene=input[,.SD[oper(begin.Scene,first(unique(begin.Scene), n))],.(corpus,drama)][])
}

first <- function(x,n=0) {
  sort(x)[n]
}

last <- function(x, n=0) {
  len <- length(x)
  if (n > len) {
    return(NA)
  }
  sort(x,partial=len-(n-1))[len-(n-1)]
}

maxN <- function(x, N=2){
  len <- length(x)
  if(N>len){
    warning('N greater than length(x).  Setting N=length(x)')
    N <- length(x)
  }
  sort(x,partial=len-N+1)[(len-N+1):(len)]
}
