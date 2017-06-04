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

#' @title Filtering figures
#' @description This function can be used to remove speech content of certain figures. 
#' Currently, it offers two ways of filtering: By rank or by spoken words. Filtering by 
#' rank is an upper criterion, i.e., up to $threshold$ figures are included. Filtering 
#' by tokens is a lower limit: Every figure that speaks more than $threshold$ figures is
#' included.
#' @param text The dramatic text in table form
#' @param by A character vector, either "rank" or "tokens"
#' @param threshold A number specifying the limit
#' @export
#' @examples 
#' data(rksp.0.text)
#' text.top10 <- limitFigures(rksp.0.text)
limitFigures <- function(text, by="rank", threshold=ifelse(by=="tokens",500,10)) {
  if(is.na(pmatch(by, c("tokens", "rank")))) stop("Invalid filtering criterion")
  if (by=="tokens") {
    limit.figures.by.tokens(text, minTokens=threshold)
  } else {
    limit.figures.by.rank(text, maxRank = threshold)
  }
}

#' This method removes the spoken tokens of all but the most frequent n figures
#' @param t The text, a data frame listing each token for each figure
#' @param maxRank Up to maxRank figures remain in the data set
#' @importFrom utils head
limit.figures.by.rank <- function(t, maxRank=10) {
  counts <- aggregate(t$Speaker.figure_surface, by=list(t$drama, t$Speaker.figure_id), length)
  counts <- counts[order(counts$x, decreasing = TRUE),]
  rcounts <- Reduce(rbind, by(counts, counts["Group.1"], head, n=maxRank))
  t[paste(t$drama, t$Speaker.figure_id) %in% paste(rcounts$Group.1, rcounts$Group.2),]
}

#' This method removes the spoken tokens by all figures that speak infrequently.
#' @param t The text, a data frame listing each token for each figure
#' @param minTokens The minimal amount of tokens a figure has to speak
limit.figures.by.tokens <- function(t, minTokens=100) {
    counts <- tapply(t$Speaker.figure_surface, paste(t$drama, t$Speaker.figure_id), length)
    write(paste(length(counts[counts > minTokens]), "remaining."),stderr())
    subset(t, counts[paste(t$drama, t$Speaker.figure_id)] > minTokens )
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
#' data(rksp.0.text)
#' rksp.0.ftable <- frequencytable(rksp.0.text,byFigure=TRUE)
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

#' Creates classic drama configuration matrix. Returns a list with 
#' the three components matrix, drama, and figure
#' @param mtext The text including Act and Scene markings
#' @param by A string, either "Act" or "Scene"
#' @param onlyPresence If TRUE, the resulting matrix only contains 
#' logical values for stage presence
#' @seealso DramaAnalysis::load.text2()
#' @export
#' @examples
#' data(rksp.0.mtext)
#' cfg <- configuration(rksp.0.mtext)
#' 
configuration <- function(mtext, by="Act", onlyPresence=FALSE) {
  
  if (by=="Scene") {
    c <- configuration.scene(mtext)
  } else {
    c <- configuration.act(mtext)
  }
  if (onlyPresence)
    c$matrix <- c$matrix>0
  c
}

#' @importFrom stats reshape
configuration.act <- function(mtext) {
  t <- mtext
  words.per.segment <- aggregate(Token.surface ~ drama + Speaker.figure_surface + Number.Act, 
                                 data=t, length)
  cfg <- stats::reshape(words.per.segment, direction="wide", idvar = c("drama","Speaker.figure_surface"), timevar = "Number.Act")
  cfg[is.na(cfg)] <- 0
  colnames(cfg) <- c("drama", "Speaker.figure_surface",seq(1,(ncol(cfg)-2)))
  list(matrix=as.matrix(cfg[,3:ncol(cfg)]),drama=cfg[,1],figure=cfg[,2])
}

#' @importFrom stats reshape
configuration.scene <- function(text) {
  t <- text
  bylist = list(t$drama, t$Speaker.figure_surface, paste0(t$Number.Act,"-", formatC(t$Number.Scene, width=2)))
  words.per.segment <- aggregate(t$Token.surface, 
                                 by=bylist, 
                                 length)
  cfg <- stats::reshape(words.per.segment, direction="wide", idvar = c("Group.1","Group.2"), timevar = "Group.3")
  cfg[is.na(cfg)] <- 0
  colnames(cfg) <- c("drama", "Speaker.figure_surface",seq(1,(ncol(cfg)-2)))
  list(matrix=as.matrix(cfg[,3:ncol(cfg)]),drama=cfg[,1],figure=cfg[,2])
  
}

#' @title Report
#' @description generates a report for a specific dramatic text
#' @param id The id of the text
#' @param of The output file
#' @param colors A list of colors to be used for plots
#' @importFrom rmarkdown render
#' @export
report <- function(id="tg:rksp.0", of=paste0("../", id, ".html"), colors=qd.colors) {
  rmarkdown::render("R/Report.Rmd", params=list(id=id, col=colors), 
                    output_format = "html_document", 
                    output_file = of)
}
