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


#' @export
#' @title Format Character Names
#' @description This function is applicable on all tables with a character table 
#' (that are in the class QDHasCharacter). It can be used to reformated the character 
#' names
#' @param object The object in which we want to transform names
#' @param drama The QDDrama object with all the information
#' @param FUN A function applied to the strings
#' @examples 
#' data(rksp.0)
#' ustat <- utteranceStatistics(rksp.0)
#' ustat <- format(ustat, rksp.0)
format.QDHasCharacter <- function(object, drama, FUN=stringr::str_to_title) {
  stopifnot(inherits(object, "QDHasCharacter"))
  stopifnot(inherits(drama, "QDDrama"))
  
  positions <- match(levels(object$character), drama$characters$figure_id)
  levels(object$character) <- FUN(drama$characters$figure_surface[positions])

  object    

}

#' @title Filter characters
#' @description This function can be used to filter characters from all tables 
#' that contain  a character column (and are of the class QDHasCharacter).
#' @param object The object we want to filter
#' @param drama The QDDrama object
#' @param by Specifies the filter mechanism
#' @param threshold The threshold
#' @export
#' @examples 
#' data(rjmw.0)
#' dstat <- dictionaryStatistics(rjmw.0)
#' filter(dstat, rjmw.0, by="tokens", threshold=1000)
filter <- function(object, drama, by=c("rank", "tokens"), 
                   threshold=ifelse(by=="tokens", 500, 10)) {
  stopifnot(inherits(object, "QDHasCharacter"))
  stopifnot(inherits(drama, "QDDrama"))
  by <- match.arg(by) 
  
  charStat <- characterStatistics(drama)
  
  keep <- charStat$character
  if (by == "tokens") {
    keep <- charStat[charStat$tokens >= threshold,]$character
  } else if (by == "rank") {
    keep <- charStat[order(charStat$tokens, decreasing = TRUE),]$character[1:threshold]
  }
  
  object[object$character %in% keep, ]
}

#' @title Filtering Mentioned Figures
#' @description This function can be used to remove the mentions of figures 
#' that do not appear as speakers in the subsetted input text (after using 
#' limitFigures(), for example), or to summarize them as 'OTHER'.
#' @param t The text, a data frame listing each token for each figure
#' @param other Whether to summarize mentioned figures as 'OTHER'
#' @export
#' @examples 
#' data(rksp.0)
#' text.top10.filtered <- filterMentioned(limitFigures(rksp.0$mtext))
filterMentioned <- function(t, other=FALSE) {
  figure_id.set <- unique(t$Speaker.figure_id)
  figure_surface.set <- unique(t$Speaker.figure_surface)
  if (other == FALSE) {
    t$Mentioned.figure_id[!(t$Mentioned.figure_id %in% figure_id.set)] <- NA
    t$Mentioned.figure_surface[!t$Mentioned.figure_surface %in% figure_surface.set] <- NA
  } else {
    levels(t$Mentioned.figure_id) <- c(levels(t$Mentioned.figure_id),"OTHER")
    levels(t$Mentioned.figure_surface) <- c(levels(t$Mentioned.figure_surface),"OTHER")
    t$Mentioned.figure_id[!(t$Mentioned.figure_id %in% figure_id.set) & !(is.na(t$Mentioned.figure_id))] <- "OTHER"
    t$Mentioned.figure_surface[!(t$Mentioned.figure_surface %in% figure_surface.set) & !(is.na(t$Mentioned.figure_surface))] <- "OTHER"
  }
  t$Mentioned.figure_id <- droplevels(t$Mentioned.figure_id)
  t$Mentioned.figure_surface <- droplevels(t$Mentioned.figure_surface)
  t
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
#' @param id The id of the text or a list of ids
#' @param of The output file
#' @param type The type of the report. "Single" gives a report about a single play, 
#' while "Compare" can be used to compare multiple editions of a play
#' @param ... Arguments passed through to the rmarkdown document
#' @export
report <- function(id="test:rksp.0", 
                   of=file.path(getwd(),paste0(unlist(strsplit(id,":",fixed=TRUE))[2], ".html")), 
                   type=c("Single"),
                   ...) {
  force(of)
  type <- match.arg(type)
  
  fileName <- switch(type,
         Single="Report.Rmd",
         Compare="Compare-editions.Rmd")
  rmarkdown::render(system.file(paste0("rmd/",fileName), package="DramaAnalysis"), 
                    params=list(id=id,col=qd.colors,...), 
                    output_format = "html_document", 
                    output_file = of)
}

#' @export
segment <- function(hasUtteranceBE, segmentTable) {
  stopifnot(inherits(hasUtteranceBE, "QDHasUtteranceBE"))
  stopifnot(inherits(segmentTable,   "QDHasSegments"))
  
  # if scene begin/end field is NA, we replace it with the act begin/end
  # therefore, we don't loose any text
  segmentTable[is.na(begin.Scene),  `:=`(begin.Scene  = begin.Act),]
  segmentTable[is.na(end.Scene),    `:=`(end.Scene    = end.Act),]
  segmentTable[is.na(Number.Scene), `:=`(Number.Scene = 0),]
  
  data.table::setkey(hasUtteranceBE, "corpus", "drama", "utteranceBegin", "utteranceEnd")
  data.table::setkey(segmentTable, "corpus", "drama", "begin.Scene", "end.Scene")
  
  mtext <- data.table::foverlaps(hasUtteranceBE, segmentTable, type="any",
                                 by.x=c("corpus", "drama", "utteranceBegin", "utteranceEnd"), 
                                 by.y=c("corpus", "drama", "begin.Scene", "end.Scene"))
  mtext
}

#' @export
combine <- function(d1, d2) {
  stopifnot(inherits(d1, "QDDrama"))
  stopifnot(inherits(d2, "QDDrama"))
  r <- list()
  
  # handle text
  r$text     <- rbind(d1$text, d2$text)
  r$meta     <- rbind(d1$meta, d2$meta)
  r$segments <- rbind(d1$segments, d2$segments)
  r$mentions <- rbind(d1$mentions, d2$mentions)
  r$characters <- rbind(d1$characters, d2$characters)

  
  class(r) <- append("QDDrama", class(r))
  class(r$text) <- class(d1$text)
  class(r$meta) <- class(d1$meta)
  class(r$segments) <- class(d1$segments)
  class(r$mentions) <- class(d1$mentions)
  
  
  r
  
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
