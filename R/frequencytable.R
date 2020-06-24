#' @title Word frequencies 
#' @description The function \code{frequencytable()} generates a matrix of word frequencies 
#' by drama, act or scene and/or by character. The output of this function can be fed to stylo.
#' @param drama A \code{QDDrama}. May be covering multiple texts.
#' @param acceptedPOS A list of accepted pos tags. Words of all POS tags not in this list 
#' are filtered out. Specify NULL or an empty list to include all words.
#' @param bigram Logical. Calculates bigram frequencies instead of frequencies of single words.
#' @param byCharacter Logical. Whether the count is by character or by text.
#' @param segment Character vector. Whether the count is by drama (default), act or scene
#' @param column The column name we should use (should be either Token.surface or Token.lemma)
#' @param sep The separation symbol that goes between drama name and character (if applicable). 
#' Defaults to the pipe symbol.
#' @param sepBigram The separation symbol that goes between words of a bigram. Ignored if \code{bigram} is false.
#' Defaults to " " (a single space).
#' @param normalize Whether to normalize values or not. If set to TRUE, the values are normalized by
#' row sums.
#' @param sortResult Logical. If true, the columns with the highest sum are ordered left (i.e., frequent words are visible first). If false, the columns are ordered alphabetically by column name.
#' @return Matrix of word frequencies in the format words X segments
#' @rdname frequencyTable
#' @seealso \code{stylo}
#' @importFrom stats xtabs ftable
#' @importFrom data.table shift
#' @examples
#' data(rksp.0)
#' st <- frequencytable(rksp.0)
#' \donttest{
#' stylo(gui=FALSE, frequencies = st)
#' }
#' @export
frequencytable <- function(drama, 
                           acceptedPOS = postags$de$words,
			   bigram=FALSE,
                           column="Token.lemma", 
                           byCharacter=FALSE, 
                           sep="|", 
			   sepBigram=" ",
                           normalize=FALSE, 
                           sortResult=FALSE, 
                           segment=c("Drama", "Act", "Scene")) {
  stopifnot(inherits(drama, "QDDrama"))
  
  segment <- match.arg(segment)
  
  ft <- switch(segment,
               Drama=drama$text,
               Act=segment(drama$text, drama$segments),
               Scene=segment(drama$text, drama$segments))
  
  
  if (length(acceptedPOS) > 0)
    ft <- ft[as.character(ft$Token.pos) %in% acceptedPOS,]
 
  if (bigram == TRUE) {
    ft$nextToken <- data.table::shift(ft[,get(column)], n=-1)
    ft$ngram <- paste(ft[,get(column)], ft$nextToken, sep=sepBigram)
  } else {
    ft$ngram <- ft[,get(column)]
  }
  
  segment <- match.arg(segment)
  switch(segment,
         Drama = { 
           if (byCharacter == FALSE) { xt <- stats::xtabs(~drama + ft$ngram, data=ft) }
           else { xt <- stats::xtabs(~ paste(drama,Speaker.figure_id,sep=sep) + ~ft$ngram, data=ft) }
         },
         Act = {
           if (byCharacter == FALSE) { xt <- stats::xtabs(~ paste(drama,Number.Act,sep=sep) + ~ft$ngram, data=ft) }
           else { xt <- stats::xtabs(~ paste(drama,Number.Act,Speaker.figure_id,sep=sep) + ~ft$ngram, data=ft) }
         },
         Scene = {
           if (byCharacter == FALSE) { xt <- stats::xtabs(~ paste(drama,Number.Act,Number.Scene,sep=sep) + ~ft$ngram, data=ft) }
           else { xt <- stats::xtabs(~ paste(drama,Number.Act,Number.Scene,Speaker.figure_id,sep=sep) + ~ft$ngram, data=ft) }
         },
         stop("Please enter valid string-value for argument 'segment' (default = 'Drama', 'Act' or 'Scene').")
  )

  r <- as.matrix(stats::ftable(xt, row.vars = c(), col.vars = c()))
  
  if (normalize == TRUE) {
    r <- t(apply(r,1,function(x) { x / sum(x)}))
  } 
  
  if (sortResult == TRUE) {
    r <- r[,order(colSums(r),decreasing = TRUE),drop = FALSE]
  } else {
    r <- r[, order(colnames(r)) ,drop = FALSE]
  }
  
  class(r) <- append(class(r), switch(segment, 
                                      Drama = "QDByDrama",
                                      Act   = "QDByAct",
                                      Scene ="QDByScene"))
  
  r
}


# defunct, but new development
frequencytable3 <- function(drama,
                           acceptedPOS = postags$de$words,
                           column="Token.lemma", 
                           byCharacter=FALSE, 
                           sep="|", 
                           threshold=10,
                           normalize=FALSE, 
                           sortResult=FALSE, 
                           segment=c("Drama", "Act", "Scene")) {
  stopifnot(inherits(drama, "QDDrama"))
  
  segment <- match.arg(segment)
  
  ft <- switch(segment,
               Drama=drama$text,
               Act=segment(drama$text, drama$segments),
               Scene=segment(drama$text, drama$segments))
  
  
  if (length(acceptedPOS) > 0)
    ft <- ft[as.character(ft$Token.pos) %in% acceptedPOS,]
  
  segment <- match.arg(segment)
  
  fmla <- if(byCharacter == TRUE){
    switch(segment,
           Drama=("drama + Speaker.figure_id"),
           Act=("drama + Number.Act + Speaker.figure_id"),
           Scene=("drama + Number.Act + Number.Scene + Speaker.figure_id"))
  } else {
    switch(segment,
           Drama=("drama"),
           Act=("drama + Number.Act"),
           Scene=("drama + Number.Act + Number.Scene"))
  }
  
  metaCols <- if(byCharacter == TRUE)
  { switch(segment,
           Drama=2,
           Act=3,
           Scene=4)
  } else {
    switch(segment,
           Drama=1,
           Act=2,
           Scene=3)
  }
  
  xt <- stats::xtabs(as.formula(paste0("~", fmla, "+", column, collapse = "")), data=ft)
  
  
  xt <- as.data.table(as.data.frame(ftable(xt)))
  fmla.f <- as.formula(paste(fmla, "Token.lemma", sep = "~"))
  xt <- data.table::dcast(xt, fmla.f, value.var="Freq")
  
  if (normalize == TRUE) {
    xt[metaCols:ncol(xt),] <- t(apply(xt[metaCols:ncol(xt),],1,function(x) { xt / sum(xt)}))
  } 
  
  if (sortResult == TRUE) {
    xt <- xt[,order(colSums(xt),decreasing = TRUE)]
  }
  
  class(xt) <- append(class(xt), switch(segment, 
                                        Drama = "QDByDrama",
                                        Act   = "QDByAct",
                                        Scene ="QDByScene"))
  
  xt
}
