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
#' @title Format Names
#' @description The function \code{characterNames()} is applicable on 
#' all tables with a character table 
#' (that are of the class \code{QDHasCharacter}). It can be used to reformat the character 
#' names. The function \code{FUN} is applied to the character \emph{name} entries within 
#' the \code{QDDrama} object. The factor levels in the character column of \code{x} are replaced by 
#' the result values of \code{FUN}.
#' @param x The object in which we want to transform names, needs to inherit the type \code{QDHasCharacter}.
#' @param drama The QDDrama object with all the information.
#' @param sort Numberic. If set to a non-zero value, the resulting data.frame will be sorted 
#' alphabetically
#' according to the drama and character name. If the value is above 0, the 
#' sorting will be ascending, if set to a negative value, the sorting is 
#' descending. If sort is set to 0 (the default), the order is unchanged. 
#' The ordering can also be specified explicitly, by passing an integer vector 
#' with as many elements as \code{x} has rows.
#' @param FUN A function applied to the strings. 
#' Defaults to \code{stringr::str_to_title}, which
#' converts the strings to title case.
#' @param ... All other arguments are ignored.
#' @seealso \code{\link[stringr]{str_to_title}}
#' @importFrom stringr str_to_title
#' @examples 
#' data(rksp.0)
#' ustat <- utteranceStatistics(rksp.0)
#' ustat <- characterNames(ustat, rksp.0)
characterNames <- function(x, 
                                  drama, 
                                  FUN=stringr::str_to_title, 
                                  sort=0, 
                                  ...) {
  requireNamespace("stringr")
  stopifnot(inherits(x, "QDHasCharacter"))
  stopifnot(inherits(drama, "QDDrama"))
  
  positions <- match(levels(x$character), drama$characters$figure_id)
  levels(x$character) <- FUN(drama$characters$figure_surface[positions])

  if (length(sort) == nrow(x)) {
    x <- x[sort,]
  } else if (length(sort) == 1) {
    if (length(sort) == 1 && sort > 0) {
      x <- x[order(x$drama, x$character),]
    } else if (length(sort) == 1 && sort < 0) {
      x <- x[order(x$drama, x$character, decreasing=TRUE),]
    }
  }
    
  x

}

#' @title Format drama titles
#' @description Given a QDDrama object, this function generates a list of nicely 
#' formatted names, following the format string.
#' @param x The QDDrama object
#' @param orderBy The meta data key that the final list will be ordered by
#' @param formatString A character vector. Contains special symbols that
#' are replaced by meta data entries about the plays. The following symbols can
#' be used:
#' - \%T: title of the play
#' - \%A: Author name 
#' - \%P: GND entry of the author (if known)
#' - \%DR, %DP, %DW: Date of premiere, print or written
#' - \%DM: The minimal date 
#' - \%L: The language
#' - \%I: The id
#' - \%C: The corpus prefix
#' @export
#' @importFrom stringr str_replace
dramaNames <- function(x, 
                       formatString = "%A: %T (%DM)", 
                       orderBy = "drama") {
  stopifnot(inherits(x, "QDDrama"))
  
  keys = list("%T"="documentTitle", 
              "%A"="Name",
              "%DR" = "Date.Premiere",
              "%DP" = "Date.Printed",
              "%DW" = "Date.Written",
              "%P" = "Pnd",
              "%I" = "drama",
              "%C" = "corpus",
              "%L" = "language")
  t <- rep(formatString, times=nrow(x$meta))
  
  for (key in names(keys)) {
    column <- x$meta[[keys[[key]]]]
    t <- stringr::str_replace(t, key, ifelse(is.na(column), 
                                             "NA",
                                             as.character(column)))
    
  }
  
  suppressWarnings(column <- apply(x$meta[,c("Date.Printed", "Date.Written", "Date.Premiere")], 
                                   1, min, na.rm=TRUE))
  
  t <- stringr::str_replace(t, "%DM", ifelse(is.finite(column),
                                             as.character(column),
                                             "NA"))
  
  
  
  t[order(x$meta[[orderBy]])]
}

#' @title Filter characters
#' @description This function can be used to filter characters from all tables 
#' that contain  a character column (and are of the class QDHasCharacter). 
#' @param hasCharacter The object we want to filter.
#' @param drama The QDDrama object.
#' @param by Character vector. Specifies the filter mechanism. 
#' @param n The threshold or a list of character names/ids to keep.
#' @details The function supports three filter mechanisms: The filter by 
#' \code{rank} sorts the characters according to the number of tokens they speak
#' and \emph{keeps} the top $n$ characters. The filter called \code{tokens} keeps 
#' all characters that speak $n$ or more tokens. The filter called \code{name} 
#' keeps the characters that are provided by name as a vector as \code{n}.
#' @export
#' @examples 
#' data(rjmw.0)
#' dstat <- dictionaryStatistics(rjmw.0)
#' filterCharacters(dstat, rjmw.0, by="tokens", n=1000)
#' 
#' 
filterCharacters <- function(hasCharacter, 
                   drama, 
                   by=c("rank", "tokens", "name"), 
                   n=ifelse(by=="tokens", 500, ifelse(by=="rank", 10, c()))) {
  
  # verify that objects have the correct types
  stopifnot(inherits(hasCharacter, "QDHasCharacter"))
  stopifnot(inherits(drama, "QDDrama"))
  
  # match argument
  by <- match.arg(by) 
  
  # retrieve character statistics
  charStat <- characterStatistics(drama)
  
  # by default, we keep everyone
  keep <- charStat$character
  if (by == "tokens") {
    keep <- charStat[charStat$tokens >= n,]$character
  } else if (by == "rank") {
    keep <- charStat[order(charStat$tokens, decreasing = TRUE),]$character[1:n]
  } else if (by == "name") {
    keep <- n
  }
  
  # filter based on keep and return
  hasCharacter[hasCharacter$character %in% keep, ]
}

#' @title Filtering Mentioned Characters
#' @description This function can be used to remove the mentions of figures 
#' that do not appear as speakers in the subsetted input text (after using 
#' limitFigures(), for example), or to summarize them as 'OTHER'.
#' @param t The text, a data frame listing each token for each figure
#' @param other Whether to summarize mentioned figures as 'OTHER'
#' @examples 
#' \dontrun{
#' data(rksp.0)
#' text.top10.filtered <- filterMentioned(limitFigures(rksp.0$text))
#' }
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
#' rksp.0.ftable <- frequencytable(rksp.0, byCharacter=TRUE, normalize=TRUE)
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







#' @export
#' @description The function \code{combine(x, y)} can be used to merge 
#' multiple objects of the type \code{QDDrama} into one.
#' @param y A \code{QDDrama}
#' @rdname loadDrama
#' @examples 
#' 
#' data(rksp.0)
#' data(rjmw.0)
#' d <- combine(rjmw.0, rksp.0)
combine <- function(x, y) {
  stopifnot(inherits(x, "QDDrama"))
  stopifnot(inherits(y, "QDDrama"))

  r <- lapply(names(x), function(z) {
    t <- rbind(x[[z]], y[[z]])
    class(t) <- class(x[[z]])
    t
  })
  names(r) <- names(x)
  class(r) <- append("QDDrama", class(r))

  r
}

#' @export
#' @description The function \code{split(x)} expects an object of type \code{QDDrama} and can 
#' be used to split a \code{QDDrama} object that consists of multiple dramas 
#' into a list thereof. It is the counterpart to \code{combine(x, y)}.
#' @param x The object of class \code{QDDrama} (consisting of multiple dramas). 
#' For \code{split()} it should consistof multiple plays. For \code{combine()} it 
#' can but doesn't have to.
#' @param ... All other arguments are ignored.
#' @rdname loadDrama
#' @examples 
#' data(rksp.0)
#' data(rjmw.0)
#' d <- combine(rjmw.0, rksp.0)
#' dlist <- split(d)
split.QDDrama <- function(x, ...) {
  stopifnot(inherits(x, "QDDrama"))
  r <- lapply(unique(x$characters$drama), function(y) {
    d <- lapply(x, function(z) {
      t <- z[z$drama == y]
      class(t) <- class(z)
      t
    })
    class(d) <- append("QDDrama", class(d))
    d
  })
  names(r) <- unique(x$characters$drama)

  r
}

#' @description The function \code{numberOfPlays()} determines how many
#' different plays are contained in a single QDDrama object.
#' @rdname loadDrama
#' @export
#' @examples 
#' # returns 1
#' numberOfPlays(rksp.0)
#' 
#' # returns 2
#' numberOfPlays(combine(rksp.0, rjmw.0))
numberOfPlays <- function(x) {
  stopifnot(inherits(x, "QDDrama"))
  
  if ("meta" %in% names(x) && "drama" %in% names(x$meta)) {
    length(unique(x$meta$drama))
  } else if ("text" %in% names(x) && "drama" %in% names(x$text)) {
    length(unique(x$text$drama))
  }
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
#' @examples 
#' \dontrun{
#' data(rksp.0)
#' # Extract the second last scene
#' dramaTail(rksp.0$text, by="Scene", op="==", n=2)
#' }
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
#' @description Extracts a sub segment of the text(s). 
#' The result is an empty table if more scenes or acts
#' are given than exist in the play. In this case, a
#' warning is printed.
#' @param input Segmented text (can be multiple texts)
#' @param op Whether to extract exactly one or more than one
#' @param by Act or Scene, or matching substring
#' @param n The number of segments to extract
#' @examples 
#' \dontrun{
#' data(rksp.0)
#' # Extract everything before the 4th scene
#' dramaHead(rksp.0$text, by="Scene", op="<", n=4)
#' }
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
