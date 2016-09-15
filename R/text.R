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
#' Returns frequency tables for each figure in all dramas in t
#'
#' @param t A data frame representing (one or more) text
#' @param relative Whether to calculate relative frequencies
#' @param accepted.pos A vector containing pos tags that should be included.
#'    If the vector is empty, includes everything.
#' @param If set to true, the resulting table will include figure names instead of figure ids
#'
#' @examples
#' data(rksp.0)
#' freq_table <- get_frequency_table(rksp.0)
#' @export
get_frequency_table <- function(t, relative=FALSE, names=FALSE, accepted.pos=c()) {
  if (relative == TRUE) {
    (count_per_figure(t, fnt=function(x) {prop.table(table(x))}, names=names, accepted.pos = accepted.pos))
  } else {
    (count_per_figure(t, fnt=table, names=names, accepted.pos = accepted.pos))
  }
}

#' This function extracts figure statistics from a drama text table.
#' @return A data frame with the following columns and one row for each figure:
#' tokens: The number of tokens spoken by that figure
#' types : The number of different tokens (= types) spoken by each figure
#' utterances: The number of utterances
#' utterance_length_mean: The mean length of utterances
#' utterance_length_sd: The standard deviation in utterance length
#' @param t The drama text
#' @param names If set to true, the table will contains figure names instead of ids
#' @param normalize Normalising the individual columns
#'
#' @examples
#' data(rksp.0)
#' stat <- make_figure_statistics(rksp.0, names = FALSE)
#'
#' @export
make_figure_statistics <- function(t, names = FALSE, normalize = FALSE) {
  dup <- tapply(t$begin, paste(t$drama, t$Speaker.figure_id), function(x) {
    dup <- duplicated(x)
    diffs <- dup[-1L] != dup[-length(dup)]
    idx <- c(which(diffs), length(dup))
    diff(c(0, idx))
  })
  indexes <- paste(t$drama, t$Speaker.figure_id)
  bylist <- list(t$drama, t$Speaker.figure_id)
  if (names == TRUE) {
    indexes <- paste(t$drama, t$Speaker.figure_surface)
    bylist <- list(t$drama, t$Speaker.figure_surface)
  }

  r <- as.data.frame(cbind(
    aggregate(t$Token.surface, by=bylist, function(x) { length(x) }),
    aggregate(t$Token.surface, by=bylist, function(x) { length(unique(x)) })[,3],
    aggregate(t$begin, by=bylist, function(x) { length(unique(x)) })[,3],
    aggregate(t$begin, by=bylist, function(x) { mean(rle(x)$lengths) })[,3],
    aggregate(t$begin, by=bylist, function(x) { sd(rle(x)$lengths) })[,3],
    aggregate(t$begin, by=bylist, min)[,3],
    aggregate(t$end, by=bylist, max)[,3],
    aggregate(t$length, by=bylist, function(x) { unique(x) })[,3]
  ))
  colnames(r) <- c("drama", "figure","tokens", "types", "utterances", "utterance_length_mean", "utterance_length_sd", "first_begin", "last_end", "length")
  if (normalize == TRUE) {
    r$tokens <- r$tokens / r$length
    r$utterances <- ave(r$utterances, r$drama, FUN=function(x) {x/sum(x)})
    r$first_begin <- r$first_begin / ave(r$last_end, r$drama, FUN=max)
    r$last_end <- ave(r$last_end, r$drama, FUN=function(x) {x/max(x)})
  }
  r
}



count_tokens_per_drama <- function(t) {
  tapply(t$Token.surface, t$drama, length)
}

#' @export
count_tokens_per_figure <- function(t, names=FALSE) {
  count_per_figure(t, names)
}

count_types_per_figure <- function(t, names=FALSE) {
  count_per_figure(t, names, function(x) {length(levels(factor(x)))})
}

count_per_figure <- function(t, names=FALSE, fnt=length, column="Token.lemma", accepted.pos=c()) {
  tf <- t
  if (length(accepted.pos) > 0) {
    tf <- t[t$Token.pos %in% accepted.pos,]
  };
  if (names == TRUE) {
    tapply(tf[[column]], paste(tf$drama, tf$Speaker.figure_surface,sep=";"), fnt)
  } else {
    tapply(tf[[column]], paste(tf$drama, tf$Speaker.figure_id,sep=";"), fnt)
  }
}

filter_counts_for_drama <- function(counts, drama_id) {
  counts[drama_id,][!is.na(counts[drama_id,])]
}

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

#' This function generates a table to be used by stylo
#' @param t The text table, potentially covering multiple texts
#' @param accepted.pos A list of accepted pos tags
#' @param names Whether to use figure names or ids
#' @param column The column name we should use (should be either Token.surface or Token.lemma)
#' @export
#' @examples
#' data(rksp.0)
#' st <- make_style_table(rksp.0)
#' stylo(gui=FALSE, st)
#' @examples
#' t <- load_text(read.csv("http://localhost:8080/drama.web/dramas", header=FALSE)[,], tokens=T)
#' tl <- limit_figures(t, minTokens=1000)
#' stylo_table <- make_stylo_table(tl, names=TRUE)
#' stylo(gui=F, frequencies = stylo_table, network=T, write.png.file=T, analysis.type="BCT")
make_stylo_table <- function(t, accepted.pos=postags$de$words, names=FALSE, column="Token.surface") {
  ft <- t
  if (length(accepted.pos) > 0)
    ft <- t[t$Token.pos %in% accepted.pos,]
  if (names == TRUE)
    r <- do.call(rbind, tapply(ft[[column]], paste(ft$drama, ft$Speaker.figure_surface), function(x){prop.table(table(x))}))
  else
    r <- do.call(rbind, tapply(ft[[column]], paste(ft$drama, ft$Speaker.figure_id), function(x){prop.table(table(x))}))
  r[,order(colSums(r),decreasing=TRUE)]


  }

#' @export
limit.figures.by.rank <- function(t, maxRank=10) {
  counts <- aggregate(t$Speaker.figure_surface, by=list(t$drama, t$Speaker.figure_id), length)
  counts <- counts[order(counts$x, decreasing = TRUE),]
  rcounts <- Reduce(rbind, by(counts, counts["Group.1"], head, n=maxRank))
  t[paste(t$drama, t$Speaker.figure_id) %in% paste(rcounts$Group.1, rcounts$Group.2),]
}

#' @export
limit_figures <- function(t, minTokens=100) {
    counts <- tapply(t$Speaker.figure_surface, paste(t$drama, t$Speaker.figure_id), length)
    write(paste(length(counts[counts > minTokens]), "remaining."),stderr())
    subset(t, counts[paste(t$drama, t$Speaker.figure_id)] > minTokens )
}

#' @export
count_word_fields <- function(t, fieldnames=c(), normalize = FALSE, names=FALSE, boost = 100) {
  baseurl <- "https://raw.githubusercontent.com/quadrama/metadata/master/fields/"
  bylist <- list(t$drama, t$Speaker.figure_id)
  if (names == TRUE)
    bylist <- list(t$drama, t$Speaker.figure_surface)
  r <- aggregate(t, by=bylist, length)[,1:2]
  for (field in fieldnames) {
    url <- paste(baseurl, field, ".txt", sep="")
    list <- read.csv(url, header=F, fileEncoding = "UTF-8")
    r <- cbind(r,  count_word_field(t, tolower(list$V1), normalize.by.figure =FALSE, normalize.by.field = TRUE,names=names)[,3])
  }
  colnames(r) <- c("drama", "figure", fieldnames)
  if (normalize == TRUE) {
    tokens <- aggregate(t$Token.surface, by=bylist, function(x) { length(x) })
    r[,-(1:2)] <- r[,-(1:2)] / ave(tokens[[3]], tokens[1:2], FUN=function(x) {x})
  }
  r[,-(1:2)] <- r[,-(1:2)] * boost
  r
}

count_word_field <- function(t, wordfield=c(), names = FALSE, normalize.by.figure = FALSE, normalize.by.field = FALSE) {
  bylist <- list(t$drama, t$Speaker.figure_id)
  if (names == TRUE)
    bylist <- list(t$drama, t$Speaker.figure_surface)

  r <- aggregate(t$Token.surface, by=bylist, function(x) {
    if (normalize.by.field == TRUE)
      length(x[tolower(x) %in% wordfield])/length(wordfield)
    else
      length(x[tolower(x) %in% wordfield])
  })


  colnames(r) <- c("drama", "figure", "x")
  if (normalize.by.figure == TRUE) {
    for (i in 1:nrow(r)) {
      r[i,]$x <- r[i,]$x / length(t[t$drama == r[i,1] & t$Speaker.figure_id == r[i,2],]$Token.lemma)
    }
  }
  r
}

#' This method calculates the length of each utterance, organised by figure and drama.
#' @param t The dramatic text(s)
#' @param num.figures The maximal number of figures per drama to include. Default: 10. Set to FALSE to include all figures.
#' @export
#' @examples
#' data(rksp.0)
#' num_figures <- 5
#' ustat <- utterance.statistics(rksp.0, num.figures = num_figures)
#' boxplot(ustat$utterance_length ~ ustat$figure,col=qd.colors[1:num_figures], las=2,frame=F)
utterance.statistics <- function(t, num.figures=10) {

  if (typeof(num.figures) == "double") {
    t <- limit.figures.by.rank(t, maxRank = num.figures)
  }
  # utterance statistics
  ulength <- aggregate(t$Token.surface, by=list(t$drama, t$Speaker.figure_surface, t$begin, t$length), length)

  colnames(ulength) <- c("drama", "figure", "begin", "drama_length","utterance_length")

  # normalize by drama length
  ulength$utterance_length <- ulength$utterance_length / ulength$drama_length

  # skip empty factor levels
  ulength <- droplevels(ulength)

  # make nicer names
  ulength$figure <- sapply(strsplit(x = as.character(ulength$figure), split="[,.]"),"[", 1)

  # order them by drama and alphabetically
  ulength$figure <- factor(ulength$figure, levels=unique(ulength[order(ulength$drama, ulength$figure),]$figure))

  ulength
}
