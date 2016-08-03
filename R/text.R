#'
#' Returns frequency tables for each figure in all dramas in t
#'
#' @param t A data frame representing (one or more) text
#' @param relative Whether to calculate relative frequencies
#' @param accepted.pos A vector containing pos tags that should be included.
#'    If the vector is empty, includes everything.
#'
#' @examples
#' t <- load_text("rksp.0", tokens=TRUE)
#' freq_table <- get_frequency_table(t)
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
#'
#' @examples
#' t <- load_text("rksp.0", tokens = TRUE)
#' stat <- make_figure_statistics(t, names = FALSE)
#'
#' @export
make_figure_statistics <- function(t, names = FALSE, normalized = FALSE) {
  dup <- tapply(t$begin, paste(t$drama, t$Speaker.figure_id), function(x) {
    dup <- duplicated(x)
    diffs <- dup[-1L] != dup[-length(dup)]
    idx <- c(which(diffs), length(dup))
    diff(c(0, idx))
  })
  indexes <- paste(t$drama, t$Speaker.figure_id)
  if (names == TRUE)
    indexes <- paste(t$drama, t$Speaker.figure_surface)


  r <- as.data.frame(cbind(
    aggregate(t$Token.surface, by=list(t$drama, t$Speaker.figure_surface), function(x) { length(x) }),
    aggregate(t$Token.surface, by=list(t$drama, t$Speaker.figure_surface), function(x) { length(unique(x)) })[,3],
    tapply(t$begin, indexes, function(x) { length(unique(x)) }),
    tapply(t$begin, indexes, function(x) { mean(rle(x)$lengths) }),
    tapply(t$begin, indexes, function(x) { sd(rle(x)$lengths) }),
    tapply(t$begin, indexes, min),
    tapply(t$begin, indexes, max)
  ))
  colnames(r) <- c("drama", "figure","tokens", "types", "utterances", "utterance_length_mean", "utterance_length_sd", "first_begin", "last_end")
  if (normalized == TRUE) {
    normalise <- function(x) {
      x$tokens <- as.numeric(x$tokens) / sum(t[t$drama == x$drama,]$tokens)
      #x$utterances <- x$utterances / sum(t1[t1$drama == x$drama,]$utterances)
      #x$last_end <- x$last_end / max(t1[t1$drama == x$drama,]$last_end)
      #x$first_begin <- x$first_begin / max(t1[t1$drama == x$drama,]$first_begin)
      print(x)
    }
    n <- r
    for (i in 1:nrow(r)) {
      n[i,]$tokens <- r[i,]$tokens / sum(r[r$drama == r[i,1],]$tokens)
      n[i,]$utterances <- r[i,]$utterances / sum(r[r$drama == r[i,1],]$utterances)
      n[i,]$last_end <- r[i,]$last_end / max(r[r$drama == r[i,1],]$last_end)
      n[i,]$first_begin <- r[i,]$first_begin / max(r[r$drama == r[i,1],]$last_end)

    }
    r <- n
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
#' @export
#' @param freq_table A single frequency table
#' @param min.freq The minimal frequency a token should have to be in the word cloud
#'
#' @examples
#' t <- load_text("rksp.0", tokens=TRUE)
#' freq_table <- get_frequency_table(t)
#' generate_word_cloud(freq_table[1,1][[1]])
#'
generate_word_cloud <- function(freq_table, min.freq=10) {
  wordcloud(dimnames(freq_table)[[1]],as.vector(freq_table), min.freq=min.freq, scale=c(5,0.2),random.order = FALSE)
}

#' This function generates a table to be used by stylo
#' @param t The text table, potentially covering multiple texts
#' @param accepted.pos A list of accepted pos tags
#' @param names Whether to use figure names or ids
#' @export
#' @examples
#' t <- load_text("rksp.0", tokens=TRUE)
#' st <- make_style_table(t)
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
limit_figures <- function(t, minTokens=100) {
  counts <- tapply(t$Speaker.figure_surface, paste(t$drama, t$Speaker.figure_id), length)
  write(paste(length(counts[counts > minTokens]), "remaining."),stderr())
  subset(t, counts[paste(t$drama, t$Speaker.figure_id)] > minTokens )
}
