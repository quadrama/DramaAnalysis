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
make_stylo_table <- function(t, accepted.pos=c(), names=FALSE) {
  ft <- t
  if (length(accepted.pos) > 0)
    ft <- t[t$Token.pos %in% accepted.pos]
  if (names == TRUE)
    do.call(rbind, tapply(ft$Token.lemma, paste(ft$drama, ft$Speaker.figure_surface), function(x){prop.table(table(x))}))
  else
      do.call(rbind, tapply(ft$Token.lemma, paste(ft$drama, ft$Speaker.figure_id), function(x){prop.table(table(x))}))
}

#' @export
limit_figures <- function(t, minTokens=100) {
  counts <- as.data.frame(tapply(t$Speaker.figure_surface, paste(t$drama, t$Speaker.figure_id), length))
  subset(t, counts[paste(t$drama, t$Speaker.figure_id),] > 1000 )
}
