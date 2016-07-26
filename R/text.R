get_frequency_table <- function(t, RELATIVE=TRUE) {
  if (relative==TRUE) {
    count_per_figure(t, fnt=function(x) {prop.table(table(x))})
  } else {
    count_per_figure(t, fnt=table)
  }
}

count_tokens_per_drama <- function(t) {
  tapply(t$Token.surface, t$drama, length)
}

count_tokens_per_figure <- function(t, names=FALSE) {
  count_per_figure(t, names)
}

count_types_per_figure <- function(t, names=FALSE) {
  count_per_figure(t, names, function(x) {length(levels(factor(x)))})
}

count_per_figure <- function(t, names=FALSE, fnt=length, column="Token.lemma") {
  if (names == TRUE)
    tapply(t[[column]], list(t$drama, t$Speaker.figure_surface), fnt)
  else
    tapply(t[[column]], list(t$drama, t$Speaker.figure_id), fnt)
}

filter_counts_for_drama <- function(counts, drama_id) {
  counts[drama_id,][!is.na(counts[drama_id,])]
}

generate_word_cloud <- function(freq_table, min.freq=10) {
  wordcloud(as.list(dimnames(l)),as.vector(l), min.freq=min.freq)
}
