get_frequency_table <- function(drama_id, figure_id) {
  t <- load_text(drama_id, tokens=TRUE)
  table(t[t$Speaker.figure_id == figure_id,]$Token.lemma)
}

get_speeches <- function(drama_id, figure_id) {
  t <- load_text(drama_id, tokens=FALSE)
  t[t$Speaker.figure_id == figure_id,]
}

get_corpus <- function(...) {
  args <- list(...)
  text_by_figure <- c()
  for (a in args) {
    t <- load_text(a, tokens = TRUE)
    figures <- sort(unique(t$Speaker.figure_id))
    for (figure_id in figures) {
      text_by_figure <- c(text_by_figure, paste(t[t$Speaker.figure_id == figure_id,]$Token.surface, sep=" ", collapse=" "))
    }
  }
  VCorpus(VectorSource(text_by_figure))
}
