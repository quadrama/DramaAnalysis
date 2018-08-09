#' @title Save Isolated Figure Speech
#' @description \code{saveFigureSpeech()} isolates the speeches
#' of individual figures and saves them in a new text file.
#' @param t A text (or multiple texts, as data.frame or data.table)
#' @param segment "Drama", "Act", or "Scene". Determines on what segment-level the speech is isolated.
#' @param min_token_count The minimal token count for a speech to be saved in a
#' file. The argument is ignored if set to -1 (default).
#' @param count_punctuation Whether to include punctuation in min_token_count
#' @param dir The directory into which the files will be written (default: current working directory)
#' @export
#' @examples
#' 
saveFigureSpeech <- function(t,
                             segment=c("Drama", "Act", "Scene"),
                             min_token_count=-1,
                             count_punctuation=TRUE,
                             dir=getwd()) {
  # TODO
}