#' @title Graphical User Interface
#' @description The function \code{exploreDramas()} opens a user interface that shows different analyses for the installed dramas in the form of interactive plots.
#' @param ... Provide arguments for shiny::runApp()
#' @return Shiny App
#' @rdname exploreDramas
#' @seealso \code{shiny}
#' @import shiny
#' @examples
#' exploreDramas(quiet = TRUE)
#' exploreDramas(launch.browser = TRUE)
#' @export
exploreDramas <- function(...) {
  shiny::runApp(appDir = "../DramaAnalysisWeb", ...) 
}
