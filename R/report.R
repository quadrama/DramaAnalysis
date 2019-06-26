#' @title Report
#' @description generates a report for a specific dramatic text
#' @param id The id of the text or a list of ids
#' @param of The output file
#' @param type The type of the report. "Single" gives a report about a single play, 
#' while "Compare" can be used to compare multiple editions of a play. Please note that 
#' the "Compare" report is still under development.
#' @param ... Arguments passed through to the rmarkdown document
#' @return The return value of \code{\link[rmarkdown]{render}}
#' @export
report <- function(id="test:rksp.0", 
                   of=file.path(getwd(),paste0(unlist(strsplit(id,":",fixed=TRUE))[2], ".html")), 
                   type=c("Single", "Compare"),
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