#' @title Utterance positions
#' @description Uses `stripchart` to  plot each utterance at their position,
#' in a line representing the figure. The dot is marked in the middle of each utterance.
#' Might look weird if very long utterances are present.
#' @param x A table generated from the function
#' @param segmentedText If supplied, act boundaries will be marked
#' @param colors The colors to be used
#' @param plotFrame Whether to draw a frame around the plot
#' @param ... Parameters passed to stripchart().
#' @importFrom graphics stripchart abline
#' @export
#' @seealso stripchart
plot.QDUtteranceStatistics <- function(x,
                                       drama=NULL,
                                       colors=qd.colors,
                                       frame=TRUE,
                                       xlab="Time",
                                       ...) {
  stopifnot(inherits(x, "QDUtteranceStatistics"))
  
  graphics::stripchart(utteranceBegin+(utteranceLength/2) ~ character, 
                      data=x, 
                      las=1, # horizontal labels
                      pch=20, # use a small bullet as symbol
                      col=colors, # get nice colors
                      xaxt="n", # suppress the x axis
                      frame=frame,
                      xlab=xlab,
                      ...)
  if ( !is.null(drama) ) {
    stopifnot(inherits(drama, "QDDrama"))
    graphics::abline(v=unique(drama$segments$begin.Act)[-1])
  }
}

#' @title Spider-Webs
#' @description Generates spider-web like plot
#' @note radar charts and spider web plots are dangerous, 
#' they can easily become misleading. They are in this package 
#' for historic reasons, but should not be used anymore.
#' @param dstat A data frame containing data, e.g., output from dictionaryStatistics()
#' @param mat A data frame that only contains data assignments. Defaults to all columns except for the first three from dstat.
#' @param names A list of names for the legend
#' @param symbols Symbols to be used in the plot
#' @param maxValue The maximal value. Defaults to the highest value in mat.
#' @param minValue The minimal value. Defaults to the lowest value in mat.
#' @param cglcol The color for the spider net
#' @param pcol The line color(s)
#' @param legend Whether to print a legend
#' @param legend.cex Scaling factor for legend
#' @param legend.pos.x X position of legend
#' @param legend.pos.y Y position of legend
#' @param legend.horizontal Whether to print legend horizontally or vertically
#' @param ... Miscellaneous arguments to be given for radarchart().
#' @export
#' @examples 
#' data(rksp.0)
#' fnames <- c("Krieg", "Liebe", "Familie", "Ratio","Religion")
#' ds <- dictionaryStatistics(rksp.0, normalizeByField=TRUE, 
#'                            fieldnames=fnames, asList=TRUE)
#' plotSpiderWebs(dstat=ds,max=50)
plotSpiderWebs <- function(dstat=NULL, mat=dstat$mat, names=dstat$figure, 
                           symbols=c(17,16,15,4,8),
                           maxValue=max(mat),minValue=min(mat), cglcol="black", 
                           legend=TRUE,
                           legend.cex=0.7,
                           legend.pos.x="bottomright",
                           legend.pos.y=NA,
                           legend.horizontal = FALSE,
                           pcol=qd.colors,... ) {

  data <- data.frame(rbind(rep(maxValue,ncol(mat)),rep(minValue,ncol(mat)),mat))
  fmsb::radarchart(data, maxmin=TRUE, 
             plwd=1,pcol=pcol,
             pty=symbols,
             plty=1,vlcex=0.7, ...,
             cglcol = cglcol,
             centerzero=TRUE,
             seg=2)
  if (legend) {
    legend(x=legend.pos.x, y=legend.pos.y, horiz=legend.horizontal, col=pcol,
         legend=names,pch=symbols,cex=legend.cex,bg="white", bty="n")     
  }  
  
}

#' @title Stacked Bar Plot
#' @description This function expects an object of type QDCharacterStatistics and 
#' plots the specified column as a stacked bar plot.
#' @param height The object of class QDCharacterStatistics that is to be plotted
#' @param column Which column of the character statistics should be used?
#' @param labels Whether to add character labels into the plot
#' @param order Sort the fields inversely
#' @param top Limit the labels to the top 5 characters. Otherwise, 
#' labels will become unreadable.
#' @param col The colors to use
#' @param ... All remainig options are passed to \code{barplot.default()}.
#' @export
#' @importFrom graphics text barplot
#' @seealso barplot.default
barplot.QDCharacterStatistics <- function(height, 
                                       col=qd.colors, 
                                       column="tokens", 
                                       order=-1, 
                                       labels = TRUE,
                                       top = 5,
                                       ...) {
  stopifnot(inherits(height, "QDCharacterStatistics"))
  
  # prevent note in R CMD check
  drama <- NULL
  `:=` <- NULL
  head <- NULL
  
  fs <- as.data.table(height)
  fs[,rank:=as.double(rank( get(column) *order,ties.method = "first")),drama]
  mat_values <- as.matrix(dcast(data=fs,rank ~ drama, value.var=column)[,-1])
  mat_labels <- as.matrix(dcast(data=fs,rank ~ drama, value.var="character")[,-1])
  mat_cs <- apply(mat_values, 2,cumsum)
  mat_cs <- rbind(matrix(0,ncol=ncol(mat_cs)),mat_cs)
  mat_values <- rbind(mat_values,matrix(NA,ncol=ncol(mat_values)))
  mat <- list(values=mat_values,labels=mat_labels,cs=mat_cs)
  
  b <- graphics::barplot.default(mat$values, col=col, ...)
  if (labels) {
    graphics::text(x=b, y=t(head(mat$cs,top)+(head(mat$values,top)/2)),
         labels=t(substr(head(mat$labels,top),0,20)))
  }
  b
}