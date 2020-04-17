#' @title Utterance positions
#' @description Uses the function \code{stripchart} to  plot each utterance at their position,
#' in a line representing the character. The dot is marked in the middle of each utterance.
#' Might look weird if very long utterances are present.
#' @param x A table generated from the function
#' @param colors The colors to be used
#' @param drama Optional QDDrama object. If present, segment boundaries
#' are extracted from it and included in the plot.
#' @param xlab A character vector that is used as x axis label. Defaults to
#' "Time".
#' @param ... Parameters passed to stripchart().
#' @importFrom graphics stripchart abline
#' @export
#' @return See \code{stripchart()}.
#' @seealso stripchart
plot.QDUtteranceStatistics <- function(x,
                                       drama=NULL,
                                       colors=qd.colors,
                                       xlab="Time",
                                       ...) {
  stopifnot(inherits(x, "QDUtteranceStatistics"))
  
  graphics::stripchart(utteranceBegin+(utteranceLength/2) ~ character, 
                      data=x, 
                      las=1, # horizontal labels
                      pch=20, # use a small bullet as symbol
                      col=colors, # get nice colors
                      xaxt="n", # suppress the x axis
                      xlab=xlab,
                      ...)
  if ( !is.null(drama) ) {
    stopifnot(inherits(drama, "QDDrama"))
    graphics::abline(v=unique(drama$segments$begin.Act)[-1])
  }
}

#' @title Personnel Exchange
#' @description Uses the default scatterplot function to plot the personnel 
#' exchange in each scene.
#' @param x A numeric vector generated from the function
#' @param drama Optional QDDrama object. If present, act boundaries
#' and correct scene labels are included in the plot.
#' @param xlab A character vector that is used as x axis label. Defaults to
#' "Scene".
#' @param ylab A character vector that is used as y axis label. Defaults to
#' "Exchange".
#' @param ... Parameters passed to \code{plot.default()}.
#' @importFrom graphics plot.default axis abline mtext
#' @importFrom utils tail
#' @export
#' @return See \code{plot.default()}.
#' @seealso plot.default
#' @examples
#' data(rksp.0)
#' h <- hamming(rksp.0)
#' plot(h, drama=rksp.0)
plot.QDHamming <- function(x, 
                           drama=NULL,
                           xlab="Scene",
                           ylab="Exchange after Scene",
                           ...) {
  
  # prevent notes in R CMD check
  Number.Act <- NA
  Number.Scene <- NA
  
  stopifnot(inherits(x, "QDHamming"))
  
  graphics::plot.default(x, 
                         xlab=xlab,
                         ylab=ylab,
                         pch=20,
                         xaxt="n",
                         bty="n",
                         ...)
  
  if ( is.null(drama) ) {
    # add x-axis
    graphics::axis(1, at=1:length(x)) 
  } else {
    stopifnot(inherits(drama, "QDDrama"))
    segments_dt <- cbind(drama$segments, nscene=1:nrow(drama$segments))
    # draw vertical act lines
    graphics::abline(v=segments_dt[Number.Scene == 1]$nscene[2:nrow(segments_dt[Number.Scene == 1])] - 0.5)
    # add x-axis
    graphics::axis(1, at=1:(nrow(segments_dt)-1), labels=head(segments_dt$Number.Scene, -1))
    # compute positions for act axis
    act_index = sapply(unique(segments_dt$Number.Act), function(x) {
      beginScene = segments_dt[Number.Act == x][1]$nscene
      endScene = tail(segments_dt[Number.Act == x], 1)$nscene
      (beginScene + endScene) / 2
    })
    # add and label act axis above plot
    graphics::axis(3, at=act_index, labels=unique(segments_dt$Number.Act), tick=FALSE, xlab="Act")
    graphics::mtext("Act", side=3, line=3)
  }
}

#' @title Spider-Webs
#' @description Generates spider-web like plot. 
#' Spider webs may look cool, but they are terrible 
#' to interpret. You should think of using a bar 
#' chart to represent the same information. \emph{You have been warned.}
#' @note radar charts and spider web plots are dangerous, 
#' they can easily become misleading. They are in this package 
#' for historic reasons, but should not be used anymore.
#' @param dstat A data frame containing data, e.g., output from dictionaryStatistics()
#' @param symbols Symbols to be used in the plot
#' @param cglcol The color for the spider net
#' @param pcol The line color(s)
#' @param legend Whether to print a legend
#' @param legend.cex Scaling factor for legend
#' @param legend.pos.x X position of legend
#' @param legend.pos.y Y position of legend
#' @param legend.horizontal Whether to print legend horizontally or vertically
#' @param ... Miscellaneous arguments to be given for radarchart().
#' @export
#' @return No value is returned.
#' @examples 
#' data(rksp.0)
#' fnames <- c("Krieg", "Liebe", "Familie", "Ratio","Religion")
#' ds <- dictionaryStatistics(rksp.0, normalizeByField=TRUE, 
#'                            fieldnames=fnames)
#' plotSpiderWebs(ds)
plotSpiderWebs <- function(dstat, 
                           symbols=c(17,16,15,4,8),
                           cglcol="black", 
                           legend=TRUE,
                           legend.cex=0.7,
                           legend.pos.x="bottomright",
                           legend.pos.y=NA,
                           legend.horizontal = FALSE,
                           pcol=qd.colors,... ) {
  stopifnot(inherits(dstat, "QDDictionaryStatistics"))
  
  warning("spider webs may look cool, but they are terrible to interpret. You should think of using a bar chart to represent the same information. You have been warned.")
  
  mat <- as.matrix(dstat)
  names = dstat$character

  maxValue <- max(mat)
  minValue <- min(mat)
  
  data <- data.frame(rbind(rep(maxValue,ncol(mat)),
                           rep(minValue,ncol(mat)), mat))
  fmsb::radarchart(data, maxmin=TRUE, 
             plwd=1,pcol=pcol,
             pty=symbols,
             plty=1,vlcex=0.7,
             cglcol = cglcol,
             centerzero=TRUE,
             ...,
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
#' @param ... All remaining options are passed to \code{barplot.default()}.
#' @return See \code{barplot.default()}.
#' @export
#' @importFrom graphics text barplot
#' @importFrom data.table dcast
#' @seealso barplot.default
barplot.QDCharacterStatistics <- function(height, 
                                       col = qd.colors, 
                                       column = "tokens", 
                                       order = -1, 
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