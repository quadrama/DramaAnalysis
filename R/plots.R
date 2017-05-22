require(fmsb)


#' @title Spider-Webs
#' @description Generates spider-web like plot
#' @note radar charts and spider web plots are dangerous, 
#' they can easily become misleading. They are in this package 
#' for historic reasons, but should not be used anymore.
#' @param dstat A data frame containing data, e.g., output from dictionaryStatistics()
#' @param mat A data frame that only contains data assignments. Defaults to all columns except for the first two from dstat.
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
#' @importFrom fmsb radarchart
#' @export
#' @examples 
#' data(rksp.0.text)
#' dstat <- dictionaryStatistics(rksp.0.text, normalizeByField=TRUE, names=TRUE,
#'                               fieldnames=c("Krieg", "Liebe", "Familie", "Ratio","Religion"))
#' plotWordFields(dstat[,3:7],names=dstat[,2],max=50)
plotWordFields <- function(dstat=NULL, mat=dstat[,-c(1,2)], names=dstat[,2], 
                           symbols=c(17,16,15,4,8),
                           maxValue=max(mat),minValue=min(mat), cglcol="black", 
                           legend=TRUE,
                           legend.cex=0.7,
                           legend.pos.x="bottomright",
                           legend.pos.y=NA,
                           legend.horizontal = FALSE,
                           pcol=qd.colors,... ) {
  
  data <- data.frame(rbind(rep(maxValue,ncol(mat)),rep(minValue,ncol(mat)),mat))
  radarchart(data, maxmin=TRUE, 
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