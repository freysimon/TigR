
# Does not longer work with xts > 0.10.0

#' #' Advanced plotting function to plot xts objects
#' #' @description The function is identical to the plotting function in the xts package but has the advantage
#' #' that labeling of the x-axis can be turned off. In addition, it takes the mgp options from par instead of using
#' #' the standard ootions without the possibility to edit them.
#' #'     Also, the lines.color can be set.
#' #' @param x an xts object
#' #' @param y an xts object or NULL
#' #' @param type type of plot to produce
#' #' @param auto.grid should grid lines be drawn
#' #' @param major.ticks should major tickmarks be drawn and labeled
#' #' @param minor.ticks should minor tickmarks be drawn
#' #' @param major.format passed along to axTicksByTime.
#' #' @param bar.col the color of the bars when type is ‘bars’ or ‘candles’
#' #' @param candle.col the color of the candles when type is ‘candles’
#' #' @param lines.col the color of the candles when type is ‘lines’
#' #' @param ann passed ‘par’ graphical parameter
#' #' @param axes passed ‘par’ graphical parameter
#' #' @param xlabels logical or a vector giving the labels (see details)
#' #' @param ... additional graphical arguments
#' #' @details Mainly used to draw time-series plots with sensible x-axis labels, it can also plot basic OHLC series using type='candles' or type='bars'.
#' #'
#' #'    Better financial plots can be found in the quantmod package, though these are generally incompatible with standard R graphics tools.
#' #'
#' #'    If xlabels is FALSE, no lables are drawn (but the ticks are). If it is TRUE, the lables determined by the xts function are drawn. If it is a vector, this vector is used for labeling. Will be recycled.
#' #' @return Plots an xts object to the current device.
#' #' @author Simon Frey
#' #' @export
#' #' @seealso \code{\link{xts}}
#' #' @examples
#' #' data(sample_matrix)
#' #' plot(sample_matrix)
#' #' plot.xxts(as.xts(sample_matrix))
#' #' plot.xxts(as.xts(sample_matrix), lines.col="red", xlabels = FALSE)
#' plot.xxts <- function(x, y = NULL, type = "l", auto.grid = TRUE, major.ticks = "auto",
#'                             minor.ticks = TRUE, major.format = TRUE, bar.col = "grey",
#'                             candle.col = "white", ann = TRUE, axes = TRUE,lines.col="black",
#'                             xlabels=TRUE,...){
#' 
#'   #xlabels might be TRUE/FALSE or a vector giving the labels
#' 
#' 
#'   load.quantmod <- library(quantmod,logical.return=TRUE)
#'   if(!load.quantmod){
#'     install.packages("quantmod")
#'     load.quantmod <- library(quantmod,logical.return=TRUE)
#'   }
#'   series.title <- deparse(substitute(x))
#'   ep <- axTicksByTime(x, major.ticks, format.labels = major.format)
#'   otype <- type
#'   if (is.OHLC(x) && type %in% c("candles", "bars")) {
#'     x <- x[, has.OHLC(x, TRUE)]
#'     xycoords <- list(x = .index(x), y = seq(min(x), max(x),
#'                                             length.out = NROW(x)))
#'     type <- "n"
#'   }
#'   else {
#'     if (NCOL(x) > 1)
#'       warning("only the univariate series will be plotted")
#'     if (is.null(y))
#'       xycoords <- xy.coords(.index(x), x[, 1])
#'   }
#'   plot(xycoords$x, xycoords$y, type = type, axes = FALSE, ann = FALSE,
#'        col = lines.col, ...)
#'   if (auto.grid) {
#'     abline(v = xycoords$x[ep], col = "grey", lty = 4)
#'     grid(NA, NULL)
#'   }
#'   if (is.OHLC(x) && otype == "candles")
#'     plot.ohlc.candles(x, bar.col = bar.col, candle.col = candle.col,
#'                       ...)
#'   dots <- list(...)
#'   if (axes) {
#'     if (minor.ticks)
#'       axis(1, at = xycoords$x, labels = FALSE, col = "#BBBBBB",
#'            ...)
#'     if(is.logical(xlabels)){
#'       if(xlabels==TRUE){
#'         axis(1, at = xycoords$x[ep], labels = names(ep), las = 1,
#'              lwd = 1, ...)
#'       } else if(xlabels==FALSE) {
#'         axis(1, at = xycoords$x[ep], labels = FALSE, las = 1,
#'              lwd = 1, ...)
#'       }
#'     } else {
#'       axis(1, at = xycoords$x[ep], labels = xlabels[ep], las = 1,
#'            lwd = 1, ...)
#'     }
#' 
#'     axis(2, ...)
#'   }
#'   box()
#'   if (!"main" %in% names(dots))
#'     title(main = series.title)
#'   do.call("title", list(...))
#'   assign(".plot.xts", recordPlot(), xts:::.xtsEnv)
#' }
