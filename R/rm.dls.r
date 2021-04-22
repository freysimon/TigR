#' Remove daylight saving time steps in an extended time series
#' @description Remove the daylight saving time steps in an xts-object
#' @author Simon Frey
#' @export
#' @import xts
#' @details The function essentially creates an time series sequence in utc with the same length as x. Then the index of x is replaced by this new time series.
#' @param x an xts object.
#' @return an xts object.
#' 
rm.dls <- function(x){

  if(!is.xts(x)){
    stop("x must be an xts object")
  }
  
  # extract the time index
  ti <- index(x)
  
  # conserve colnames
  cn <- colnames(x)
  
  # create an utc time series (no dst) with the same start and length as ti
  sq <- seq.POSIXt(from = ti[1], to = ti[1]+length(ti)*3600, by = "hour", tz = "UTC")
  sq <- sq[1:(length(sq)-1)]
  
  x <- as.xts(as.matrix(x), order.by = sq)
  colnames(x) <- cn
  
  return(x)
  
}