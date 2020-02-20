#' Columnwise apply a function to receive hourly or daily agreggated values
#' @param x an xts object
#' @param FUN an R function
#' @param agg character string 'day' or 'hour' to specifiy whether to receive daily or hourly values, respectivly
#' @param PB a character indicating whether and what kind of progress bar should be drawn. See details.
#' @param tz character specifying the time zone or NULL (the standard). If the latter, the time zone of x is used
#' @param ... additional arguments to FUN
#' @description Apply a specified function to each column of an xts object creating hourly or daily values
#' @details A simple mechanism to use \code{\link{apply.daily}} or \code{\link{apply.hourly}} to each column of an xts object. 
#'     
#'     By setting PB, an optional progressbar can be drawn: "w" or "win" draws a \code{\link{winProgressBar}}, 
#'     "t" or "txt" draws a \code{\link{txtProgressBar}} and "n" or "none" (the default) suppresses the progressbar.
#'     
#' @author Simon Frey
#' @export
#' @import xts
#' @return An xts object containing daily values
#' @seealso \code{\link{apply.daily}}
#' @seealso \code{\link{apply.hourly}}
#' @examples  
#'     # load precipitation input
#'     data(precipitation)
#'     head(x)
#'     
#'     # aggregate to daily values
#'     aday <- apply.daily.columns(x, FUN = sum, agg = 'day', PB = 'txt')
#'     head(aday)

apply.daily.columns <- function(x, FUN, agg = 'day', PB = "n", tz = NULL, ...){
  library(xts)
  if(class(x)[1] != "xts"){
    stop("x must be an xts object")
  }
  if(!PB %in% c("w", "win", "t", "txt", "n", "none")){
    warning("PB not recognized.")
    PB <- "n"
  }
  if(!agg %in% c('day', 'hour')){
    stop("agg must be one of 'day' or 'hour'")
  }
  if(is.null(tz)){
    tz = indexTZ(x)
  }
  
  # get dimensions of x
  dim.in <- dim(x)
  
  # get dimensions of processed time series
  if(agg == 'day'){
    temp <- apply.daily(x[,1], FUN = FUN)
  } else {
    temp <- TigR::apply.hourly(x[,1], FUN = FUN)
  }
  
  dim.out <- dim(temp)
  
  # create new aggregated xts object with dim.out
  out <- matrix(nrow = dim.out[1], ncol = dim.in[2], data = NA)
  out <- xts(out, order.by = index(temp))
  
  rm(temp)
  
  titl <- ifelse(agg == 'day', "Aggregating to daily data", "Aggregating to hourly data")
  
  if(!PB %in% c("n", "none")){
    if(PB %in% c("win", "w")){
      pb <- winProgressBar(title = titl, label = "",
                           min = 0, max = dim.in[2], initial = 0, width = 400)
    }  
    if(PB %in% c("txt", "t")){
      pb <- txtProgressBar(min = 0, max = dim.in[2], initial = 0)
    }
  }
  
  # fill out with data
  for(j in 1:dim.in[2]){
    
    
    if(agg == 'day'){
      out[,j] <- apply.daily(x[,j], FUN = FUN, ...)
    } else {
      out[,j] <- apply.hourly(x[,j], FUN = FUN, ...)
    }
    
    
    if(PB %in% c("win", "w")){
      setWinProgressBar(pb, j, label =  round((j / dim.in[2]),2))
    }
    if(PB %in% c("txt", "t")){
      setTxtProgressBar(pb, j)
    }
    
  }
  
  if(!PB %in% c("n", "none")){
    close(pb) 
  }
  
  # formatting index of out
  #tz <- indexTZ(out)
  if(agg == 'day'){
    index(out) <- as.Date(index(out), tz = tz)
  } else {
    index(out) <- as.POSIXct(format(index(out), format = "%Y-%m-%d %H:00"), tz = tz)
  }
  
  
  return(out)
  
}