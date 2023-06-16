#' Columnwise apply a function to receive hourly or daily aggregated values
#' @param x an xts object
#' @param FUN an R function
#' @param agg character string to specify whether to receive monthly, weekly, daily or hourly values, respectively. See details.
#' @param PB a character indicating whether and what kind of progress bar should be drawn. See details.
#' @param tz character specifying the time zone or NULL (the standard). If the latter, the time zone of x is used
#' @param ... additional arguments to FUN
#' @description Apply a specified function to each column of an xts object creating hourly, daily or monthly values
#' @details A simple mechanism to use \code{\link{apply.daily}}, \code{\link{apply.hourly}},  \code{\link{apply.weekly}} or \code{\link{apply.monthly}} to each column of an xts object. 
#'     
#'     By setting PB, an optional progressbar can be drawn: "w" or "win" draws a \code{\link{winProgressBar}}, 
#'     "t" or "txt" draws a \code{\link{txtProgressBar}} and "n" or "none" (the default) suppresses the progress bar.
#'     
#' @author Simon Frey
#' @export
#' @import xts
#' @return An xts object containing hourly, daily, ... values
#' @seealso \code{\link{apply.daily}}
#' @seealso \code{\link{apply.hourly}}
#' @seealso \code{\link{apply.monthly}}
#' @seealso \code{\link{apply.weekly}}
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
  if(!agg %in% c('day', 'hour', "week", "month")){
    stop("agg must be one of 'month', 'day' or 'hour'")
  }
  if(is.null(tz)){
    tz = tzone(x)
  }
  
  # get dimensions of x
  dim.in <- dim(x)
  
  # get dimensions of processed time series
  if(agg == 'day'){
    temp <- apply.daily(x[,1], FUN = FUN)
  } else if(agg == "hour") {
    temp <- TigR::apply.hourly(x[,1], FUN = FUN)
  } else if (agg == "week") {
    temp <- apply.weekly(x[,1], FUN = FUN)
  } else {
    temp <- apply.monthly(x[,1], FUN = FUN)
  }
  
  dim.out <- dim(temp)
  
  # create new aggregated xts object with dim.out
  out <- matrix(nrow = dim.out[1], ncol = dim.in[2], data = NA)
  out <- xts(out, order.by = index(temp))
  
  rm(temp)
  
  if(agg == 'day'){
    titl <- "Aggregating to daily data"
  }
  if(agg == "hour"){
    titl <- "Aggregating to hourly data"
  }
  if(agg == "week"){
    titl <- "Aggregating to weekly data"
  }
  if(agg == "month"){
    titl <- "Aggregating to monthly data"
  }
  
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
    }
    if(agg == "hour"){
      out[,j] <- apply.hourly(x[,j], FUN = FUN, ...)
    }
    if(agg == "month"){
      out[,j] <- apply.monthly(x[,j],FUN = FUN, ...)
    }
    if(agg == "week"){
      out[,j] <- apply.weekly(x[,j],FUN = FUN, ...)
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
  }
  if(agg == 'week'){
    index(out) <- as.Date(index(out), tz = tz)
  }
  if(agg == "hour"){
    index(out) <- as.POSIXct(format(index(out), format = "%Y-%m-%d %H:00"), tz = tz)
  }
  if(agg == "month"){
    index(out) <- as.POSIXct(format(index(out), fomat = "%Y-%m"), tz = tz)
  }
  
  
  return(out)
  
}