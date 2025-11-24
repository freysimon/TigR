
#' Columnwise apply a function to receive hourly or daily aggregated values
#' @param x an xts object
#' @param FUN an R function
#' @param agg character string to specify whether to receive monthly, weekly, daily or hourly values, respectively. See details.
#' @param PB deprecated. A character indicating whether and what kind of progress bar should be drawn. See details.
#' @param tz character specifying the time zone or NULL (the standard). If the latter, the time zone of x is used.
#' @param ncores integer. Number of CPU cores used. See details.
#' @param ... additional arguments to FUN
#' @description Apply a specified function to each column of an xts object creating hourly, daily or monthly values
#' @details A simple mechanism to use \code{\link{apply.daily}}, \code{\link{apply.hourly}},  \code{\link{apply.weekly}} or \code{\link{apply.monthly}} to each column of an xts object.
#' 
#'     Since november 2025 apply.daily.columns uses multicore parallel computing. By default it uses all but one cores on the system. By setting ncores, the user may overrule this.
#'     
#'     The argument PB is no longer supported and is kept for compability reasons only.
#'     
#' @author Simon Frey
#' @export
#' @import foreach
#' @import doParallel
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

apply.daily.columns <- function(x, FUN, agg = 'day', PB = "n", ncores = 0, tz = NULL, ...){
  
  
  
  library(xts)
  library(foreach)
  library(doParallel)
  
  n_cores <- detectCores()
  
  if(ncores < n_cores){
    n_cores <- max(2, ncores)
  }

  cluster <- makeCluster(n_cores - 1)
  registerDoParallel(cluster)
  
  
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
    out <-  foreach(j=1:dim.in[2], .export = "apply.daily", .packages = "xts", 
                 .combine=cbind) %dopar%
            apply.daily(x[,j], FUN = FUN, ...)
  }
  
  if(agg == "hour"){
    out <-  foreach(j=1:dim.in[2], .export = "apply.hourly", .packages = "xts", 
                    .combine=cbind) %dopar%
            apply.hourly(x[,j], FUN = FUN, ...)
  }
  
  if(agg == "month"){
    out <-  foreach(j=1:dim.in[2], .export = "apply.monthly", .packages = "xts", 
                    .combine=cbind) %dopar%
            apply.monthly(x[,j], FUN = FUN, ...)
  }
  
  if(agg == "week"){
    out <-  foreach(j=1:dim.in[2], .export = "apply.weekly", .packages = "xts", 
                    .combine=cbind) %dopar%
            apply.monthly(x[,j], FUN = FUN, ...)
  }

 
 
  # 
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

  
  # stopping cluster

  stopCluster(cl = cluster)
  return(out)
  
}