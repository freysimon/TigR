
#' Columnwise apply a function to receive hourly or daily aggregated values
#' @param x an xts object
#' @param FUN an R function
#' @param agg character string to specify whether to receive monthly, weekly, daily or hourly values, respectively. See details.
#' @param PB logical. Should a progressbar be shown? See details.
#' @param tz character specifying the time zone or NULL (the standard). If the latter, the time zone of x is used.
#' @param ncores integer. Number of CPU cores used. See details.
#' @param ... additional arguments to FUN
#' @description Apply a specified function to each column of an xts object creating hourly, daily or monthly values
#' @details A simple mechanism to use \code{\link{apply.daily}}, \code{\link{apply.hourly}},  \code{\link{apply.weekly}} or \code{\link{apply.monthly}} to each column of an xts object.
#' 
#'     Since November 2025 apply.daily.columns uses multicore parallel computing. By default it uses just a single core. By setting ncores, the user may overrule this.
#'     
#'     In previous versions, PB indicated whether a txt or winProgressbar should indicate the progress of the calculations. Since parallel computing is now supported PB now indicates whether a progressbar should be displayed (using \code{\link{progressr}}). Default is TRUE. Still "n" suppresses the progressbar. The old parameters ("t", "txt", "w" "win") are kept for compability reasons.
#'     
#' @author Simon Frey
#' @export
#' @import foreach
#' @import doParallel
#' @import xts
#' @import progressr
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
#'     aday <- apply.daily.columns(x, FUN = sum, agg = 'day', PB = TRUE)
#'     head(aday)
#'
#'	   # using a progressbar
#'	   library(progressr)
#'	   handlers("txtprogressbar")
#'	   with_progress({
#'       aday <- apply.daily.columns(x, FUN = sum, agg = 'day', PB = TRUE)
#'     })
#'	   head(aday)

apply.daily.columns <- function(x, FUN, agg = 'day', PB = TRUE, ncores = 1, tz = NULL, ...) {
  library("xts")
  library("foreach")
  library("doParallel")
  library("progressr")
  
  if (class(x)[1] != "xts") stop("x must be an xts object")
  if (is.null(tz)) tz <- xts::tzone(x)
  
  max_cores <- parallel::detectCores()
  if (is.null(ncores) || ncores <= 0 || ncores > max_cores) {
    n_cores <- max_cores - 1
  } else {
    n_cores <- min(ncores, max_cores)
  }
  if(n_cores < 1) n_cores <- 1
  
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  
  agg_fun <- switch(agg,
                    day = xts::apply.daily,
                    hour = {
                      if (!requireNamespace("TigR", quietly = TRUE)) {
                        parallel::stopCluster(cl)
                        stop("Package 'TigR' needed for hourly aggregation is not installed.")
                      }
                      TigR::apply.hourly
                    },
                    week = xts::apply.weekly,
                    month = xts::apply.monthly,
                    stop("agg must be one of 'day', 'hour', 'week', 'month'")
  )
  
  temp <- agg_fun(x[,1, drop=FALSE], FUN = FUN, ...)
  dim_in <- dim(x)
  
  # Nur progressor erzeugen, wenn PB = TRUE
  if (PB) {
    p <- progressr::progressor(steps = dim_in[2])
  } else {
    p <- function(...) NULL
  }
  
  out_list <- foreach::foreach(j = 1:dim_in[2],
                               .packages = c("xts", if(agg == "hour") "TigR" else NULL),
                               .combine = cbind) %dopar% {
                                 res <- agg_fun(x[, j, drop=FALSE], FUN = FUN, ...)
                                 p()  # Fortschritt inkrementieren
                                 return(res)
                               }
  
  parallel::stopCluster(cl)
  
  out_xts <- xts(out_list, order.by = index(temp))
  
  if (agg %in% c("day", "week")) {
    index(out_xts) <- as.Date(index(out_xts), tz = tz)
  } else if (agg == "hour") {
    index(out_xts) <- as.POSIXct(format(index(out_xts), "%Y-%m-%d %H:00:00"), tz = tz)
  } else if (agg == "month") {
    index(out_xts) <- as.POSIXct(paste0(format(index(out_xts), "%Y-%m"), "-01"), tz = tz)
  }
  
  return(out_xts)
}