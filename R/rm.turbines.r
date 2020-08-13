#' remove influence of turbine activity from a time series
#' @author Simon Frey
#' @export
#' @description analyse a xts object and try to remove the activity from turbines from it
#' @param x an xts object
#' @param rise numeric. Rise in percent that needs to be exceeded for detecting as turbine influence
#' @param tolerance numeric. tolerance in percent that the runoff level needs reach again
#' @param duration numeric. time steps that will be analyzed after an exceed event was detected
#' @param thresQ numeric or NULL. Threshold of runoff below which no detection will be performed.
#' @return an xts object
#'

rm_turbine <- function(x,rise = 300, tolerance = 100, duration = 12, thresQ = NULL){
  if(!is.xts(x)){
    stop("x must be an xts object")
  }
  
  if(ncol(x) > 1){
    warning("Only the first column of x will be used")
    x <- x[,1]
  }
  
  if(is.null(thresQ)){
    thresQ <- quantile(x,0.33,na.rm=TRUE)
  }
  
  # calculate diff time series
  diffts <- diff(x)
  diffts[1:(nrow(diffts)-1)] <- diffts[2:nrow(diffts)]
  diffts[nrow(diffts)] <- NA
  diff100 <- (diffts/x)*100
  diff100[2:nrow(diff100)] <- diff100[1:(nrow(diff100)-1)]
  diff100[1] <- NA

  diffts <- cbind(diffts,x,diff100)
  
  # mark entires with a percentage rise of more than rise (500 per default)
  # then check whether the vlaues drop to the level before the rise including 
  # a tolerance in percent whithin a given time (duration). 
  # If yes, set all values until the level was reached to NA
  
  starts <- which(diff100 >= rise)
  
  k = 1
  
  while(k <= length(starts)){
    s <- starts[k]
    if(x[s] > thresQ){
      level.before.rise <- x[s-1]+x[s-1]*(tolerance/100)
      if(any(x[s:(min((s+duration),nrow(x)))] <= as.numeric(level.before.rise),na.rm=TRUE)){
        x[s:(min((s+duration),nrow(x)))] <- NA
      }
      if(starts[k+1] %in% s:(min((s+duration),nrow(x)))){
        remaining.ks <- (k+1):length(starts)
        w <- which(!starts[remaining.ks] %in% s:(min((s+duration),nrow(x))))
        if(length(w) > 0){
          k <- remaining.ks[w[1]]
        } else {
          k <- length(starts) + 1
        }
      } else {
        k = k + 1
      }
    } else {
      k = k + 1
    }
  }
  
  return(x)
  
}
