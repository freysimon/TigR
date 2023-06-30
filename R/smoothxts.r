#####################################################
#                                                   #
#   Glätten einer Zeitreihe innerhalb eines         #
#   bestimmten Wertebereichs                        #
#                                                   #
#####################################################

#' Smooth a time series
#' @param x an xts object.
#' @param timesteps integer. Number of timestpes used for smoothing
#' @param lowerbund numeric. (Optional) Lower boundary. Above this smoothing will be performed.
#' @param upperbound numeric. (Optional). Upper boundary. Below this, smoothing will be performed.
#' @return An xts object
#' @author Simon Frey
#' @description Smooth a time series, optionally within two boundaries.
#' @details It uses the function \code{\link{embed.fullextent}} to smooth the timeseries
#' @examples 
#' data(runoff)
#' x <- runoff[,1]
#' summary(x)
#' X <- smoothXTS(x)
#' summary(X)
#' X <- smoothXTS(x, lowerbound = quantile(x,0.2), upperbound = quantile(x, 0.8))
#' summary(X)
#' @seealso \code{\link{embed.fullextent}}
#' @export
smoothXTS <- function(x,timesteps=24,lowerbound=NULL, upperbound=NULL){

  # x = Zeitreihe, die geglättet werden soll
  # timesteps = Anzahl der Zeitschritte, die für die Glättung verwendet werden
  # optional: lowerbound = Unterer Grenzwert, AB dem geglättet werden soll
  # optional: upperbound = Oberer Grenzwert, BIS zu dem geglättet werden soll

  if(!is.xts(x)){
    stop("x muss im XTS Format sein!")
  }

  if(is.null(lowerbound)){
    lowerbound <- min(x)
  }
  if(is.null(upperbound)){
    upperbound <- max(x)
  }

  wnotsmooth_l <- which(x<=lowerbound)
  wnotsmooth_u <- which(x>=upperbound)
  notsmooth_l <- x[wnotsmooth_l]
  notsmooth_u <- x[wnotsmooth_u]



  x[wnotsmooth_l] <- NA
  x[wnotsmooth_u] <- NA

  output <- apply(embed.fullextent(x,timesteps), 1, FUN=function(x) mean(x,na.rm=TRUE))

  output <- xts(output,order.by=time(x))
  output[wnotsmooth_l] <- notsmooth_l
  output[wnotsmooth_u] <- notsmooth_u


  return(output)

}

#' Modified version of embed
#' @param x a numeric vector, matrix or time series
#' @param timesteps integer. Number of timestpes used for smoothing.
#' @return A matrix containing the embedded time series x
#' @author Simon Frey
#' @description Modified version of \code{\link{embed}}.
#' @details By adding the last timesteps-1 entries at the end of the result of embed, the length of the output is equal to the length of x. Otherwise the length of the output would be length(x)-(timesteps-1)
#' @export
#' @seealso \code{\link{embed}}
#' @examples
#' x <- 1:10
#' embed.fullextent(x,3)
embed.fullextent <- function(x,timesteps=timesteps){

  # Modifizierte Version von der Funktion embed (zum Vgl. siehe ?embed)
  # Das Ende des outputs von embed wird um timesteps-1 verlängert,
  # so dass length(embed) == length(x),
  # ansonsten wäre length(embed) == length(x)-(timesteps-1)

  out <- embed(x,timesteps)
  addtail <- matrix(data=NA,ncol=timesteps,nrow=timesteps)
  krev <- rev(1:(timesteps-1))
  for(k in 1:timesteps){
    addtail[k,] <- as.numeric(c(rep(NA,k),rep(x[length(x)-(k-1)],krev[k])))
  }
  out <- rbind(out,addtail[1:(timesteps-1),])
  return(out)
}


#' Smooth a time series on daily basis
#' @param x An xts object.
#' @param upperbound (Optional) A numeric value giving the threshold below which smoothing is performed.
#' @return A smoothed xts with daily time steps.
#' @description Smoothes an time series x on daily basis
#' @details Smoothing is performed if no value within the processed day exeeds upperbound. Smoothing is done by \eqn{(max(x)+min(x))/2}.
#' @author Simon Frey
#' @export
#' @examples
#' data(runoff)
#' x <- smoothdaily(runoff[,1])
#' x <- smoothdaily(runoff[,1], upperbound = quantile(runoff[,1], 0.66))
smoothdaily <- function(x, upperbound=NULL){

  if(!is.xts(x)){
    stop("x muss im XTS Format sein!")
  }

  if(is.null(upperbound)){
    upperbound <- max(x,na.rm = TRUE)
  }

  days <- difftime(max(index(x)),min(index(x)),units="days")
  meanday <- apply.daily(x,FUN=mean,na.rm=TRUE)
  tclass(meanday) <- "Date"

  fx <- format(index(x),format="%Y-%m-%d")

  for(day in 1:days){
    xx <- x[as.character(index(meanday[day]))]

    if(any(!is.na(xx))){
      if(!any(xx > upperbound, na.rm = TRUE)){
      meanday[day] <- (max(xx, na.rm = TRUE) + min(xx,na.rm = TRUE)) / 2
      }
    }

  }

  return(meanday)

}



ifmean <- function(x,upperbound=upperbound,timespan,...){
  if(!is.xts(x)){
    stop("x muss im XTS Format sein!")
  }

  output <- x

  for(k in 1:length(x)){
    mx <- min(length(x),(k+timespan))
    temp <- x[k:mx]
    if(!any(temp>upperbound,...)){
      output[k] <- mean(temp,...)
    }
  }

  return(output)
}



