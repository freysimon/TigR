#' Columnwise apply a function to receive daily agreggated values
#' @param x an xts object
#' @param FUN an R function
#' @param PB a character indicating whether and what kind of progress bar should be drawn. See details.
#' @param ... additional arguments to FUN
#' @description Apply a specified function to each column of an xts object creating daily values
#' @details A simple mechanism to use \code{\link{apply.daily}} to each column of an xts object. 
#'     
#'     By setting PB, an optional progressbar can be drawn: "w" or "win" draws a \code{\link{winProgressBar}}, 
#'     "t" or "txt" draws a \code{\link{txtProgressBar}} and "n" or "none" (the default) suppresses the progressbar.
#'     
#' @author Simon Frey
#' @export
#' @import xts
#' @return An xts object containing daily values

apply.daily.columns <- function(x, FUN, PB = "n", ...){
  library(xts)
  if(class(x)[1] != "xts"){
    stop("x must be an xts object")
  }
  if(!PB %in% c("w", "win", "t", "txt", "n", "none")){
    warning("PB not recognized.")
    PB <- "n"
  }
  
  # get dimensions of x
  dim.in <- dim(x)
  
  # get dimensions of processed time series
  temp <- apply.daily(x[,1], FUN = FUN)
  dim.out <- dim(temp)
  
  # create new aggregated xts object with dim.out
  out <- matrix(nrow = dim.out[1], ncol = dim.in[2], data = NA)
  out <- xts(out, order.by = index(temp))
  
  rm(temp)
  
  if(!PB %in% c("n", "none")){
    if(PB %in% c("win", "w")){
      pb <- winProgressBar(title = "Aggregating to daily data", label = "",
                           min = 0, max = dim.in[2], initial = 0, width = 400)
    }  
    if(PB %in% c("txt", "t")){
      pb <- txtProgressBar(min = 0, max = dim.in[2], initial = 0)
    }
  }
  
  # fill out with data
  for(j in 1:dim.in[2]){
    
    out[,j] <- apply.daily(x[,j], FUN = FUN, ...)
    
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
  index(out) <- format(index(out), format = "%Y-%m-%d")
  
  return(out)
  
}