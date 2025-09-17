#' Check for unique values
#' @description Check whether a variable within a vector is unique
#' @author Simon Frey
#' @param x vector to be analysed.
#' @param ... additional parameters passed to \code{\link{count}}
#' @details This function uses the \code{\link{count}} function from the plyr package and returns all entries with a frquency greater than one. Works with character and numeric vectors
#' @import plyr
#' @export
#' @return a boolean vector
#' @seealso \code{\link{unique}}
#' 
is.unique <- function(x, ...){
  library(plyr)
  countx <- count(x, ...)
  
  cx <- as.data.frame(matrix(data=NA, nrow = nrow(countx), ncol = 2))
  colnames(cx) <- c("x","freq")
  
  if(any(class(x) %in% c("numeric","integer"))){
    cx[,1] <- as.numeric(countx[,1])
  } else if(any(class(x) %in% c("POSIXct", "POSIXt"))){
    cx[,1] <- countx[,1]
  } else if(class(x) == "character"){
    cx[,1] <- as.character(countx[,1])
  } else {
    cx[,1] <- as.factor(countx[,1])
  }
  
  cx[,2] <- as.integer(countx[,2])
  
  return(cx[,2] == 1)
  
}

#' Check for unique values
#' @description Check whether and which, if any, variable within a vector is unique
#' @author Simon Frey
#' @param x vector to be analysed.
#' @param ... additional parameters passed to \code{\link{count}}
#' @details This function uses the \code{\link{count}} function from the plyr package and returns all entries with a frquency greater than one. Works with character and numeric vectors
#' @import plyr
#' @export
#' @return a vector containing the unique values of x
#' @seealso \code{\link{unique}}
#' 
which.is.unique <- function(x, ...){
  library(plyr)
  countx <- count(x, ...)
  
  cx <- as.data.frame(matrix(data=NA, nrow = nrow(countx), ncol = 2))
  colnames(cx) <- c("x","freq")
  if(any(class(x) %in% c("numeric","integer"))){
    cx[,1] <- as.numeric(countx[,1])
  } else if(any(class(x) %in% c("POSIXct", "POSIXt"))){
    cx[,1] <- countx[,1]
  } else if(class(x) == "character"){
    cx[,1] <- as.character(countx[,1])
  } else {
    cx[,1] <- as.factor(countx[,1])
  }
  
  cx[,2] <- as.integer(countx[,2])
  
  return(cx[cx[,2] == 1,1])
  
}