#' Check for unique values
#' @description Check whether a variable within a vetcor is unique
#' @author Simon Frey
#' @param x vector to be analysed.
#' @param ... additional parameters passed to \code{\link{count}}
#' @details This function uses the \code{\link{count}} function from the plyr package and returns all entries with a frquency greater than one. Works with character and numeric vectors
#' @import plyr
#' @export
#' @return a boolean vector
#' @seealso \code{\link{unique}}
is.unique <- function(x, ...){
  library(plyr)
  cx <- as.data.frame(matrix(data=NA, nrow = nrow(cx), ncol = 2))
  colnames(cx) <- c("x","freq")
  countx <- count(x, ...elt())
  if(class(x) == "character"){
    cx[,1] <- as.character(countx[,1])
  } else if(class(x) %in% c("numeric","integer")){
    cx[,1] <- as.numeric(countx[,1])
  } else {
    cx[,1] <- as.factor(countx[,1])
  }
  
  cx[,2] <- as.integer(countx[,2])
  cx[,2] == 1
  
  
}