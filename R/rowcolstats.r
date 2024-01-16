#' Calculate some simple statistic estimators on rows in a matrix or data.frame
#' @description Calculate some simple statistics like median, min, max or quantiles on rows in a matrix or data.frame object
#' @author Simon Frey
#' @export
#' @param x numerical matrix or data.frame.
#' @param fun statistical function that will be calculated. See details.
#' @param na.rm logical. Should NA values be removed?
#' @param q numerical. Quantile that will be estimated. Used only if fun == quantile.
#' @param weights numerical vector. Used if fun == weighted.quantile only. See details.
#' @return numerical vector
#' @details 
#'     This function uses predefined statistical function that can be used:
#'     * min (determine the minimum value)
#'     * max (determine the maximum value)
#'     * mean (estimate the mean value)
#'     * median (estimate the median value)
#'     * quantile (estimate a certain quantile value. Note that values between 0 and 1 are accepted)
#'     * weighted.quantile (calculation of quantiles using individual weights. \link{Quantile} from \code{DescTools} is used.)
#' @examples 
#'     # load runoff data
#'     data(runoff)
#'     
#'     rowStats(runoff, fun = "mean")
#'     rowStats(runoff, fun = "max")
#'     rowStats(runoff, fun = "quantile", q = 0.33)
#'     rowStats(runoff, fun = "weighted.quantiles, q = 0.33, weights = c(1,1,1,4,1,1))
#' @seealso \code{\link{colStats}}
#' @seealso \code{\link{colMin}}
#' @seealso \code{\link{rowMin}}
#' @seealso \code{\link{rowMax}}
#' @seealso \code{\link{colMax}}
#' @md

rowStats <- function(x, fun, na.rm=TRUE, q = 0.1, weights = NULL){
  if(!fun %in% c("min","max","median","mean","quantile", "weighted.quantile")){
    stop("fun must be a statistical funtion. See details in the help site.")
  }
  if(!class(x)[1] %in% c("matrix","data.frame","xts")){
    stop("x must be a matrix, data.frame or an xts object")
  }
  if(q < 0 | q > 1){
    stop("q must be between 0 and 1")
  }
  
  if(fun %in% c("min","max","median","mean")){
    result <- apply(x, MARGIN=c(1), FUN = fun ,na.rm=na.rm)
  }
  
  if(fun == "quantile"){
    # ff <- function(){
    #   quantile(x, probs = q, na.rm = na.rm)
    # }
    result <- apply(x, MARGIN=c(1), FUN = function(x){
      quantile(x, probs=q, na.rm=na.rm)
    })
  }
  
  if(fun == "weighted.quantile"){
    if(ncol(x) != length(weights)){
      stop("Error weights must be the same length as columns in x.")
    }
    result <- apply(x, MARGIN=c(1), FUN = 
                      DescTools::Quantile(x, weights = weights, probs = q, na.rm = na.rm, digits = 2)
    )
  }
  
  return(result)
}


#' Calculate some simple statistic estimators on cols in a matrix or data.frame
#' @description Calculate some simple statistics like median, min, max or quantiles on columns in a matrix or data.frame object
#' @author Simon Frey
#' @export
#' @param x numerical matrix or data.frame.
#' @param fun statistical function that will be calculated. See details.
#' @param na.rm logical. Should NA values be removed?
#' @param q numerical. Quantile that will be estimated. Used only if fun == quantile.
#' #' @param weights numerical vector. Used if fun == weighted.quantile only. See details.
#' @return numerical vector
#' @details This function uses predefined statistical function that can be used:
#' * min (determine the minimum value)
#' * max (determine the maximum value)
#' * mean (estimate the mean value)
#' * median (estimate the median value)
#' * quantile (estimate a certain quantile value. Note that values between 0 and 1 are accepted)
#' * weighted.quantile (calculation of quantiles using individual weights. \link{Quantile} from \code{DescTools} is used.)
#' @examples 
#'     # load runoff data
#'     data(runoff)
#'     
#'     colStats(runoff, fun = "mean")
#'     colStats(runoff, fun = "max")
#'     colStats(runoff, fun = "quantile", q = 0.33)
#' @seealso \code{\link{rowStats}}
#' @seealso \code{\link{colMin}}
#' @seealso \code{\link{rowMin}}
#' @seealso \code{\link{rowMax}}
#' @seealso \code{\link{colMax}}
#' @md

colStats <- function(x, fun, na.rm=TRUE, q = 0.1, weights = NULL){
  if(!fun %in% c("min","max","median","mean","quantile", "weighted.quantile")){
    stop("fun must be a statistical funtion. See details in the help site")
  }
  if(!class(x)[1] %in% c("matrix","data.frame","xts")){
    stop("x must be a matrix, data.frame or an xts object")
  }
  if(q < 0 | q > 1){
    stop("q must be between 0 and 1")
  }
  
  if(fun %in% c("min","max","median","mean")){
    result <- apply(x, MARGIN=c(2), FUN = fun ,na.rm=na.rm)
  }
  
  if(fun == "quantile"){
    # ff <- function(){
    #   quantile(x, probs = q, na.rm = na.rm)
    # }
    result <- apply(x, MARGIN=c(2), FUN = function(x){
      quantile(x, probs=q, na.rm=na.rm)
    })
  }
  
  if(fun == "weighted.quantile"){
    if(nrow(x) != length(weights)){
      stop("Error weights must be the same length as rows in x.")
    }
    result <- apply(x, MARGIN=c(2), FUN = 
                      DescTools::Quantile(x, weights = weights, probs = q, na.rm = na.rm, digits = 2)
    )
  }
  
  return(result)
}
