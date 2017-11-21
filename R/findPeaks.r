#' Find peak(s) in a vector
#' @author stas g
#' @param x a numerical vector
#' @param m integer. How many points on either side must be smaller than the peak.
#' @param order character. Order data along their data values. Accepts 'i' and 'd' for increasing / decreasing, respectively, and 'n' for no order.
#' @param na.rm logical. Should na values be removed before the calculation
#' @export
#' @description A simple algorithm to find local maxima/minima in sequential data
#' @details The function takes an ordered sequence (vector) of values x and a number m and returns a vector of indices of local peaks in x. 
#' A (local) peak is defined as a point such that m points either side of it has a lower or equal value to it.
#' 
#'     Thus, m can be used adjust the sensitivity of the peak detection procedure: 
#'     larger m will result in fewer peaks, whilst smaller values of m will result in more peaks found.
#' @examples 
#' set.seed(321)
#' w <- abs(rnorm(1000))
#' w[sample(1 : 1000, 25)] <- rpois(25, 5)
#' w[sample(1 : 1000, 25)] <- rpois(25, 10)

#' par(mfrow = c(2, 2))
#' for(k in c(10, 20, 50, 250)){
#'  p <- find_peaks(w, m = k)
#'  ind <- rep(1, length(w))
#'  ind[p] <- 2
#'  plot(w, type = 'l', main = paste0('m = ', k))
#'  points(p, w[p], col = 'red', pch = 19)
#'}

find_peaks <- function (x, m = 3, order = 'd', na.rm = TRUE){
  # check for order specification
  if(!order %in% c("i","d","n")){
    warning("Cannot recognize order specification. Ordering is suppressed.")
  }
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  if(na.rm){
    shape[which(is.na(shape))] <- 0
  }
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  
  if(order == 'd'){
    pks <- pks[order(x[pks], decreasing = TRUE)]
  } else if(order == 'i'){
    pks <- pks[order(x[pks], decreasing = FALSE)]
  } 
  
  return(pks)
}