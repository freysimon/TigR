#' compute snow distribution accoding to nvar
#' @description compute snow distribution in COSERO according to the parameter nvar
#' @author Simon Frey
#' @param SWE amount of (new) snow to be dirstibuted over a grid cell
#' @param nvar parameter from COSERO
#' @return A vector of length 5 with the snow distribution
#' @examples 
#'     distrib(10,5)
#'     distrib(10,0)
#' @seealso \link{read_COSERO_par} for reading a COSERO parameter file
#' @details
#'     formula : SWE(IKL) = exp(m + s * SWE)
#'     
#'     m = mean(SWE)
#'     s = sd(SWE)
#'     
#'     nvar = variance of SWE
#'     
#'     Only apply if 5 snow classes are used!

distrib <- function(SWE, nvar){
  U = c(-1.28,-0.53,0,0.53,1.28)
  X = rep(NA,5)
  
  XVAR = nvar * SWE * SWE
  
  TMP = XVAR/(SWE*SWE)
  TMP = TMP+1
  TMP = log(TMP)
  SIGMA = sqrt(TMP)
  ETA = log(SWE) - TMP/2
  SUM = 0
  
  for(i in 1:5){
    TMP = U[i]*SIGMA+ETA
    X[i] = exp(TMP)
    SUM = SUM+X[i]
  }
  
  TMP = SWE/SUM
  
  for(i in 1:5){
    X[i] = X[i]*TMP*5
  }
  
  return(X)
  
}