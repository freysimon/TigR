#' Get dimensions from COSERO parameters
#' @author Simon Frey
#' @export
#' @description Get dimensions (NB, IZ, NZ) from COSERO parameters.
#' @param x character string pointing towards a COSERO parameter file or giving an object derived from read_COSERO_par
#' @return a matrix giving the dimensions NB, IZ, NZ
#' @examples 
#'     ### do not run ###
#'     get.dimensions("path/to/parameter_COSERO.par")
#'     
#'     x <- read_COSERO_par("path/to/parameter_COSERO.par")
#'     get.dimensions(x)
#' @seealso \code{\link{read_COSERO_par}}

get.dimensions <- function(x){
  
  if(is.character(x)){
    if(!file.exists(x)){
      stop("x not found. It must be a COSERO parameter file")
    } else {
      print("reading COSERO parameter file")
      x <- read_COSERO_par(x)
    }
  }
  
  # keep only dimensions part
  x <- x[[1]]
  maxNB <- as.numeric(x[which(x == "NBASIN") + 1])
  IZS <- vector()
  
  NBSchar <- as.character(c(1:maxNB))
  NBS <- c(1:maxNB)
  
  for(k in 1:maxNB){
    if(NBS[k] < 10){
      NBSchar[k] <- paste("00",NBSchar[k],sep="")
    } else if (NBS[k] < 100){
      NBSchar[k] <- paste("0",NBSchar[k],sep="")
    }
  }
  
  for(k in 1:maxNB){
    IZS[k] <- as.numeric(x[which(x == paste("IZONE_",NBSchar[k],sep=""))+1])
  }
  
  # Disaggregating IZS and building NZ
  NZ <- c(1:sum(IZS))
  IZ <- c(1:IZS[1])
  NB <- rep(1,IZS[1])
  
  for(k in 2:maxNB){
    IZ <- c(IZ, c(1:IZS[k]))
    NB <- c(NB, rep(k,IZS[k]))
  }
  
  NBIZNZ <- cbind(NB,IZ,NZ)
  colnames(NBIZNZ) <- c("NB","IZ","NZ")
  
  return(NBIZNZ)
}