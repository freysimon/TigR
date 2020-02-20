#' Extract a parameter from a COSERO parameter file
#' @description Extract a parameter from a COSERO parameter file.
#' @author Simon Frey
#' @export
#' @param x character. The name of the parameter that is extracted
#' @param parafile character. The parameter file. May be a path to a file or a object retrieved by \code{\link{read_COSERO_par}}
#' @param dims a matrix giving the dimensions of the parafile or NULL. If the latter, the dimensions of parafile will be determined.
#' @seealso \code{\link{read_COSERO_par}}
#' @examples 
#'     ### do not run
#'     get.parameter("CTMAX",par)
#' @return A matrix consisting of NB, IZ, NZ and the parameter is returned

get.parameter <- function(x, parafile, dims = NULL){
  
  if(is.character(parafile)){
    if(!file.exists(parafile)){
      stop("parafile not found. It must be a COSERO parameter file")
    } else {
      print("reading COSERO parameter file")
      parafile <- read_COSERO_par(parafile)
    }
  }
  if(is.null(dims)){
    dims <- get.dimensions(parafile)
  }
  
  
  # grep parametername
  w <- grep(x, names(parafile[[3]]))
  
  for(k in w){
    if(k == w[1]){
      out <- as.numeric(parafile[[3]][[k]])
    } else {
      out <- as.numeric(c(out, parafile[[3]][[k]]))
    }
  }
  
  out <- cbind(dims, out)
  
  colnames(out) <- c("NB","IZ","NZ",x)
  
  return(out)
  
}