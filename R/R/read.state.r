#' Read a COSERO state file
#' @description Read a file containing the state variables of COSERO
#' @author Simon Frey
#' @export
#' @return a list (see Details)
#' @details This function simply reads a state variable file of COSERO and returns a list containing the four parts of the file, each as a matrix:
#'     1) The variables on the subbasin scale (NB)
#'     2) The variables on the grid scale (NB, IZ)
#'     3) The variables on the subgrid scale, i.e. the state of the snow cover (NB,IZ,IKL)
#'     4) The state of the glacier runoff routing
#' @param file path to a state file
#' @examples 
#'     read.state("C:/TEMP/statevar.dmp")
#' @seealso \code{\link{analyse.snow.state}}
#' 
read.state <- function(file = "statevar.dmp"){
  x <- readLines(file)
  x <- strsplit(x, " ")
  
  # get rid of blank entries
  for(k in 1:length(x)){
    w <- which(nchar(x[[k]]) == 0)
    x[[k]] <- x[[k]][-w]
  }
  
  # get dimensions of textblocks
  blocks <- list()
  j = 1
  for(k in 1:length(x)){
    
    if(length(x[[k]]) <= 1){
      blocks[[j]] <- k
      j = j + 1
    }
  }
  
  blocks[[1]] <- c(1,blocks[[1]]-1)
  blocks[[2]] <- c(blocks[[1]][2]+2,blocks[[2]]-1)
  blocks[[3]] <- c(blocks[[2]][2]+2,blocks[[3]]-1)
  blocks[[4]] <- c(blocks[[3]][2]+2, length(x))
  
  # read state variables on subbasin scale
  NBvar <- matrix(nrow = diff(blocks[[1]]), ncol = 5, data = NA)
  colnames(NBvar) <- x[[blocks[[1]][1]]]
  j = 1
  for(k in (blocks[[1]][1]+1):blocks[[1]][2]){
    NBvar[j,] <- as.numeric(x[[k]])
    j = j + 1
  }
  
  # read state variables on grid scale
  IZvar <- matrix(nrow = diff(blocks[[2]]),ncol = 12, data = NA)
  colnames(IZvar) <- x[[blocks[[2]][1]]]
  j = 1
  for(k in (blocks[[2]][1]+1):blocks[[2]][2]){
    IZvar[j,] <- as.numeric(x[[k]])
    j = j + 1
  }
  
  # read state variables on subgrid scale
  IKLvar <- matrix(nrow = diff(blocks[[3]]), ncol = 8, data = NA)
  colnames(IKLvar) <- x[[blocks[[3]][1]]]
  j = 1
  for(k in (blocks[[3]][1]+1):blocks[[3]][2]){
    IKLvar[j,] <- as.numeric(x[[k]])
    j = j + 1
  }
  
  
  # read state variables of the glaciers
  glacvar <- matrix(nrow = diff(blocks[[4]]), ncol = 2, data = NA)
  colnames(glacvar) <- x[[blocks[[4]][1]]]
  j = 1
  for(k in (blocks[[4]][1]+1):blocks[[4]][2]){
    glacvar[j,] <- as.numeric(x[[k]])
    j = j + 1
  }
  
  return(list(NBvar,IZvar,IKLvar,glacvar))
}