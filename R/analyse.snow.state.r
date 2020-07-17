#' Analyse snow state
#' @description Analyse the state of the snow cover in a state file
#' @author Simon Frey
#' @export
#' @param state output of \code{\link{read.state}} or, if read.file == TRUE, a string pointing to a state file.
#' @param NB numeric. Number of subbasin that will be analysed.
#' @param IZ numeric. Number of cell in the subbasin NB that will be analysed.
#' @param read.file logical. If TRUE a state file will be read in. In this case, state has to be a string pointing to a state file.
#' @param show.plot logical. Should the result be plotted?
#' @param ... additional arguments passed on to \code{\link{dev.new.file}}
#' @seealso \code{\link{read.state}}
#' @examples 
#'     x <- read.state("C:/TEMP/statevar.dmp")
#'     analyse.snow.state(state = x,NB=1,IZ=1)
#'     
#'     analyse.snow.state("C:/TEMP/statevar.dmp",NB=1,IZ=1,read.file=TRUE)
#'     
#' @return a list containing the state of the snow cover on grid scale (1) and subgrid scale (2)
#' 
analyse.snow.state <- function(state, NB, IZ, read.file = FALSE, show.plot = FALSE, ...){
  
  # check if read.file == TRUE
  if(read.file){
    #check if state is a string
    if(!is.character(state)){
      stop("Error: state has to be a string pointing to a state file")
    }
    if(!file.exists(state)){
      stop(paste("Error: Cannot find ",state,sep=""))
    }
    state <- read.state(state)
  }
  
  
  IZvar <- subset(state[[2]], state[[2]][,1] == NB & state[[2]][,2] == IZ)
  IKLvar <- subset(state[[3]], state[[3]][,1] == NB & state[[3]][,2] == IZ)
  
  if(show.plot){
    d <- dev.new.file(...)
    layout(matrix(c(1,1,1,1,1,2:6),2,5, byrow = TRUE))
    barplot(IZvar[,3:10], main = paste("NB: ",NB," IZ: ",IZ,sep=), ylab = "mm", col = "steelblue")
    
    for(k in 1:5){
      barplot(IKLvar[,k+3], main = colnames(IKLvar)[k+3], ylab = "mm", col = "lightblue")
    }
    
    if(d %in% c("png","pdf")){
      dev.off()
    }
  }
  
  return(list(IZvar,IKLvar))
}
