#' Edit a parameter in a COSERO parameter file
#' @author Simon Frey
#' @description Edit a parameter in a previouly loaded COSERO parameter file. Either a single value or a vector is accepted.
#' @details If val is of length=1, it will be repeated using \code{\link{rep}} to the length of parameter values in the parameter file. Use \code{\link{read_COSERO_par}} for reading a COSERO parameter file.
#' @seealso \link{read_COSERO_par} for reading a COSERO parameter file
#' @seealso \link{write_COSERO_par} for writing a COSERO parameter file
#' @param file A previously loaded COSERO parameter file
#' @param val Value(s) to be edited. May be a single value (character or numeric) or a vector of values
#' @param param Name of the parameter that will be edited.
#' @return A list of the same structure as \code{file}
#' @export
#' 

edit_par <- function(file, val, param){
  
  x <- file
  
  options("encoding" = "UTF-8")
  if(length(x) != 3){
    stop("file must be a previously read in paramter file")
  }
  
  if(!is.character(param)){
    stop("param must be a character")
  }
  
  # locate parameter names in list
  w <- agrep(param, names(x[[3]]), ignore.case = TRUE, max.distance = 0)
  
  if(length(w) == 0){
    stop(paste("Could not find parameter ",param,". Please check spelling.", sep = ""))
  }
  if(length(w) > 1){
    stop(paste("No unique parameter found. Possible findings are: ", paste(names(x[[3]][w]), collapse = ","), sep=""))
  }
  
  if(length(val) == 1){
    x[[3]][w][[1]] <- rep(val, length(x[[3]][w][[1]]))
  } else {
    if(length(val) == length(x[[3]][w][[1]])){
      x[[3]][w][[1]] <- val
    } else {
      stop(paste("val has the length of ", length(val),", which is not equal to the length required (", length(x[[3]][w][[1]]),")",sep=""))
    }
  }
  
  return(x)
  
}