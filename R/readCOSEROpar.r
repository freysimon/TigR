#' Read a COSERO parameterfile
#' @description Read a COSERO parameter file and return the values as list.
#' @author Simon Frey
#' @export
#' @param x charcater string. A COSERO parameter file
#' @param NB numerical. Return only the parameters of a certain subbasin. May be a single number or a numerical vector.
#' @return A list containing three lists: Nr 1 contains the dimensions, Nr 2 contains the headers, and number 3 the values
read_COSERO_par <- function(x, NB=NULL){
  ll <- readLines(x, encoding = "UTF-8")
  
  dimensions <- ll[1 : (which(ll == "** Parameters **")-1)]
  parameters <- ll[which(ll == "** Parameters **") : length(ll)]
  
  nr_of_pars <- length(which(parameters == "####"))
  pars <- parameters[which(parameters == "####") + 1]
  
  if(!is.null(NB)){
    NBS <- substrRight(gsub(" 10","",pars),3)
    pars <- pars[which(NBS %in% sprintf("%03i",NB))]
  }
  
  #### headers auslesen ####
  
  parheaders <- list()
  pb <- txtProgressBar(min = 0, max = length(pars), style = 3)
  for(k in 1:length(pars)){
    parstart <- which(parameters == pars[k]) + 1
    parend <- which(parameters == pars[k]) + 4
    
    if(parameters[parend - 1] == "MONATE"){
      parend <- parend + 1
    }
    
    parheaders[[k]] <- parameters[parstart : parend]
    setTxtProgressBar(pb, value = k)
  }
  names(parheaders) <- pars
  
 
  
  #### Parameterwerte auslesen ####
  parvals <- list()
  pb <- txtProgressBar(min = 0, max = length(pars), style = 3)
  for(k in 1:length(pars)){
    parstart <- (which(parameters == pars[k]) + 5)
    parend <- as.numeric(parameters[which(parameters == pars[k]) + 3])
    if(is.na(parend)){
      parend <- as.numeric(parameters[which(parameters == pars[k]) + 4])
      parstart <- parstart + 1
    }
    parvals[[k]] <- parameters[parstart : (parstart + parend - 1)]
    setTxtProgressBar(pb, value = k)
  }
  names(parvals) <- pars
  
  
  return(list(dimensions,parheaders, parvals))
}

#' Write a COSERO parameter file
#' @description write a previously loaded COSERO parameter file
#' @author Simon Frey
#' @export
#' @param x previously loaded parameter file
#' @param file character string. The filename of the new parameter file.
#' @return Nothing is returned to R

write_COSERO_par <- function(x, file = "parameter_COSERO.par"){
  options("encoding" = "UTF-8")
  if(length(x) != 3){
    stop("x must be a previously read in paramter file")
  }
  
  write.table(x[[1]], file = file, col.names = FALSE, row.names = FALSE, sep = "", quote = FALSE, fileEncoding = "UTF-8")
  write.table("** Parameters **", file = file, col.names = FALSE, row.names = FALSE, sep = "", quote = FALSE, append = TRUE,
              fileEncoding = "UTF-8")
  for(k in 1:length(x[[2]])){
    write.table("####", file = file, col.names = FALSE, row.names = FALSE, sep = "", quote = FALSE, append = TRUE,
                fileEncoding = "UTF-8")
    write.table(names(x[[2]][k]), file = file, col.names = FALSE, row.names = FALSE, sep = "", quote = FALSE, append = TRUE,
                fileEncoding = "UTF-8")
    write.table(x[[2]][[k]], file = file, col.names = FALSE, row.names = FALSE, sep = "", quote = FALSE, append = TRUE,
                fileEncoding = "UTF-8")
    write.table(x[[3]][[k]], file = file, col.names = FALSE, row.names = FALSE, sep = "", quote = FALSE, append = TRUE,
                fileEncoding = "UTF-8")
  }
}
