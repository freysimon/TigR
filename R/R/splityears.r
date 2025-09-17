#' Split an XTS object into years
#' @details Split an xts object into years. One can chose between hydrological years starting with October 1st or calendar years starting with January 1st.
#' @author Simon Frey
#' @description Split an xts object either into hydrological or calendar years.
#' @param x an xts object
#' @param type String. Either 'hydro' for hydrological years or 'calendar' for splitting into calendar years
#' @export
#' @import magrittr
#' @import xts
#' @return A list where each entry contains a single year
#' @seealso \code{\link{yearplot}}
#' @examples  
#'     # load runoff data
#'     data(runoff)
#'     summary(runoff)
#'     
#'     # split data into hydrological years
#'     x <- splityears(runoff)
#'     summary(x)
#'     
#'     # split data into calendar years
#'     x <- splityears(runoff, "calendar")
#'     summary(x)

splityears <- function(x, type = "hydro"){
  
  library(magrittr)
  library(xts)
  
  if(!is.xts(x)){
    stop("x must be an xts object")
  }
  
  if(!type %in% c("hydro","calendar")){
    warning("type must be one of hydro or calendar. Assuming it to be hydro.")
  }
  
  # scan for years
  years <- index(x) %>%
    format("%Y") %>%
    as.numeric() %>%
    unique()
  
  # preallocate a list with length of found years
  temp <- vector("list",length(years))
  names(temp) <- as.character(years)
  
  
  
  # subset ts into singe years
  for(k in 1:(length(years)-1)){
    if(type == "hydro"){
      jj <- paste(years[k],"-10-01/",years[k+1],"-09-30",sep="")
    } else {
      jj <- paste(years[k],"-01-01/",years[k],"-12-31",sep="")
    }
    
    temp[[k]] <- x[jj]
  }
  
  return(temp)
  
}
