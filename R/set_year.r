#' Alter the year in a POSIX-Object or Series
#' @author Simon Frey
#' @export
#' @description Alter the year in a POSIX object or a vector of such objects
#' @param x a POSIX object or a vector of such objects
#' @param year character or character vector. The "new" year that the new POSIX object(s) will be hold
#' @return A POSIX object or a vector of POSIX objects
#' @details If year == NULL the year of \link{Sys.Date} will be used. Note, that, if year is a single character value, it is interpreted as the first value in case that x is a vector.
#' @examples 
#'     x <- as.POSIXct(c("2022-12-30","2022-12-31","2023-01-01","2023-01-02"))
#'     year <- "2023"
#'     set_year(x, year)
#'     
#'     #################################
#'     
#'     x <- as.POSIXct("2021-01-01")
#'     year <- "2023"
#'     set_year(x,year)
#'     
#'     #################################
#'      
#'     x <- as.POSIXct(c("2022-12-30","2022-12-31","2023-01-01","2023-01-02"))
#'     year <- c("2023","2023","2024","2024")
#'     set_year(x,year)
#'     
#'     #################################


set_year <- function(x, year = NULL){
  
  if(!"POSIXct" %in% class(x)){
    stop("x must be a POSIX object")
  }
  
  if(is.null(year)){
    year <- format(Sys.Date(), format = "%Y")
  }
  
  if(length(year) != length(x)){
    if(length(year) > 1){
      warning("lenghts of x and year differ. Only the first element of year is used.")
      year <- year[1]
    }
    

  
    if(length(x) == 1){
      tzx <- xts::tzone(x)
      x.year <- as.character(format(x, format="%Y"))
  
      x.char <- as.character(x)
      x.char.new <- gsub(x.year,year,x.char,fixed=TRUE)
      
      x.new <- as.POSIXct(x.char.new, tz=tzx)
    }
  
    if(length(x) > 1){
      tzx <- xts::tzone(x)
      x.year <- as.character(format(x[1], format="%Y"))
      x.char <- as.character(x[1])
      x.char.new <- gsub(x.year,year,x.char,fixed=TRUE)
    
      x.new <- x
    
      for(n in 1:length(x)){
        if(n==1){
          x.new[n] <- as.POSIXct(x.char.new, tz=tzx)
        }
        dt <- difftime(x[n],x[n-1])
        x.new[n] <- x.new[n-1]+dt
      
      }
    
    }
    
  } else {
    
    tzx <- xts::tzone(x)
    x.year <- as.character(format(x, format="%Y"))
    
    x.char <- as.character(x)
    
    for(k in 1:length(x)){
      x.char[k] <- gsub(x.year[k], year[k], x.char[k], fixed=TRUE)
    }
    
    x.new <- as.POSIXct(x.char, tz=tzx)
    
  }
  
  
  return(x.new)
  
}