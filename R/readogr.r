#' Read a shapefile using RGDAL
#' @description Read a shapefile using the RGDAL library. Essentially this is just a wrapper around \code{\link{readOGR}} from the rgdal package
#' @author Simon Frey
#' @export
#' @import rgdal
#' @import tools
#' @details This wrapper splits a path to a shapfile into the \code{\link{dirname}} and the \code{\link{basename}}. The latter is passed
#' as dsn agrument the first as layer argument to \code{\link{readOGR}}.
#' @param x character string. Path to the shapefile to be read in.
#' @param ... further arguments passed on to \code{\link{readOGR}}
#' @examples 
#'     #### not run ####
#'     x <- "C:/TEMP/someshapefile.shp"
#'     readogr(x)
#'     

readogr <- function(x, ...){
  xb <- basename(x)
  xn <- dirname(x)
  
  temp <- readOGR(dsn = xn, layer = file_path_sans_ext(xb), ...)
  
  return(temp)
}