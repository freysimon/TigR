#' Read a shapefile using RGDAL
#' @description Read a shapefile using the RGDAL library. Essentially this is just a wrapper around \code{\link{readOGR}} from the rgdal package.
#' @author Simon Frey
#' @export
#' @import rgdal
#' @import tools
#' @details This wrapper splits a path to a shapfile into the \code{\link{dirname}} and the \code{\link{basename}}. The latter is passed
#' as dsn argument the first as layer argument to \code{\link{readOGR}}.
#' @param x character string. Path to the shapefile to be read in.
#' @param ... further arguments passed on to \code{\link{readOGR}}
#' @examples 
#'     #### not run ####
#'     x <- "C:/TEMP/someshapefile.shp"
#'     readogr(x)
#'     
#'     #### end not run ####
#'
#' @seealso \code{\link{writeogr}}
#' @seealso \code{\link{writeOGR}}

readogr <- function(x, ...){
  xb <- basename(x)
  xn <- dirname(x)
  
  temp <- readOGR(dsn = xn, layer = file_path_sans_ext(xb), ...)
  
  return(temp)
}

#' Write a shapefile using RGDAL
#' @description Write an ESRI Shapefile using the RGDAL library Essentially this is just a wrapper around \code{\link{writeOGR}} from the rgdal package.
#' @author Simon Frey
#' @export
#' @import rgdal
#' @import tools
#' @details This wrapper splits a path to a shapfile into the \code{\link{dirname}} and the \code{\link{basename}}. The latter is passed
#' as dsn argument the first as layer argument to \code{\link{writeOGR}}.
#' @param x character string. Filename (including path) of the shapefile to be written.
#' @param shp a SpatialPointsDataFrame, SpatialLinesDataFrame, or a SpatialPolygonsDataFrame object to be written as shapefile x.
#' @param driver chracter string. driver used to write x. Per default this is ESRI Shapefile.
#' @param ... further arguments passed on to \code{\link{writeOGR}}
#' @seealso \code{\link{readogr}}
#' @seealso \code{\link{readOGR}}
#' @examples 
#'     #### not run ####
#'     shapefilename <- "C:/TEMP/exampleshapefile.shp"
#'     shapefile <- "EXAMPLE"
#'     writeogr(x = shapefilename, shp = shapefile)
#'     
#'     #### ned not run ####
#' 
writeogr <- function(x, shp, driver = "ESRI Shapefile", ...){
  xb <- basename(x)
  xn <- dirname(x)
  
  writeOGR(obj = shp, dsn = xn, layer = file_path_sans_ext(xb), driver = driver, ...)
}