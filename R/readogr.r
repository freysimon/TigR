#' Read a shapefile using vect
#' @description Read a shapefile using the RGDAL library. Essentially this is just a wrapper around \code{\link{vect}} from the terra package.
#' @author Simon Frey
#' @export
#' @import terra
#' @import tools
#' @details 
#'     DEPRECATED!!! 
#'     This wrapper splits a path to a shapfile into the \code{\link{dirname}} and the \code{\link{basename}}. The latter is passed
#'     as dsn argument the first as layer argument to \code{readOGR}.
#'     
#'     This now is just a wrapper of \code{\link{vect}} from the \code{\link{terra}} package and kept for compability issues.
#' @param x character string. Path to the shapefile to be read in.
#' @param ... further arguments passed on to \code{\link{vect}}
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
  temp <- terra::vect(x, ...)
  
  return(temp)
}

#' Write a shapefile using terra
#' @description Write an ESRI Shapefile using the RGDAL library Essentially this is just a wrapper around \code{\link{writeVector}} from the terra package.
#' @author Simon Frey
#' @export
#' @import terra
#' @import tools
#' @details 
#'     DEPRECATED! This wrapper splits a path to a shapefile into the \code{\link{dirname}} and the \code{\link{basename}}. The latter is passed
#'     as dsn argument the first as layer argument to \code{writeOGR}.
#'     
#'     This now ist just a wrapper around \code{\link{writeVector}} from the \code{terra} package and kept for compability issues.
#' @param filename character string. Filename (including path) of the shapefile to be written.
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
#'     #### end not run ####
#' 
writeogr <- function(filename, shp, driver = "ESRI Shapefile", ...){

  writeOGR(x = shp,  filename = filename, filetype = driver, ...)
}