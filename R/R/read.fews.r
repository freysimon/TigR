#' read data in the fews clipboard format
#' @description read date in the format when copying data from fews to the clipboard
#' @author Simon Frey
#' @export
#' @import xts
#' @param x character string. path to the file that will be read.
#' @param dec chracter. decimal seperator
#' @param param logical. Should the parameter be included in the header?
#' @param loc.name logical. Should the location name be included in the header?
#' @param loc.ID logical. Should the location ID be included in the header?
#' @param date.t0 logical. Should the date of t0 be included in the header?
#' @param time.t0 logical. Should the time of t0 be included in the header?
#' @details The xts object that will be returned will be contain header information based on the parameters param, loc.name, loc.ID, date.t0 and time.t0. All that are TRUE will be concatenated using "_@_" as separator.
#' @return an xts object
#' @examples 
#'    ### not run ###
#'    xx <- read.fews(x, dec=",", param = T, loc.name=T)

read.fews <- function(x, dec = ",", param = T, loc.name = T, loc.ID = F, date.t0 = F, time.t0 = F){
  
  library(xts)
  # read header first

  parameter <- read.table(x, colClasses = "character", nrow=1, sep = "\t")
  timezone <- parameter[1]
  length.of.header <- length(parameter)
  parameter <- parameter[-1]
  location.name <- read.table(x, colClasses = "character", nrow=1, skip = 2 , sep = "\t")
  location.ID <- read.table(x, colClasses = "character", nrow=1, skip = 3 , sep = "\t")
  DATE.t0 <- read.table(x, colClasses = "character", nrow=1, skip = 5 , sep = "\t")
  TIME.t0 <- read.table(x, colClasses = "character", nrow=1, skip = 6 , sep = "\t")
  
  location.name <- location.name[2:length.of.header]
  location.ID <- location.ID[2:length.of.header]
  DATE.t0 <- DATE.t0[2:length.of.header]
  TIME.t0 <- TIME.t0[2:length.of.header]

  f <- read.xts(x, datecolumns = 1, sep = "\t", dec = dec, format = "%Y-%m-%d %H:%M", skip = 7, header=F, cut.prefix = 3)
  f <- rm.dls(f)
  
  header=NULL
  
  if(param){
    if(is.null(header)){
      header = parameter
    } else {
      header <- paste(header, parameter, sep = "_@_")
    }
  }
  if(loc.name){
    if(is.null(header)){
      header <- location.name
    } else {
      header <- paste(header, location.name, sep = "_@_")
    }
  }
  if(loc.ID){
    if(is.null(header)){
      header <- location.ID
    } else {
      header <- paste(header, location.ID, sep = "_@_")
    }
  }
  if(date.t0){
    if(is.null(header)){
      header <- DATE.t0
    } else {
      header <- paste(header, DATE.t0, sep = "_@_")
    }
  }
  if(time.t0){
    if(is.null(header)){
      header <- TIME.t0
    } else {
      header <- paste(header, TIME.t0, sep = "_@_")
    }
  }
  
  if(!is.null(header)){
    colnames(f) <- header
  }
  
  return(f)  

}
