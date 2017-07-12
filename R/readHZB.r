#' read a file provides by HZB
#' @description read a file provided by HZB (e.g. from ehyd)
#' @author Simon Frey
#' @export
#' @param file file to read
#' @param name logical. Should the return value contain a name given by the keyword in the file?
#' @param keyword character. Keyword used for naming the xts object.
#' @param format format for the formatting date and time (see \code{\link{as.POSIXct}})
#' @param tz timezone
#' @param ... agruments passed on to \code{\link{read.xts}}
#' @examples 
#'  ### not run ###
#'  readHZB(x, TRUE)
#'    
#' @seealso \code{\link{read.table}}
#' @seealso \code{\link{readLines}}
#' @seealso \code{\link{read.xts}}
#' @return an xts object 

readHZB <- function(file, name=TRUE, keyword = "Messstelle", format = "%d.%m.%Y %H:%M",tz="UTC", ...){
  

  #########################################################
  #                                                       #
  # Funktion, die HZB-Dateien (z.B. von ehyd) einliest,   #
  # und die Werte als xts-Zeitreihe zurückgibt.           #
  # Mittels name=TRUE kann der Name der Messstelle        #
  # zurückgegeben werden. Dies geschieht als colname.     #
  #                                                       #
  # Autor: S. Frey                                        #
  # Datum: 03.2016                                        #
  #                                                       #
  #########################################################
  
  # Libraries laden
  library(xts)

    
  # Überprüfen, ob file existiert
  if(!file.exists(file)) stop(paste(file,"nicht gefunden!",sep=""))
  
  # Die ersten 30 Zeilen lesen und hoffen, dass der Header nicht länger ist
  xh <- readLines(file,n=30)
  
  # Nach dem Schlagwort "Werte:" suchen, welches den Anfang der Daten markiert
  end.of.header <- grep("Werte:",xh)
  
  # Nach dem Schlagwort "Messstelle:" suchen und als name verwenden
  if(name){
    xn <- grep(paste(keyword,":",sep="") ,xh)
    xn <- strsplit(xh[xn],";",fixed=TRUE)[[1]][2]
  }
  
  # Daten einlesen
  x <- read.xts(file, header = FALSE, datecolumns = 1, sep = ";",
                format = format, tz = tz, skip = end.of.header, 
                dec = ",", na.strings = "Lücke", ...)

  
  if(name){
    colnames(x) <- xn
  }
  
  # xts-Zeitreihe zurückgeben
  return(x)

}