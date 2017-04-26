#################################################
#                                               #
# Sammlung mehr oder minder nützlicher          #
# Funktionen um R zu erweitern.                 #
#                                               #
# Autor: Simon Frey                             #
#                                               #
#################################################


#' Return last n characters of a string
#'
#' @param x character. A character string
#' @param n integer. Number of characters
#' @return character string of the last n characters of x
#' @export
#' @seealso \code{\link{substr}}
#' @author Simon Frey
#' @examples
#' substrRight('hello',1)
#' substrRight('more than one word', 6)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


# Einem string  (z.B. Pfad) einen slash hinzufügen
#' Add a slash to an existing character string
#' @param x character string
#' @return character string
#' @description Adds a slash (/) to x, if x does not already end with one.
#' @author Simon Frey
#' @export
#' @examples
#' addSlash("C:/TEMP)
#' addSlash("C:/TEMP/)
addSlash <- function(x){
  if(substrRight(x,1) != "/"){
    x <- paste(x,"/",sep="")
  }
  return(x)
}

# Falls na.locf nicht funktioniert
#' Alternative to na.locf
#' @param z data.frame or matrix
#' @description honestly, can't remember why and what it does
#' @author Simon Frey
rna <- function(z) {
  y <- c(NA, head(z, -1))
  z <- ifelse(is.na(z), y, z)
  if(any(is.na(z))){
    Recall(z)
  } else
  {
    z
  }
}


#' Convert mm to m^3/s
#' @param input numeric. May be a vector, matrix, timeseries, etc
#' @param area numeric. Area in m^2
#' @param dt numeric. time step
#' @return numeric of the same type as input
#' @export
#' @author Simon Frey
#' @examples
#' data("runoff")
#' mm2m3s(input = as.numeric(input[,1]), area = 20000000, dt = 3600)
#' mm2m3s(input = runoff, area = c(rep(20000000,3),rep(50000000,3)), dt = 3600)
mm2m3s <- function(input,area,dt){
  # input = Abfluss in mm
  # area = Gebietsfläche in m²
  # dt = Zeitschritt in s
  # output = Abfluss in m³/s
  output <- input/(dt*1000)*area
  return(output)
}

#' Convert m^3/s in mm
#' @param input numeric. May be a vector, matrix, timeseries, etc
#' @param area numeric. Area in m^2
#' @param dt numeric. time step
#' @return numeric of the same type as input
#' @author Simon Frey
#' @export
#' @examples
#' data("Nile")
#' m3s2mm(input = Nile, area = 6852000000, dt = 86400 * 365)
m3s2mm <- function(input,area,dt){
  # input = Abfluss in m³/s
  # area = Gebietsfläche in m²
  # dt = Zeitschritt in s
  # output = Abfluss in mm

  output <- input*1000*dt/area
  return(output)
}


# Suche nach Doppelten Zeitflags. Falls welche gefunden werden, wird der Wert beider Einträge gemittelt.
#' Remove duplicated entries
#' @param x xts object
#' @return xts object
#' @author Simon Frey
#' @export
#' @description Searches duplicated timestamps in an xts object. If some are found, the values of this entry and its neighbours are averaged
clear.duplicated <- function(x){
  any.duplicated <- anyDuplicated(time(x))
  if(length(any.duplicated) > 0){
    if(any.duplicated != 0){
      which.duplicated <- which(time(x[any.duplicated]) == time(x))
      x[which.duplicated[1],] <- mean(x[which.duplicated[1],])
      x <- x[-which.duplicated[1]]
    }
  }
  return(x)
}

# Schaltjahre erkenntn
#' Check for leap years
#' @param year numeric
#' @return logical. TRUE if year is a leap year
#' @author Simon Frey
#' @export
#' @examples
#' is.leapyear(2000)
#' is.leapyear(2001)
#' is.leapyear(c(1970:2100))
#' @seealso \url{http://en.wikipedia.org/wiki/Leap_year}
is.leapyear=function(year){
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

# XTS auf Stundenwerte agreggieren
#' aggregate xts object to hourly values
#' @param x xts object
#' @param FUN function to apply to x
#' @param ... additonal arguments passed to apply
#' @author Simon Frey
#' @export
#' @description Aggregate an xts object to hourly values. It is a wrapper of \code{\link{period.apply}} with endpoints = "hours"
#' @seealso \code{\link{period.apply}}
apply.hourly <- function(x, FUN, ...){
  ap <- endpoints(x,'hours')
  period.apply(x,ap,FUN,...)
}

# qobs_qsim von COSERO lesen und als getrennte Zeitreihen in einer Liste zurückgeben
#' read qobs_qsim.txt (a COSERO output file)
#' @param x character string pointing towards the file qobs_qsim.txt
#' @author Simon Frey
#' @return a list of length 2 with QOBS and QSIM values, respectively
#' @import xts
#' @export
#' @description Reads a qobs_qsim.txt file which is written by COSERO. Returns the recordings as list holding xts objects.
#' @details The file qobs_qsim.txt typically is located in the output folder of COSERO. However, by default, COSERO does NOT  write this file. To tell COSERO to write this, use OUTCONTROL = 1 or 2 in the COSERO defaults file.
#'
#'     For reading Q_output.txt, which is always be written by COSERO, see \code{\link{readCosero}}.
#' @examples
#' fpath <- system.file("extdata","qobs_qsim.txt", package = "TigeR")
#' out <- read.qobsqsim(fpath)
#' summary(out)
#' @seealso \code{\link{readCosero}}
read.qobsqsim <- function(x){
  output <- list()
  library(xts)
  out <- read.table(x,nrow=1,header=TRUE)
  nc <- ncol(out)
  out <- read.table(x,header=TRUE,colClasses=
                      c(rep("character",5),rep("numeric",nc-5)))
  datum <- as.POSIXct(paste(out[,1],out[,2],out[,3],out[,4],out[,5],sep=" "),
                      format="%Y %m %d %H %M",tz="utc")
  out <- xts(out[,6:nc],order.by=datum)

  output$QOBS <- out[,grep("Qobs",colnames(out))]
  output$QSIM <- out[,grep("Qsim",colnames(out))]

  return(output)
}
#' read a textfile and save it as xts-object
#' @param x character string. file to load
#' @param datacolums integer vector. Giving the columns with date informations
#' @param format character string. Specify the format of the date
#' @param header logical. Does the data have a header?
#' @param tz character string. Time zone of the data
#' @param ... additional arguments from other methods passed to read.table
#' @return an xts object.
#' @import xts
#' @export
#' @description Read a text file and directly save it as xts object.
#' @author Simon Frey
#' @examples
#' fpath <- system.file("extdata","qobs_qsim.txt", package = "TigeR")
#' out <- read.xts(x = fpath)
read.xts <- function(x, datecolums=c(1:5), format="%Y %m %d %H %M", header=TRUE, tz = "utc", ...){
  library(xts)
  temp <- read.table(x,nrow=1,header=header,...)
  nc <- ncol(temp)
  temp <- read.table(x,colClasses=c(rep("character",max(datecolums)),
                                    rep("numeric",nc-max(datecolums))),
                     header=header,...)
  for(k in datecolums){
    if(k == datecolums[1]){
      datum <- temp[,k]
    } else {
      datum <- paste(datum,temp[,k],sep=" ")
    }
  }
  datum <- as.POSIXct(datum, format=format,tz=tz)
  output <- xts(temp[,(max(datecolums)+1):nc], order.by=datum)
  return(output)
}


#' Open new graphical device
#' @param device character string. See details.
#' @param return.device logical. Should the device character string be returned
#' @param filename character string.
#' @param ... Additional arguments passed from other methods
#' @return If return.device = TRUE, device is retured. Else nothing is returned.
#' @description Open a new graphical device on screen or open a pdf/png file for plotting
#' @author Simon Frey
#' @export
#' @details Valid arguments for device are "dev", "png", "pdf" and "svg".
#'
#'     If device = "dev", the standard, then a new graphical deive is opened by calling dev.new()
#'
#'     If device = "png", "pdf" or "svg" the respective file specified by filename is opend for writing. Note that it is not automatically closed.
#'
#'     Filename doesn't need to end with an ending. It will be concatenated if missing.
#'
#'     Units of the devices is inches.
#'     A png will have the resolution of 300 dpi
#' @examples
#' dev.new.file()
#' dev.new.file(width=7, height=2)
#' dev.new.file(device = "png", return.device = TRUE, filename = tempfile)
#' # do not run
#' dev.new.file(device = "pdf")
#'
dev.new.file <- function(device="dev",return.device=TRUE,filename=NULL, ...){
  ##  Erstellen eines neues Plotfensters oder schreiben des Plot in eine Datei
  ##  Kann durch device angegeben werden:
  ##    - device = dev : öffenen eines neuen device (dev.new)
  ##    - device = png : öffenen einer neuen PNG Datei
  ##    - device = pdf : öffnen einer neuen pdf Datei
  ##
  ##    Simon Frey
  ##    September 2016 - Februar 2017
  ##############################################################################

  if(!device %in% c("dev","png","pdf","svg")){
    stop("ERROR: unknown file format")
  }

  if(device != "dev"){
    if(is.null(filename)){
       stop("Filename missing.")
    }
    if(substr(filename, nchar(filename)-3+1, nchar(filename)) != device){
      filename <- paste(filename,".",device,sep="")
    }
  }

  if(device == "dev"){
    dev.new(units="in",...)
  }
  if(device == "png"){
    png(units="in",res=300,filename, ...)
  }
  if(device == "pdf"){
    pdf(file = filename, ...)
  }
  if(device == "svg"){
    svg(filename, ...)
  }
  if(return.device){
    return(device)
  }
}

#' Find the max value of a (set of) column(s)
#' @param colData numerical matrix
#' @param na.rm logical. Should NA values be removed?
#' @return numerical vector
#' @author Simon Frey
#' @export
#' @examples
#' data("runoff")
#' colMax(runoff)
#' @seealso \code{\link{colMin}}
colMax <- function (colData,na.rm=TRUE) {
  apply(colData, MARGIN=c(2), max,na.rm=na.rm)
}

#' Find the min value of a (set of) column(s)
#' @param colData numerical matrix
#' @param na.rm logical. Should NA values be removed?
#' @return numerical vector
#' @author Simon Frey
#' @examples
#' data("runoff")
#' colMin(runoff)
#' @seealso \code{\link{colMax}}
colMin <- function (colData,na.rm=TRUE) {
  apply(colData, MARGIN=c(2), min,na.rm=na.rm)
}

#' Write an xts object using the date format as rownames
#' @param x xts object
#' @param ... Additonal arguments passed to write.table
#' @author Simon Frey
#' @export
#' @description Writing an xts object to a file using its date format as rownames instead of the numerical values of the date
#' @examples
#' ### do not run
#' data("runoff")
#' write.xts(runoff, file = tempfile())
write.xts <- function(x,...){
  if(!is.xts(x)){
    stop("x must be an xts object")
  }
  times <- as.character(time(x))
  xx <- as.matrix(x)
  rownames(xx) <- times
  write.table(xx,row.names=TRUE,...)
}

#' Define a mfrow object out of a number of plots
#'@param x number of plots to be drawn
#'@param maxrow maximum number of plots in one row
#'@return integer vector of length 2
#'@description returns an integer vector of length 2 giving the rows and columns
#'@author Simon Frey
#'@export
#'@examples
#' setcolumns(x = 6)
#' setcolumns(x = 6, maxrow = 2)
setcolumns <- function(x,maxrow = 4){

  nrcol <- ceiling(x/maxrow)
  nrrow <- ceiling(x/nrcol)

  return(c(nrrow,nrcol))

}

#' return a single letter from alphabet
#'@param x integer. Number of letter in the alphabet
#'@param capitals logical. Return uppercase letters?
#'@return character.
#'@author Simon Frey
#'@export
#'@examples
#' abc(1)
#' abc(x=7, capitals = TRUE)
#' abc(x=c(1:3))
abc <- function(x, capitals = FALSE){
  atoz <- c("a","b","c","d","e","f","g","h","i",
            "j","k","l","m","n","o","p","q","r",
            "s","t","u","v","w","x","y","z")
  if(capitals){
    return(toupper(atoz[x]))
  } else {
    return(atoz[x])
  }
}







