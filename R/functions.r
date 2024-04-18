#################################################
#                                               #
# Sammlung mehr oder minder nützlicher          #
# Funktionen um R zu erweitern.                 #
#                                               #
# Autor: Simon Frey                             #
#                                               #
#################################################


#' Find missing time steps in an univariate time series
#' @param x an xts object
#' @return a numerical vector giving the differences of the time steps of the xts object
#' @export
#' @author Simon Frey
#' @import xts
#' @return a numeric vector
#' @description Irregularities are found by \code{\link{diff.POSIXt}}. A majority vote using \code{\link{which.max}} from the package \code{table} determines the most common time step. 
#' Any differing timesteps are interpreted as missing. A vector containing the locations of those missing time steps is returned.
#' Note that only the location but not how many time steps per gap are missing is returned.
#' @seealso \code{\link{fill.missing}}
missing.steps <- function(x){
  # check if x is an xts object
  if(class(x)[1] != "xts"){
    stop("x must be of xts type")
  }
  x <- diff.POSIXt(index(x))
  
  # find majority of indices
  m <- which.max(table(x))
  # check, which index does not match m
  x.not.m <- which(x != as.numeric(m))
  
  return(as.numeric(x.not.m))
}

#' Fill missing values in an xts object
#' @param x an xts object
#' @param start NULL or an POSIXct object. If NULL, the first index of x will be used
#' @param end NULL or an POSIXct object. If NULL, the last index of x will be used
#' @param steps increment of the sequence. See ‘Details’
#' @param fill logical. If TRUE missing entries are filled using \code{\link{nalocf}}, else missing entries are filled with na.value
#' @param na.value numeric or NA. Used for filling missing entries if fill == FALSE
#' @description Fill missing entries in an xts object using \code{\link{nalocf}} or, alternatively, with an constant value
#' @export
#' @import xts
#' @author Simon Frey
#' @seealso \code{\link{missing.steps}}
#' @details steps can be a character string, containing one of "sec", "min", "hour", "day", "DSTday", "week", "month", "quarter" or "year". Default is "hour"
#' @return an xts object
#' @examples 
#' data("runoff")
#' # delete a row to make the time series irregulat
#' runoff.missing <- runoff[-4,]
#' # fill missing timesteps
#' runoff.filled <- fill.missing(runoff.missing)
#' ### fill with -0.01
#' runoff.filled <- fill.missing(runoff.missing, fill = FALSE, na.value = -0.01)
fill.missing <- function(x, start=NULL, end=NULL, steps = "hour", fill = TRUE, na.value = NA){
  library(xts)
  # check if x is an xts object
  if(class(x)[1] != "xts"){
    stop("x must be of xts type")
  }
  if(is.null(start)){
    start <- index(x)[1]
  }
  if(is.null(end)){
    end <- index(x)[nrow(x)]
  }
  
  SEQ <- seq.POSIXt(from=start,to=end,by=steps)
  SEQ <- xts(rep(NA,length(SEQ)), order.by=SEQ)
  
  X <- merge(SEQ,x)
  X <- X[,-1]
  nas <- sum(is.na(X[,1]))
  if(fill){
    X <- nalocf(X)
    print(paste(nas, "entries have been filled using nalocf"))
  } else {
    x.missing <- which(is.na(X))
    xmat <- as.matrix(X)
    xmat[x.missing] <- na.value
    X <- xts(xmat, order.by=index(X))
    print(paste(length(x.missing), "missing entries have been filled by", na.value))
  }
  
  return(X)
}

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


#' Add a slash to an existing character string
#' @param x character string
#' @return character string
#' @description Adds a slash (/) to x, if x does not already end with one.
#' @author Simon Frey
#' @export
#' @examples
#' addSlash("C:/TEMP")
#' addSlash("C:/TEMP/")
#' @seealso \code{\link{removeSlash}} for removing a trailing slash; \code{\link{changeSlash}} for substituting a slah with a double backslash
addSlash <- function(x){
  if(TigR::substrRight(x,1) != "/"){
    x <- paste(x,"/",sep="")
  }
  return(x)
}

#' Remove a trailing slash from a character string
#' @param x character string
#' @return character string without a trailing slash
#' @description Removes a trailing slash (/) from x
#' @author Simon Frey
#' @export
#' @examples
#' removeSlash("C:/Temp/")
#' removeSlash("C:/Temp")
#' @seealso \code{\link{addSlash}} for adding a trailing slash; \code{\link{changeSlash}} for substituting a slah with a double backslash
removeSlash <- function(x){
  if(TigR::substrRight(x,1) == "/"){
    x <- substr(x,1,nchar(x)-1)
  }
  return(x)
}

#' Substitute slashes by double backslashes
#' @param x character string
#' @return charater string with double backslashes instead of slashes
#' @description Substitute slashes by double backslashes in a character string.
#' @author Simon Frey
#' @export
#' @examples 
#' changeSlash("C:/Temp")
#' changeSlash("C:/firstlevel/secondlevel")
#' @seealso See \code{\link{addSlash}} for adding a trailing slash and \code{\link{removeSlash}} for removing a trailing slash from a character string.
changeSlash <- function(x){
  return(gsub("/","\\",x,fixed = TRUE))
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


# Suche nach doppelten Zeitflags. Falls welche gefunden werden, wird der Wert beider Einträge gemittelt.
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
#' @param roundtime character. Valid are \code{NA}, "round", and "trunc", See details
#' @param na.rm logical. Should ne values be removed?
#' @author Simon Frey
#' @export
#' @import xts
#' @description Aggregate an xts object to hourly values. It is a wrapper of \code{\link{period.apply}} with endpoints = "hours"
#' @details An xts object is aggregated to hourly values. Using the parameter roundtime, the timestamp of the xts object can be 
#'     round to full hours. This can be done by rounding to the nearest full hour, or by going to the last full hour (trunc). NA skips rounding.
#' @examples 
#'     library(TigR)
#'     library(xts)
#'     x <- xts(runif(6,1,100), order.by = as.POSIXct(c("2000-01-20 10:55","2000-01-20 10:59","2000-01-20 11:20","2000-01-20 12:35", "2000-01-20 12:45", "2000-01-20 13:45")))
#'     apply.hourly(x, FUN = mean, roundtime = "round")
#' @seealso \code{\link{period.apply}}
apply.hourly <- function(x, FUN, roundtime = "round", na.rm = TRUE){
  if(!is.xts(x)){
    stop("x must be an xts object")
  }
  
  if(!is.na(roundtime)){
    if(roundtime == "round"){
      time(x) <- round.POSIXt(terra::time(x), "hours")
    } else if(roundtime == "trunc"){
      terra::time(x) <- terra::trunc.POSIXt(time(x), "hours")
    } else {
      stop("roundtime must be either round or trunc")
    }
  }
  
  ap <- endpoints(x,'hours')
  if(na.rm){
    period.apply(x,ap,FUN, na.rm = TRUE)
  } else {
    period.apply(x,ap,FUN)
  }
  
}

# qobs_qsim von COSERO lesen und als getrennte Zeitreihen in einer Liste zurückgeben
#' read qobs_qsim.txt (a COSERO output file)
#' @param x character string pointing towards the file qobs_qsim.txt
#' @param ... other arguments passed to \code{\link{read.table}}. Already defined are \code{header (TRUE)} and \code{colClasses}
#' @author Simon Frey
#' @return a list of length 2 with QOBS and QSIM values, respectively
#' @import xts
#' @export
#' @description Reads a qobs_qsim.txt file which is written by COSERO. Returns the recordings as list holding xts objects.
#' @details The file qobs_qsim.txt typically is located in the output folder of COSERO. However, by default, COSERO does NOT  write this file. To tell COSERO to write this, use OUTCONTROL = 1 or 2 in the COSERO defaults file.
#'
#'     For reading Q_output.txt, which is always be written by COSERO, see \code{\link{readCosero}}.
#' @examples
#' fpath <- system.file("extdata","qobs_qsim.txt", package = "TigR")
#' out <- read.qobsqsim(fpath)
#' summary(out)
#' @seealso \code{\link{readCosero}}
read.qobsqsim <- function(x, ...){
  output <- list()
  library(xts)
  out <- read.table(x,nrow=1,header=TRUE, ...)
  nc <- ncol(out)
  out <- read.table(x,header=TRUE,colClasses=
                      c(rep("character",5),rep("numeric",nc-5)), ...)
  datum <- as.POSIXct(paste(out[,1],out[,2],out[,3],out[,4],out[,5],sep=" "),
                      format="%Y %m %d %H %M",tz="utc")
  out <- xts(out[,6:nc],order.by=datum)

  output$QOBS <- out[,grep("Qobs",colnames(out))]
  output$QSIM <- out[,grep("Qsim",colnames(out))]

  return(output)
}
#' read a textfile and save it as xts-object
#' @param x character string. file to load
#' @param datecolumns integer vector. Giving the columns with date informations
#' @param format character string. Specify the format of the date
#' @param header logical. Does the data have a header?
#' @param tz character string. Time zone of the data
#' @param skip integer: the number of lines of the data file to skip before beginning to read data.
#' @param cut.prefix integer. Amount of leading characters that will be ignored in formatting the POSIXct object. Experimental!
#' @param ... additional arguments from other methods passed to \code{\link{fread}}
#' @return an xts object.
#' @import xts
#' @import data.table
#' @export
#' @description Read a text file using \code{\link{fread}} and directly save it as \code{\link{xts}} object.
#' @author Simon Frey
#' @seealso \code{\link{write.xts}}
#' @details Other than in older versions, where the standard arguments of this function were designed to read COSERO input and output files, this function by default reads tab-separated files using the time format Y-m-d H:M.
#' @examples
#' fpath <- system.file("extdata","qobs_qsim.txt", package = "TigR")
#' out <- read.xts(x = fpath)
read.xts <- function(x, datecolumns=1, format="%Y-%m-%d %H:%M", header=TRUE, tz = "utc", skip = 0, cut.prefix = 0, ...){
  library(xts)
  library(data.table)
  
  # disable tz warning
  options(xts_check_TZ = FALSE)
  
  # new method
  temp <- data.table::fread(file = x, header = TRUE, skip = skip, ...)
  temp <- as.data.frame(temp)
  nc <- ncol(temp)

  for(k in datecolumns){
    if(k == datecolumns[1]){
      datum <- temp[,k]
    } else {
      datum <- paste(datum,temp[,k],sep=" ")
    }
  }
  
  if(cut.prefix > 0){  # only substring the date character if necessary
    datum <- substring(datum, cut.prefix+1)
  }
  
  datum <- as.POSIXct(datum, format=format,tz=tz)
  output <- xts(temp[,(max(datecolumns)+1):nc], order.by=datum)
  
  
  return(output)
}


dev.new.file <- function(device="dev",return.device=TRUE,filename=NULL, ...){
  #' Open a new device for plotting
  #' @description Open a new device for plotting and allow some settings
  #' @author Simon Frey
  #' @export
  #' @param device any string of "dev","png","pdf", or "svg"
  #' @param return.device logical. should the type of device be returned?
  #' @param filename string. Where should the plot be saved? If no extention is given, it will be added according to the type of the device.
  #' @param ... additional arguments passed on to the dev fuction (e.g. \link{dev.new}, or \link{png}).
  #' @examples 
  #'     d <- dev.new.file(device = "png",filename = "C:/TEMP/test")
  #'     plot(1)
  #'     if(d %in% c("png","pdf","svg")) dev.off()
  #' 

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
#' @param x numerical matrix
#' @param na.rm logical. Should NA values be removed?
#' @return numerical vector
#' @author Simon Frey
#' @export
#' @examples
#'     data("runoff")
#'     colMax(runoff)
#' @seealso \code{\link{colMin}}
#' @seealso \code{\link{rowMin}}
#' @seealso \code{\link{rowMax}}
#' @seealso \code{\link{rowMedian}}
colMax <- function (x,na.rm=TRUE) {
  apply(x, MARGIN=c(2), max,na.rm=na.rm)
}

#' Find the min value of a (set of) column(s)
#' @param x numerical matrix
#' @param na.rm logical. Should NA values be removed?
#' @return numerical vector
#' @author Simon Frey
#' @export
#' @examples
#'     data("runoff")
#'     colMin(runoff)
#' @seealso \code{\link{colMax}}
#' @seealso \code{\link{rowMin}}
#' @seealso \code{\link{rowMax}}
#' @seealso \code{\link{rowMedian}}
colMin <- function (x,na.rm=TRUE) {
  apply(x, MARGIN=c(2), min,na.rm=na.rm)
}

#' Find the max value of a (set of) row(s)
#' @param x numerical matrix
#' @param na.rm logical. Should NA values be removed?
#' @return numerical vector
#' @author Simon Frey
#' @export
#' @examples
#'     data("runoff")
#'     rowMax(runoff)
#' @seealso \code{\link{colMin}}
#' @seealso \code{\link{rowMin}}
#' @seealso \code{\link{colMax}}
#' @seealso \code{\link{rowMedian}}
rowMax <- function (x,na.rm=TRUE) {
  apply(x, MARGIN=c(1), max,na.rm=na.rm)
}

#' Find the min value of a (set of) row(s)
#' @param x numerical matrix
#' @param na.rm logical. Should NA values be removed?
#' @return numerical vector
#' @author Simon Frey
#' @export
#' @examples
#'     data("runoff")
#'     rowMin(runoff)
#' @seealso \code{\link{colMin}}
#' @seealso \code{\link{colMax}}
#' @seealso \code{\link{rowMax}}
#' @seealso \code{\link{rowMedian}}
rowMin <- function (x,na.rm=TRUE) {
  apply(x, MARGIN=c(1), min,na.rm=na.rm)
}

#' Calculate the median of a (set of) row(s)
#' @param x numerical matrix
#' @param na.rm logical. Should NA values be removed?
#' @return numerical vector
#' @author Simon Frey
#' @export
#' @examples
#'     data("runoff")
#'     rowMedian(runoff)
#' @seealso \code{\link{colMin}}
#' @seealso \code{\link{rowMin}}
#' @seealso \code{\link{rowMax}}
#' @seealso \code{\link{colMax}}
rowMedian <- function (x,na.rm=TRUE) {
  apply(x, MARGIN=c(1), median,na.rm=na.rm)
}

#' Write an xts object using the date format as rownames
#' @param x xts object
#' @param file either a character string naming a file or a \code{\link{connection}} open for writing. "" indicates output to the console.
#' @param format format-style argument for formatting the date/time
#' @param FMT character string passed on to \code{\link{sprintf}} for formatting numerical values. If NULL, no special format is used.
#' @param quote logical. Should characterers be encapsulated in ""?
#' @param ... Additonal arguments passed to \code{\link{write.table}}
#' @author Simon Frey
#' @import xts
#' @export
#' @seealso \code{\link{read.xts}}
#' @description Writing an xts object to a file using its date format as rownames instead of the numerical values of the date
#' @examples
#'     ### do not run
#'     data("runoff")
#'     write.xts(runoff, file = tempfile())
#'     write.xts(runoff, format = "%d.%m.%Y %H:%M")
#'     write.xts(runoff, fmt = "%5.1f")
write.xts <- function(x, file = "", format = NULL, fmt = NULL, quote = FALSE, ...){
  if(!is.xts(x)){
    stop("x must be an xts object")
  }
  
  if(is.null(format)){
    times <- as.character(time(x))
  } else {
    times <- as.character(format(time(x), format = format))
  }
  
  xx <- as.matrix(x)
  
  if(!is.null(fmt)){
    if(!is.character(fmt)) stop("fmt must be a character string")
    if(substr(fmt, 1, 1) != "%") stop("fmt must begin with a % sign. See ?sprintf for details")
    xx <- apply(xx, 2, FUN = function(x) sprintf(fmt = fmt, x))
  }
  
  rownames(xx) <- times
  write.table(xx, file = file, row.names=TRUE, quote = quote, ...)
}

#' Define a mfrow object out of a number of plots
#'@param x number of plots to be drawn
#'@param maxrow maximum number of plots in one row
#'@return integer vector of length 2
#'@description returns an integer vector of length 2 giving the rows and columns
#'@author Simon Frey
#'@export
#'@examples
#'     setcolumns(x = 6)
#'     setcolumns(x = 6, maxrow = 2)
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
#'     abc(1)
#'     abc(x=7, capitals = TRUE)
#'     abc(x=c(1:3))
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


#' unpack a set of files
#' @param x vector of filenames
#' @param keeptar Should the tar.gz archives be extracted or extracted and untared?
#' @param exdir character string. The directory to extract files to.
#' @param overwrite logical. Should any existing files be overwritten?
#' @param remove logical. Should the archive be deleted after extraction? Also deletes skipped archives.
#' @param cores integer. Number of cores used to extract the files. Defaults to NULL which is a single core.
#' @param barstyle character. Style of the progressbar drawn by \code{\link{pbapply}}. Possible values are "timer", "txt", "win", "tk", or "none".  
#' @param debug logical. Should informations about debugging be printed?
#' @param ... arguments passed from other method
#' @return The default of unzip/untar/gunzip will be returned
#' @author Simon Frey
#' @seealso \code{\link{unzip}}, \code{\link{untar}}, \code{\link{gunzip}}
#' @description This function can handle tar.gz and zip files. One can decide if tar.gz files should be just extracted (use keeptar = TRUE) or extracted and untared (use keeptar = FALSE). 
#' @import R.utils
#' @import parallel
#' @import pbapply 
#' @export
unpack <- function(x, keeptar = TRUE, exdir = ".", overwrite = TRUE,  
                       remove = FALSE, cores = NULL, barstyle = "win", debug = FALSE, ...){
  library(pbapply)
  library(parallel)
  if(keeptar) library(R.utils)
  extention <- unlist(strsplit(x, "[.]"))
  if (tail(extention, n = 1) == "gz") {
    if (tail(extention, n = 2)[1] != "tar") {
      stop("Filetype not recognized")
    }
    
    if(debug) print(paste("x is: ", x, sep =""))
    
    
    # create cluster for multithreading
    if(!is.null(cores)){
      if(!is.integer(cores)){
        warning("cores must be an integer representing the number of cores you want to use. Now we're using all but one cores.")
        cores <- detectCores()-1
      }
      if(debug) print(paste("Creating cluster using ", cores, " cores", sep=""))
      cl <- makeCluster(cores)
      clusterExport(cl = cl, varlist=c("x","remove","overwrite"), envir = environment())
      #clusterExport(cl, "exdir")
    } else {
      cl = NULL
    }
    
    pboptions(type = barstyle, label = "Extracting files ...")
    
    # ... then extract them
    if(keeptar){
      pbsapply(x, FUN = gunzip, remove = remove,  overwrite = overwrite, ..., cl = cl)
    } else {
      unTar <- function(x, remove, overwrite, ...){
        if(overwrite){
          files <- untar(x, list = TRUE)
          untar(x, files[which(!file.exists(files))], ...)
        } else {
          untar(x)
        }
        if(remove) file.remove(x)
      }
      if(!is.null(cores)){
        clusterExport(cl, "unTar", envir = environment())
      }
      
      pbsapply(x, FUN = unTar, remove = remove, overwrite = overwrite, ..., cl = cl)
    }
  } else if (tail(extention, n = 1) == "zip"){
    unZip <- function(x, remove, overwrite, ...){
      unzip(x, overwrite = overwrite, ...)
      if(remove){
        file.remove(x)
      }
    }
    if(!is.null(cores)){
      clusterExport(cl, "unZip", envir = environment())
    }
    
    pbsapply(x, FUN = unZip, remove = remove, overwrite = overwrite, ..., cl = cl)
  }
  if(!is.null(cores)){
    stopCluster(cl)
  }
}

#' Loading many libraries at once
#' @param x character vector. Name(s) of the libraries that are loaded/installed.
#' @param ... Arguments passed to \code{\link{require}} and \code{\link{install.packages}}
#' @author Simon Frey
#' @description This function tries to load more than one package at once. If any of these packages is not installed it tries to istall them and load them afterward.
#' @export
#' @examples
#'     # loading xts 
#'     libraries("xts")
#'     libraries(c("xts","shiny"))
#' @return Returns nothing but gives a warning if it cannot load/install a library/package
#' @seealso \code{\link{require}}, \code{\link{library}}, \code{\link{install.packages}} 
libraries <- function(x, ...){
  
  temp <- suppressWarnings(unlist(lapply(x,require,character.only=TRUE, ...)))
  
  if(any(!temp)){
    w <- which(!temp)
    install.packages(x[w],...)
    
    
    temp <- suppressWarnings(unlist(lapply(x[w],require,character.only=TRUE, ...)))
    if(!any(temp)){
      w <- which(!temp)
      stop(paste("Error loading ",x[w],sep=""))
    }
  }
}


#' Faster version of last observation carried forward
#' @param x an xts object
#' @param loop logical. Should the operation be performed as a loop (slower but maybe better for RAM management)
#' @param ... further arguments, e.g. na.rm; see \code{\link{na.locf}}
#' @author Simon Frey
#' @description This is basically a wrapper around \code{\link{na.locf}} from the xts package. It avoids the column-by-column copying. Usefull if there are many columns in the xts object.
#' @export
#' @import xts
#' @examples 
#'     data(runoff)
#'     # add some NAs
#'     runoff[2,] <- NA
#'     index(runoff)[2] <- NA
#' 
#'     a <- nalocf(runoff)
#'     b <- na.locf(runoff)
#'     identical(a,b)
#' @return Returns an xts object
#' 
nalocf <- function(x, loop = FALSE, ...){
  library(xts)
  if(class(x)[1] != "xts"){
    stop("x must be an xts object")
  }
  TI <- index(x)
  if(loop){
    temp <- matrix(ncol = ncol(x), nrow = nrow(x), data = NA)
    for(k in 1:ncol(x)){
      if(any(is.na(x[,k]))){
        temp[,k] <- na.locf(x[,k])
      } else {
        temp[,k] <- x[,k]
      }
    }
    x <- temp
    rm(temp)
    gc()
  } else {
    x <- apply(x, 2, na.locf) 
  }
  
  x <- as.xts(x, order.by = TI)
  return(x)
}

#' Normalize values between 0 and 1
#' @author Simon Frey
#' @param x numeric (vector, matrix or data.frame) that will be scaled
#' @param ... further arguments passed to the functions min and max e.g. na.rm
#' @description Normalize values of a vector, matrix or data.frame between 0 and 1
#' @return depending on x a normalized vector, matrix or data.frame is returned
#' @export
#' @examples 
#'     ex <- c(3,5,10,2,9,20)
#'     scalevalues(ex)
scalevalues <- function(x, ...){
  if(!class(x)[1] %in% c("numeric", "matrix", "data.frame")){
    stop("x must be a numeric vector, matrix or data.frame")
  }
  
  extr <- c(min(x, ...), max(x, ...))
  x <- (x-extr[1])/(extr[2]-extr[1])
  return(x)
}


#' Return the first or last part of a list
#'
#' Returns the first or last part of a list. Instead of returning the first
#' n entries as the standard head() does, it attempts to call head()
#' recursively on the entries in the list. If it fails, it will return the
#' particular entry (standard behavior).
#' @param obj a list object
#' @param n a single integer. If positive, prints the first n items for the
#' list and all entries in the list. If negative, prints all but the last
#' n items in the list.
#' @param ... additional parameters passed on to head()
#' @author pimentel
#' @references \code{https://gist.github.com/pimentel/256fc8c9b5191da63819}
#' @export
#' @examples 
#'     a <- list()
#'     a[[1]] <- c(1:100)
#'     a[[2]] <- c(500:1000)
#'     head.list(a)
#'     # same result
#'     head(a)
#' @return a list of length n, with items in the list of length n
head.list <- function(obj, n = 6L, ...)
{
  stopifnot(length(n) == 1L)
  origN <- n
  n <- if (n < 0L)
    max(length(obj) + n, 0L)
  else min(n, length(obj))
  lapply(obj[seq_len(n)], function(x)
  {
    tryCatch({
      head(x, origN, ...)
    }, error = function(e) {
      x
    })
  })
}





