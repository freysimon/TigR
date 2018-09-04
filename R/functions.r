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
#' @param roundtime character. Valid are "NA", "round", and "trunc", See details
#' @param na.rm loggical. Should ne values be removed?
#' @author Simon Frey
#' @export
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
  
  if(roundtime != "NA"){
    if(roundtime == "round"){
      time(x) <- round.POSIXt(time(x), "hours")
    } else if(roundtime == "trunc"){
      time(x) <- trunc.POSIXt(time(x), "hours")
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
#' @param datecolumns integer vector. Giving the columns with date informations
#' @param format character string. Specify the format of the date
#' @param header logical. Does the data have a header?
#' @param tz character string. Time zone of the data
#' @param skip integer: the number of lines of the data file to skip before beginning to read data
#' @param ... additional arguments from other methods passed to read.table
#' @return an xts object.
#' @import xts
#' @export
#' @description Read a text file and directly save it as xts object.
#' @author Simon Frey
#' @examples
#' fpath <- system.file("extdata","qobs_qsim.txt", package = "TigR")
#' out <- read.xts(x = fpath)
read.xts <- function(x, datecolumns=c(1:5), format="%Y %m %d %H %M", header=TRUE, tz = "utc", skip = 0, ...){
  library(xts)
  temp <- read.table(x,nrow = 1,header=header, skip = skip, ...)
  nc <- ncol(temp)
  temp <- read.table(x,colClasses=c(rep("character",max(datecolumns)),
                                    rep("numeric",nc-max(datecolumns))),
                     header=header, skip = skip, ...)
  for(k in datecolumns){
    if(k == datecolumns[1]){
      datum <- temp[,k]
    } else {
      datum <- paste(datum,temp[,k],sep=" ")
    }
  }
  datum <- as.POSIXct(datum, format=format,tz=tz)
  output <- xts(temp[,(max(datecolumns)+1):nc], order.by=datum)
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
#' @export
#' @examples
#' data("runoff")
#' colMin(runoff)
#' @seealso \code{\link{colMax}}
colMin <- function (colData,na.rm=TRUE) {
  apply(colData, MARGIN=c(2), min,na.rm=na.rm)
}

#' Write an xts object using the date format as rownames
#' @param x xts object
#' @param file either a character string naming a file or a \code{\link{connection}} open for writing. "" indicates output to the console.
#' @param format format-style argument for formatting the date/time
#' @param FMT character string passed on to \code{\link{sprintf}} for formatting numerical values. If NULL, no special format is used.
#' @param quote logical. Should characterers be encapsulated in ""?
#' @param ... Additonal arguments passed to write.table
#' @author Simon Frey
#' @export
#' @description Writing an xts object to a file using its date format as rownames instead of the numerical values of the date
#' @examples
#' ### do not run
#' data("runoff")
#' write.xts(runoff, file = tempfile())
#' write.xts(runoff, format = "%d.%m.%Y %H:%M")
#' write.xts(runoff, fmt = "%5.1f")
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

# #' unpack an archive (zip or tar)
# #' @param x character sting. file to be unpacked
# #' @param keeptar logical. If TRUE only decompresses the .tar.gz archive, otherwise fully unpack it?
# #' @param exdir character string. The directory to extract files to.
# #' @param copyfirst logical. Should the tar.gz file first be copied to the exdir directory?
# #' @param overwrite logical. Should any existing files be overwritten?
# #' @param skip logical. Skip existing files if overwrite == FALSE.
# #' @param remove logical. Should the archive be deleted after extraction? Also deletes skipped archives.
# #' @param ... arguments passed from other method
# #' @return The default of unzip/untar/gunzip will be returned
# #' @description This funtcion tries to unpack a tar.gz or zip file
# #' @details If keeptar == TRUE, the .tar.gz archive is only beeing decompressed using the \code{\link{gunzip}}-function from the R.utils-package (to the .tar-file). Note that remove is set to FALSE, unlike the default of gunzip.
# #'
# #'    If keeptar == FALSE, the archive is fully unpacked using the \code{\link{untar}} function.
# #'
# #'    If copyfirst == TRUE (the default), the tar.gz file is copied to the exdir directory befor it is unpacked. Might be faster when unpacking from a slow source, e.g. an external harddrive. Only affects extraction of tar.gz files if kepptar == TRUE.
# #' @author Simon Frey
# #' @import R.utils
# #' @export
# #' @seealso \code{\link{unzip}}, \code{\link{untar}}, \code{\link{gunzip}}
# unpack <- function (x, keeptar = TRUE, exdir = ".", copyfirst = TRUE, overwrite = FALSE, skip = TRUE, remove = FALSE, ...){
#   extention <- unlist(strsplit(x, "[.]"))
#   if (tail(extention, n = 1) == "gz") {
#     if (tail(extention, n = 2)[1] != "tar") {
#       warning("Filetype not recognized")
#     } else {
#       if(copyfirst){
#         if(exdir != "."){
#           copy <- paste(TigR::addSlash(exdir), tail(unlist(strsplit(x, "/")), 1), sep = "")
#           file.copy(from = x, to = copy)
#           x <- copy
#         }
#       }
#       if (keeptar) {
#         library(R.utils)
#         tarfile <- gunzip(as.character(x), remove = remove, overwrite = overwrite, skip = skip, ...)
#         if (exdir != ".") {
#           if(!copyfirst){
#             file.copy(from = tarfile, to = paste(TigR::addSlash(exdir),
#                                                tail(unlist(strsplit(tarfile, "/")), 1),
#                                                sep = ""))
#             file.remove(tarfile)
#           }
#         }
#       } else {
#         if(!overwrite){
#           # list all files
#           tarlist <- untar(x, list = TRUE, exdir = exdir, ...)
#           
#           #extract only non extisting files
#           untar(x, exdir = exdir, files = tarlist[which(!file.exists(tarlist))], ...)
#           
#         } else {
#           untar(x, exdir = exdir, ...)
#         }
#       }
#     }
#   }
#   else if (tail(extention, n = 1) == "zip") {
#     if(copyfirst){
#       if(exdir != "."){
#         copy <- paste(TigR::addSlash(exdir), tail(unlist(strsplit(x, "/")), 1), sep = "")
#         file.copy(from = x, to = copy)
#         x <- copy
#       }
#     }
#     unzip(x, exdir = exdir, overwrite = overwrite, ...)
#   }
#   else {
#     warning("Filetype not recognized")
#   }
#   # delete any existing archive
#   if(remove){
#     if(file.exists(x)){
#       file.remove(x)
#     }
#   }
# }

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
    
    # first copy files ...
    # copyto <- gsub(pattern = dirname(x)[1], exdir, x)
    # file.copy(from = x, to = copyto,  overwrite = overwrite)
    # x <- copyto
    # 
    # if(debug) print(paste("copying successfully done! New x is: ",x, sep =""))
    
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
#' # loading xts 
#' libraries("xts")
#' libraries(c("xts","shiny"))
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
#' @examples 
#' data(runoff)
#' # add some NAs
#' runoff[2,] <- NA
#' index(runoff)[2] <- NA
#' 
#' a <- nalocf(runoff)
#' b <- na.locf(runoff)
#' identical(a,b)
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
#' @param ... further arguements passed to the functtions min and max e.g. na.rm
#' @description Normalize values of a vector, matrix or data.frame between 0 and 1
#' @return depending on x a normalized vector, matrix or dataframe is returned
#' @export
#' @examples 
#'     ex <- c(3,5,10,2,9,20)
#'     scalevalues(ex)
scalevalues <- function(x, ...){
  if(!class(x) %in% c("numeric", "matrix", "data.frame")){
    stop("x must be a numeric vector, matrix or data.frame")
  }
  
  extr <- c(min(x, ...), max(x, ...))
  x <- (x-extr[1])/(extr[2]-extr[1])
  return(x)
}






