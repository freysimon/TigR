###############################################################
#                                                             #
#   Funktion um Rechenergebnisse von COSERO zu lesen          #
#   Gibt eine Liste mit eingelesenen Größen zurück.           #
#                                                             #
#                                                             #
#   Beschreibung der Parameter:                               #
#     qoutput:  Erwartet die Datei "Q_output.txt"             #
#     prec:     TRUE, falls der Niederschlag eingelesen       #
#               werden soll.                                  #
#     comp:     TRUE, falls die Abflusskomponenten            #
#               eingelesen werden sollen.                     #
#     eta:      TRUE, falls die Verdungstung eingelesen       #
#               werden soll.                                  #
#     storage:  TRUE, falls die Speicherstände                #
#               werden sollen.                                #
#     snowmelt: TRUE, falls die Schneeschmelze und der        #
#               Schneewasserwert eingelesen werden soll.      #
#                                                             #
#     area:     Fläche der Einzugsgebiete. Notwendig für die  #
#               Umrechnung des Abflusses von m³/s in mm       #
#               Falls nicht angegeben, wird der Abfluss nicht #
#               umgerechnet und eine Warnung wird ausgegben   #
#     timestep: Zeitschritt des COSERO Laufs. Notwendig für   #
#               die Umrechnung des Abflusses von m³/s in mm   #
#               Falls nicht angegeben wird versucht diesen    #
#               aus den Daten zu ermitteln.                   #
#                                                             #
#   ACHTUNG! Die Größen prec, comp, eta, storage und snowmelt #
#   werden von COSERO nur geschrieben, wenn im defaultsfile   #
#   der Wert von OUTCONTROL auf 1 oder 2 gesetzt ist.         #
#                                                             #
#   Autor:  Simon Frey                                        #
#   Datum:  Februar - April 2017                              #
#   Version: 1.2                                              #
#                                                             #
###############################################################

#' Reading COSERO output files
#' @param qoutput character string. Pointing towards the file "Q_output.txt"
#' @param prec logical or character string. Should precipitation data be read in? Maybe a path to the corresponding file.
#' @param comp logical or character string. Should data about the composition of runoff be read in? Maybe a path to the corresponding file.
#' @param storage logical or character string. Should storage data be read in? Maybe a path to the corresponding file.
#' @param snowmelt logical or character string. Should snow data be read in? Maybe a path to the corresponding file.
#' @param eta logical or character string. Should data about evapotranspiration be read in? Maybe a path to the corresponding file.
#' @param area numeric. Area of the subbasin(s).
#' @param timestep numeric. Timestep of the data.
#' @param treat.neg.as.na logical. Should negative observed runoff values be treated as NA? 
#' @param ... Additional arguments passed to read.table from other methods.
#' @author Simon Frey
#' @export
#' @import xts
#' @description Read COSERO outputfiles and store them into a list.
#' @details Reading the file Q_output.txt.
#'
#'    The function automatically searches for the file containing data about precipitation, runoff composition, eta, storage or snowmelt if the respective arguments are set TRUE.
#'    It is also possible to specify the path to the respective file directly. If the file cannot be found, it is skipped an a warning is returned. 
#'    Note that those file are NOT written by COSERO by default. Use OUTCONTROL = 1 or 2 in the defaults file of COSERO to activate writing of them. They are given in mm. 
#'    To compare them to runoff, be adviced that runoff should be transformed into mm, too. This, however, can only be done if area is given. 
#'    It is done via \code{\link{m3s2mm}}. The argument timestep may be provided. If NULL the function will try to gather it from the data.
#'
#'    For reading qobs_qsim.txt see \code{\link{read.qobsqsim}}
#' @examples
#' data(area)
#' fpath <- system.file("extdata","Q_output.txt", package = "TigR")
#' x <- readCosero(qoutput = fpath, area = area)
#' x <- readCosero(qoutput = fpath)
#' @seealso \code{\link{m3s2mm}}
#' @seealso \code{\link{read.qobsqsim}}
readCosero <- function(qoutput = "./Q_output.txt", prec=FALSE, comp=FALSE, eta=FALSE, storage = FALSE,
                       snowmelt=FALSE, area = NULL, timestep=NULL, treat.neg.as.na = TRUE, ...){


  if(is.null(area[1])) warning("No information about area provided. Runoff will not be given in mm!")
  
  # wd auf das Verzeichnis von Q_output.txt setzen
  setwd(dirname(qoutput))

  # functions.r laden
  library(xts)

  # Einlesen von qobqsim
  xx <- read.table(qoutput,header=TRUE,nrow=1,skip=22,...)
  xx <- read.table(qoutput,header=TRUE,skip=22,colClasses=c(rep("character",5),rep("numeric",ncol(xx)-5)),
                   stringsAsFactors = FALSE, ...)
  
  if(treat.neg.as.na){
    xx[xx < 0] <- NA
  }

  print("runoff read")
  
  # Checking if other results should be read in and if their respective filenames are given and the files exist
  checkfiles <- list(prec, comp, eta, storage, snowmelt)
  readfiles <- checkfiles
  for(k in 1:5){
    if(is.character(checkfiles[[k]])){
      if(file.exists(checkfiles[[k]])){
        checkfiles[[k]] = TRUE
      } else {
        warning(paste("Cannot find ", readfiles[[k]], ". Will be ignored", sep = ""))
        checkfiles[[k]] = FALSE
        readfiles[[k]] = FALSE
      }
    } else {
      readfiles[[k]] = FALSE
    }
  }
  
  prec <- checkfiles[[1]]
  comp <- checkfiles[[2]]
  eta <- checkfiles[[3]]
  storage <- checkfiles[[4]]
  snowmelt <- checkfiles[[5]]
  


  # Einlesen von Niederschlag
  if(prec){
    if(is.character(readfiles[[1]])){
      readfile <- readfiles[[1]]
    } else {
      readfile <- dir()[which(substrRight(dir(),4) == "prec")]
    }
    pr <- read.table(readfile,header=TRUE,nrow=1,...)
    pr <- read.table(readfile,header=TRUE,colClasses=c(rep("character",5),rep("numeric",ncol(pr)-5)),
                   stringsAsFactors = FALSE,...)

    print("Precipitation read")
  }

  # Einlesen von Speicherständen (enns.plus1)
  if(storage){
    if(is.character(readfiles[[4]])){
      readfile <- readfiles[[4]]
    } else {
      readfile <- dir()[which(substrRight(dir(),5) == "plus1")]
    }
    storages <- read.table(readfile, header=TRUE, nrow=1, skip = 1,...)
    storages <- read.table(readfile,skip=1,header=TRUE,colClasses=c(rep("character",5),rep("numeric",ncol(storages)-5)),
                       stringsAsFactors = FALSE,...)

    print("Storages read")
  }

  # Einlesen von enns.plus (Abflusskomponenten)
  if(comp){
    if(is.character(readfiles[[2]])){
      readfile <- readfiles[[2]]
    } else {
      readfile <- dir()[which(substrRight(dir(),4) == "plus")]
    }
    comps <- read.table(readfile,skip=1,header=TRUE,nrow=1,...)
    comps <- read.table(readfile,skip=1,header=TRUE,colClasses=c(rep("character",5),rep("numeric",ncol(comps)-5)),
                     stringsAsFactors = FALSE,...)

    print("Components read")
  }

  # Einlesen von ETA_PGEB.txt (Abflusskomponenten)
  if(eta){
    if(is.character(readfiles[[3]])){
      readfile <- readfiles[[3]]
    } else {
      readfile <- "ETA_PGEB.txt"
    }
    et <- read.table(readfile,header=TRUE,nrow=1,...)
    et <- read.table(readfile,header=TRUE,colClasses=c(rep("character",5),rep("numeric",ncol(et)-5)),
                   stringsAsFactors = FALSE,...)

    print("ETA read")
  }


  # Einlesen von varSnow.txt (Schneeschmelze etc), falls dies gewünscht ist
  if(snowmelt){
    if(is.character(readfiles[[5]])){
      readfile <- readfiles[[5]]
    } else {
      readfile <- NULL
      possible_files <- c("SNOW","snow","Snow")
      for(k in 1:3){
        temp <- dir(pattern = possible_files[k])
        if(length(temp) == 1){
          readfile <- temp
          break
        }
      }
      if(is.null(readfile)){
        stop("File with snow information could not be found.")
      }
    }
    snw <- read.table(readfile,header=TRUE,nrow=1,...)
    snw <- read.table(readfile,header=TRUE,
                      colClasses=c(rep("character",5),rep("numeric",ncol(snw)-5)),stringsAsFactors = FALSE, ...)

    print("Snow read")
  }

  datum <- as.POSIXct(paste(xx[,1],"-",xx[,2],"-",xx[,3]," ",xx[,4],":",xx[,5],sep=""),format="%Y-%m-%d %H:%M",tz="utc")
  xx <- xts(xx[,6:ncol(xx)],order.by=datum)
  xx <- clear.duplicated(xx)
  obs <- xx[,grep("QOBS",colnames(xx))]
  sim <- xx[,grep("QSIM",colnames(xx))]

  # Ermitteln der Zeitschritte
  time1 <- time(obs[1:(nrow(obs)-1),])
  time2 <- time(obs[2:nrow(obs),])

  # Majority Vote der einzelnen Zeitschritte
  if(is.null(timestep)){
    timestep <- as.numeric(
      names(
        which.max(
          table(
            difftime(time2,time1, units = "s")
          )
        )
      )
    )
  }


  if(prec){
    pr <- xts(pr[,6:ncol(pr)],order.by=datum)
    pr <- clear.duplicated(pr)
    rain <- pr[,grep("PRAINGEB",colnames(pr))]
    snow <- pr[,grep("PSNOWGEB",colnames(pr))]
  }

  if(comp){
     COMP <- xts(comps[,6:ncol(comps)],order.by=datum)
     COMP <- clear.duplicated(COMP)
     QAB123 <- COMP[,grep("QAB123GEB_",colnames(COMP))]
     QAB23 <- COMP[,grep("QAB23GEB_",colnames(COMP))]
     QAB3 <- COMP[,grep("QAB3GEB_",colnames(COMP))]
  }

  if(storage){
    storages <- xts(storages[,6:ncol(storages)],order.by=datum)
    storages <- clear.duplicated(storages)
    BW0 <- storages[,grep("BW0GEB_",colnames(storages))]
    BW1 <- storages[,grep("BW1GEB_",colnames(storages))]
    BW2 <- storages[,grep("BW2GEB_",colnames(storages))]
    BW3 <- storages[,grep("BW3GEB_",colnames(storages))]
    BW4 <- storages[,grep("BW4GEB_",colnames(storages))]
  }

  if(eta){
    et <- xts(et[,6:ncol(et)],order.by=datum)
    et <- clear.duplicated(et)
    ETA <- et[,grep("ETATGEB_",colnames(et))]
  }

  if(snowmelt){
    snw <- xts(snw[,6:ncol(snw)], order.by=datum)
    snw <- clear.duplicated(snw)
    SNWMLT <- snw[,grep("MELTGEB_",colnames(snw))]
    SNWACC <- snw[,grep("SWWGEB_",colnames(snw))]
  }

  if(!is.null(area[1])){
    obsmm <- obs
    simmm <- sim
    for(j in 1:ncol(obsmm)){
      obsmm[,j] <- m3s2mm(obs[,j],area=cumsum(area[j]),dt=timestep)
      obsmm[obsmm <0] <- -0.01
      simmm[,j] <- m3s2mm(sim[,j],area=cumsum(area[j]),dt=timestep)
      simmm[simmm <0] <- -0.01
      if(snowmelt){
        SNWMLT[,j] <- mm2m3s(SNWMLT[,j],area=area[j],dt=timestep)
      }
    }
  } else {
    obsmm <- NULL
    simmm <- NULL
  }


  output <- list()
  output$runoff <- list("obs"=obs,"obs.mm"=obsmm,"sim"=sim,"sim.mm"=simmm)

  if(prec){
    output$precipitation <- list("rain" = rain, "snow" = snow)
  }
  if(storage){
    output$storages <- list("BW0" = BW0, "BW1" = BW1,"BW2" = BW2,"BW3" = BW3,"BW4" = BW4)
  }
  if(comp){
    output$components <- list("HOF" = QAB123-QAB23-QAB3, "SOF" = QAB23-QAB3, "GWF" = QAB3)
  }
  if(eta){
    output$ETA <- list("ETA" = ETA)
  }
  if(snowmelt){
    output$snowmelt <- list("snowmelt" = SNWMLT, "snowaccumulation" = SNWACC)
  }

  return(output)

}
