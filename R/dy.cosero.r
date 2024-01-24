
#' Plot results regarding runoff from COSERO using dygraphs
#' @author Simon Frey
#' @export
#' @import dygraphs
#' @import htmltools
#' @import hydroGOF
#' @description Plot the results regarding runoff from COSERO using dygraphs. The output may be grouped together with other results.
#' @param qoutput Either a path to the file to be read or the result from \link{readCosero}
#' @param read.data logical. If TRUE, qoutput will be read in using \link{readCosero}
#' @param prec logical or character string. Should precipitation data be read in? Maybe a path to the corresponding file.
#' @param wb logical. If TRUE cumsums of waterbalance components are calculated and plotted. Note that if wb == TRUE, area must be given!
#' @param comp logical or character string. Should data about the composition of runoff be read in? Maybe a path to the corresponding file.
#' @param storage logical or character string. Should storage data be read in? Maybe a path to the corresponding file.
#' @param snowmelt logical or character string. Should snow data be read in? Maybe a path to the corresponding file.
#' @param eta logical or character string. Should data about evapotranspiration be read in? Maybe a path to the corresponding file.
#' @param group character string or NULL. Used to group dygraph plots. See \link{dygraph}
#' @param area numeric. Vector with the length of subbasins modelled in qoutput. The area of each subbasin in square meters (m^2).
#' @param height numeric, "auto", or NULL. Height in pixels (optional, defaults to automatic sizing). If "auto" a total plot of 1200px is assumed and devided by the amount of plots.
#' @param NB numeric. Number of subbasin that will be analysed. Note that if NB is a vector, only the first element will be used.
#' @param ... additional arguments passed on either to \link{readCosero} or to \link{dygraph}
#' @return Nothing is returned to R. Only the dygraphs are plotted.
#' @details If read,data == FALSE the results from \code{\link{readCosero}} must be passed on to \code{dy.cosero}.
#'     In this case the parameters \code{prec},\code{comp},\code{storage},\code{snowmelt}, and \code{eta} may be logical
#'     
#'     Note that at this time, this function only works for basins with only one subbasin!

dy.cosero <- function(qoutput = NULL, read.data = TRUE, 
                      prec = FALSE, wb = FALSE, comp = FALSE, eta = FALSE,
                      storage = FALSE, snowmelt = FALSE, group = NULL,
                      area = NULL, height = NULL, NB = 1, ...){
  library(dygraphs)
  library(htmltools)
#  library(hydroGOF)
  
  max0 <- function(x){
    max(0,x,na.rm = TRUE)
  }
  
  if(length(NB) > 1){
    NB <- NB[1]
    warning("Only the first element of NB will be used.")
  }
  
  if(wb){
    if(!prec){
      prec <- TRUE
      print("setting prec == TRUE, because wb == TRUE")
    }
  }
  
  if(read.data){
    if(is.null(qoutput)){
      stop("If read.data == TRUE, qoutput must be given")
    }
    qoutput <- TigR::readCosero(qoutput = qoutput,
                                prec = prec, comp = comp, eta = eta,
                                storage = storage, snowmelt = snowmelt,
                                area = area, ...)
    if(is.character(prec)){
      prec <- TRUE
    }
    if(is.character(comp)){
      comp <- TRUE
    }
    if(is.character(eta)){
      eta <- TRUE
    }
    if(is.character(storage)){
      storage <- TRUE
    }
    if(is.character(snowmelt)){
      snowmelt <- TRUE
    }
  }
  
  if(!is.null(height)){
    if(height == "auto"){
      number.of.plots <- sum(c(prec,comp,eta,storage,snowmelt,wb)) + 1 # for qoutput which will be plotted anyway
      height <- 1200 / number.of.plots
    }
  } 
    

  
  runoff.data <- cbind(qoutput$runoff$obs[,NB],qoutput$runoff$sim[,NB])
  if(wb){
    if(is.null(area)){
      stop("area must be given if wb == TRUE")
    }
    runoff.mm <- cbind(qoutput$runoff$obs.mm[,NB],qoutput$runoff$sim.mm[,NB])
  }
  kge <- hydroGOF::KGE(sim=runoff.data[,2],obs=runoff.data[,1])
  dy_graph <- list(
    dygraph(runoff.data, group = group, main = paste("runoff (KGE: ",round(kge,3),")",sep=""), height = height, ...) %>% dyRangeSelector(height = 10)
  )
  
  if(prec){
      prec.data <- cbind(qoutput$precipitation$rain[,NB], qoutput$precipitation$snow[,NB])
      dy_graph <- c(dy_graph, list(dygraph(prec.data, group = group, main = "precipitation", height = height, ...)))
  }
  
  if(wb){
    wb.data <- cbind(runoff.mm,prec.data)
    miss <- is.na(wb.data)
    wb.data[miss] <- 0.0 
    wb.data <- cumsum(wb.data)
    dy_graph <- c(dy_graph, list(dygraph(wb.data, group = group, main = "water balance", height = height, ...)))
  }
  
  if(comp){
    comp.data <- xts(cbind(apply(qoutput$components$HOF[,NB],1,FUN=max0),
                           apply(qoutput$components$SOF[,NB],1,FUN=max0),
                           apply(qoutput$components$GWF[,NB],1,FUN=max0)),
                     order.by = as.POSIXct(names(apply(qoutput$components$HOF,1,FUN=max0)),
                                           format = "%Y-%m-%d %H:%M:%S", tz = "utc"))
    comp.data <- cbind(rowSums(comp.data),
                       rowSums(comp.data[,2:3]),
                       comp.data[,3])
    colnames(comp.data) <- c("HOF","SOF","GWF")
    dy_graph <- c(dy_graph, list(dygraph(comp.data, group = group, main = "runoff components", height = height, ...)))
  }
  
  if(storage){
    storage.data <- cbind(qoutput$storages$BW0[,NB], qoutput$storages$BW1[,NB], qoutput$storages$BW2[,NB],
                          qoutput$storages$BW3[,NB],qoutput$storages$BW4[,NB])
    dy_graph <- c(dy_graph, list(dygraph(storage.data, group = group, main = "storage levels", height = height, ...)))
  }
  
  if(eta){
    eta.data <- qoutput$ETA$ETA[,NB]
    dy_graph <- c(dy_graph, list(dygraph(eta.data, group = group, main = "evapotranspiration", height = height, ...)))
  }
  
  if(snowmelt){
    snowmelt.data <- cbind(qoutput$snowmelt$snowmelt[,NB],qoutput$snowmelt$snowaccumulation[,NB])
    colnames(snowmelt.data) <- c("snowmelt","SWE")
    dy_graph <- c(dy_graph, list(
      dygraph(snowmelt.data, group = group, main = "snow", height = height, ...) %>%
        dySeries("SWE",axis="y2"))
      )
  }
  
    
  htmltools::browsable(htmltools::tagList(dy_graph))
}

