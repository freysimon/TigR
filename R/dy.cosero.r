#' Plot results regarding runoff from COSERO using dygraphs
#' @author Simon Frey
#' @export
#' @import dygraphs
#' @import htmltools
#' @description Plot the results regarding runoff from COSERO using dygraphs. The output may be grouped together with other results.
#' @param qoutput Either a path to the file to be read or the result from \link{readCosero}
#' @param read.data logical. If TRUE, qoutput will be read in using \link{readCosero}
#' @param prec logical or character string. Should precipitation data be read in? Maybe a path to the corresponding file.
#' @param comp logical or character string. Should data about the composition of runoff be read in? Maybe a path to the corresponding file.
#' @param storage logical or character string. Should storage data be read in? Maybe a path to the corresponding file.
#' @param snowmelt logical or character string. Should snow data be read in? Maybe a path to the corresponding file.
#' @param eta logical or character string. Should data about evapotranspiration be read in? Maybe a path to the corresponding file.
#' @param group character string or NULL. Used to group dygraph plots. See \link{dygraph}
#' @param height numeric or NULL. Height in pixels (optional, defaults to automatic sizing)
#' @details If read,data == FALSE the results from \code{\link{readCosero}} must be passed on to \code{dy.cosero}.
#'     In this case the parameters \code{prec},\code{comp},\code{storage},\code{snowmelt}, and \code{eta} may be logical

dy.cosero <- function(qoutput = NULL, read.data = TRUE, 
                      prec = FALSE, comp = FALSE, eta = FALSE,
                      storage = FALSE, snowmelt = FALSE, group = NULL,
                      area = NULL, height = NULL, ...){
  library(dygraphs)
  library(htmltools)
  
  max0 <- function(x){
    max(0,x,na.rm = TRUE)
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
  
  runoff.data <- cbind(qoutput$runoff$obs,qoutput$runoff$sim)
  dy_graph <- list(
    dygraph(runoff.data, group = group, main = "runoff", height = height, ...) %>% dyRangeSelector(height = 10)
  )
  
  if(prec){
      prec.data <- cbind(qoutput$precipitation$rain, qoutput$precipitation$snow)
      dy_graph <- c(dy_graph, list(dygraph(prec.data, group = group, main = "precipitation", height = height, ...)))
  }
  
  if(comp){
    comp.data <- xts(cbind(apply(qoutput$components$HOF,1,FUN=max0),
                           apply(qoutput$components$SOF,1,FUN=max0),
                           apply(qoutput$components$GWF,1,FUN=max0)),
                     order.by = as.POSIXct(names(apply(qoutput$components$HOF,1,FUN=max0)),
                                           format = "%Y-%m-%d %H:%M:%S", tz = "utc"))
    comp.data <- cbind(rowSums(comp.data),
                       rowSums(comp.data[,2:3]),
                       comp.data[,3])
    colnames(comp.data) <- c("HOF","SOF","GWF")
    dy_graph <- c(dy_graph, list(dygraph(comp.data, group = group, main = "runoff components", height = height, ...)))
  }
  
  if(storage){
    storage.data <- cbind(qoutput$storages$BW0, qoutput$storages$BW1, qoutput$storages$BW2,
                          qoutput$storages$BW3,qoutput$storages$BW4)
    dy_graph <- c(dy_graph, list(dygraph(storage.data, group = group, main = "storage levels", height = height, ...)))
  }
  
  if(eta){
    eta.data <- qoutput$ETA$ETA
    dy_graph <- c(dy_graph, list(dygraph(eta.data, group = group, main = "evapotranspiration", height = height, ...)))
  }
  
  if(snowmelt){
    snowmelt.data <- cbind(qoutput$snowmelt$snowmelt,qoutput$snowmelt$snowaccumulation)
    colnames(snowmelt.data) <- c("snowmelt","SWE")
    dy_graph <- c(dy_graph, list(
      dygraph(snowmelt.data, group = group, main = "snow", height = height, ...) %>%
        dySeries("SWE",axis="y2"))
      )
  }
  
    
  htmltools::browsable(htmltools::tagList(dy_graph))
}

