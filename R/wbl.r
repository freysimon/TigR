
#' Water balance per time span
#' @param sim xts object of simulated data
#' @param obs xts object of observed data
#' @param time character string of time span.
#' @return list containing xts objects of sim and obs data per time span unit
#' @description Calculate water balance within a time span
#' @details Sim and obs are subsetted into bits given by time. Accepted values are "none", "year".
#'
#'    Cumulated sums are then calculated of the time spans for sim and obs and returned as a list.
#'
#'    If time = year, sim and obs are subsetted in hydrological years (1st of November until 30th of October).
#' @export
#' @import magrittr
#' @author Simon Frey
#' @examples
#'     data("area")
#'     fpath <- system.file("extdata", "Q_output.txt", package = "TigeR")
#'     x <- readCosero(qoutput = fpath, area = area)
#'     sim <- x$runoff$sim.mm[,1]
#'     obs <- x$runoff$obs.mm[,1]
#'
#'     wb <- wbl(sim = sim, obs = obs)
#' @seealso \code{\link{readCosero}}
#'
wbl <- function(sim, obs, time = "year"){

  if(!time %in% c("none","year")){
    stop("time unknown. Possible values are none and year.")
  }

  library(magrittr)

  if(time != "none"){
    time <- "%Y"
    # scan for time spans
    tspan <- index(sim) %>%
        format(time) %>%
        as.numeric() %>%
        unique()

    # preallocate a list with length of found years
    temp <- vector("list",length(tspan))
    names(temp) <- as.character(tspan)

    # subset ts into singe years
    for(k in 1:(length(tspan)-1)){
      jj <- paste(tspan[k],"-11-01/",tspan[k+1],"-10-31",sep="")
      temp[[k]] <- cbind(sim[jj] %>% cumsum(),
                       obs[jj] %>% cumsum())
    }
  } else {
    temp <- list(cbind(cumsum(sim), cumsum(obs)))
  }

  return(temp)

}
