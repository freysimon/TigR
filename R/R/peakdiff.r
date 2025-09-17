#' Calculate the peak difference of two hydrographs
#' @description Calculate the peak difference of two hydrographs.
#' @param obs xts object. The observed hydrograph.
#' @param sim xts object. The simulated hydrograph.
#' @param timesteps numeric. How many timesteps should be considered (equal distributed around the observed peak)?
#' @param width numeric. Number of timesteps on eighter side of the peak that must be smaller than the peak. Corresponds to paramteter m in \code{\link{find_peaks}}
#' @param FUN function. Objective function to be calculated whitin the region of the peak(s).
#' @param nop integer. Number of peaks being evaluated.
#' @param ... additional arguments passed to FUN
#' @author Simon Frey
#' @export
#' @import xts
#' @return a vector with the results of the objective function
#' @details FUN should be a function that can operate with sim and obs values and returns one single value.
#'     
#'     Peaks are detected in obs only!
#' @seealso \code{\link{find_peaks}}

peakdiff <- function(sim, obs, timesteps, width = 12, FUN = me, nop = 5, ...){
  # check for xts
  library(xts)

  if(!is.xts(obs)){
    stop("obs must be of type xts")
  }
  if(!is.xts(sim)){
    stop("sim must be of type xts")
  }

  # locate peaks
  #peaks <- find_peaks(as.numeric(obs), m = width, order = "d")


  obstemp <- obs
  peakregion <- list()
  of <- vector(length = nop, mode = "numeric")
  for(k in 1:nop){
    peakregion[[k]] <- obs[max(0,(which(obstemp == max(obstemp, na.rm = TRUE)) - timesteps)):
                         min(length(obstemp),(which(obstemp == max(obstemp, na.rm = TRUE)) + timesteps))]
    # cbind sim
    peakregion[[k]] <- cbind(peakregion[[k]], sim[index(peakregion[[k]])])
    colnames(peakregion[[k]]) <- c("OBS","SIM")
    peakregion[[k]] <- peakregion[[k]][,c("SIM","OBS")]

    obstemp[index(peakregion[[k]])] <- NA

    # calculate objective function
    of[k] <- FUN(sim = peakregion[[k]][,1], obs = peakregion[[k]][,2], ...)
  }

  return(list("peaks" = peakregion,"of" = of))
}