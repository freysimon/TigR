#' Plots an xts object splitted in hydrological years
#' @param sim xts object with simulated data from COSERO
#' @param obs xts object with observed data from COSERO
#' @param plot logical. should the results be plotted or returned?
#' @param NB numeric. Subbasin to be plotted
#' @param maxrow numeric. Maximum plots in a row
#' @param dev characterstring. format of the plot (valid is "dev","pdf","png")
#' @param plotfile characterstring. Filename of the plot if dev != "dev"
#' @param ... additional arguments passed to plot
#' @return An xts object is plotted
#' @seealso \code{\link{dev.new.file}}
#' @seealso \code{\link{plot.better.xts}}
yearplot <- function(sim, obs, NB=NULL, plot = TRUE, maxrow = 4, dev = "dev", plotfile = NULL, ...){

  library(magrittr)

  if(is.null(NB)){
    warning("NB = NULL --> processing NB1")
    NB = 1
  }
  if(dev != "dev"){
    if(is.null(plotfile)){
      plotfile = "Rplot02"
      warning("filename is missing. Writing plot to RPlot01")
    }
  }

  # scan for years
  years <- index(sim) %>%
              format("%Y") %>%
              as.numeric() %>%
              unique()

  # preallocate a list with length of found years
  temp <- vector("list",length(years))
  names(temp) <- as.character(years)



# subset ts into singe years
  for(k in 1:(length(years)-1)){
    jj <- paste(years[k],"-11-01/",years[k+1],"-10-31",sep="")
    temp[[k]]$sim <- sim[jj][,NB]
    temp[[k]]$obs <- obs[jj][,NB]
  }


  if(!plot){
    return(temp)
  } else {

    # find extreme values
    obs[obs < 0] <- NA
    MIN <- min(sim[,NB],obs[,NB],na.rm=TRUE)
    MAX <- max(sim[,NB],obs[,NB],na.rm=TRUE)

    colrow <- setcolumns(length(years),maxrow)

    dev.new.file(device = dev, width=colrow[2]*6,height=colrow[1]*3, filename = plotfile)
    par(mar=c(3,4,2,3), mgp = c(1.8,0.2,0), tcl = 0.3, las = 1,
        xaxs = "i", yaxs = "i", mfrow = colrow)

    for(k in 1:(length(years)-1)){
      plot.better.xts(temp[[k]]$obs, ylim = c(MIN*0.9,MAX*1.1), main="",
                      lines.col="blue", minor.ticks = FALSE,
                      major.format="%Y-%m-%d", type = "n")

      lines(temp[[k]]$obs, col="blue", lwd = 2, ...)
      lines(temp[[k]]$sim, col="orange", lwd = 2, ...)
      mtext(text=expression(paste("Runoff ",m^3,s^-1,sep=" ")), side = 2, line = 2.5, las = 0, cex = 0.8)
      mtext(text=paste(abc(k),")",sep=""),side = 3, adj=0.03,font=2,line=-1.5)
      if(k == 1){
        legend("topright",
               c("Simulated",
                 "Observed"),
               col=c("orange","blue"),
               lwd = 2,
               bty="n")
      }
    }
    par(new=TRUE,mfrow=c(1,1),mar=c(0.5,0.5,0.5,0.5))
    mtext(paste("Subbasin",NB,sep=" "),side=3,line=-0.4)

    if(dev != "dev") dev.off()
  }

}
