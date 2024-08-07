% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.dimensions.r
\name{get.dimensions}
\alias{get.dimensions}
\title{Get dimensions from COSERO parameters}
\usage{
get.dimensions(x, full = TRUE)
}
\arguments{
\item{x}{character string pointing towards a COSERO parameter file or giving an object derived from read_COSERO_par}

\item{full}{logical. See details in return.}
}
\value{
If full == TURE a matrix giving the dimensions NB, IZ, NZ will be returned. If full == FALSE only the summary (NB, maxIZ per NB) will be returned.
}
\description{
Get dimensions (NB, IZ, NZ) from COSERO parameters.
}
\examples{
    ### do not run ###
    get.dimensions("path/to/parameter_COSERO.par")
    
    x <- read_COSERO_par("path/to/parameter_COSERO.par")
    get.dimensions(x)
}
\seealso{
\code{\link{read_COSERO_par}}
}
\author{
Simon Frey
}
