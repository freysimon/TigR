% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_COSEROreg_par.r
\name{split__NZNM_monthly}
\alias{split__NZNM_monthly}
\title{Disaggregate parameters using the dimensions landuse and months from a COSERO parameter file}
\usage{
split__NZNM_monthly(x, parameter, wnames)
}
\arguments{
\item{x}{the output from \link{read_COSERO_par}}

\item{parameter}{character. Name of the parameter that will be split and returned. Note the the parameter name must not contain numbers indicating the month. E.g. INTMAX instead of INTMAX1}

\item{wnames}{integer or integer vector. Position(s) of names in vector holding all COSERO Parameter names.}
}
\value{
a matrix containing 12 columns
}
\description{
Disaggregate parameters with dimensions NC,NM to monthly columns
}
\details{
In COSERO (old) some parameters use the dimensions NC,NM (landuse, month) and are stored in the parameterfile
      in one column where all NBIZ of month 1 are placed above the ones of NBIZ of month 2 and so on and so forth. In COSEROreg, however, the
      parameterfile uses distinct columns per month. This routine searches for the corresponding land use (NC) per cell (NZ) and assigns the
      monthly values to each column.
}
\author{
Simon Frey
}
