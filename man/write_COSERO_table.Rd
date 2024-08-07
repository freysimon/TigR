% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readCOSEROpar.r
\name{write_COSERO_table}
\alias{write_COSERO_table}
\title{Write a COSERO NBIZ Parameterfile}
\usage{
write_COSERO_table(x, file = "para.txt", pars = "ALL")
}
\arguments{
\item{x}{output from \link{read_COSERO_par}}

\item{file}{character string. Filename of the output file. If NULL the output will be returned to R.}

\item{pars}{character vector or "ALL". Names of parameters that will be exported. See Details}
}
\description{
write a COSERO NBIZ Parameterfile
}
\details{
If pars == ALL the following parameters will be exported:

  BAREGR, BETA, CTMAX, CTMIN, ETSLPCOR, FKFAK, FK, H1, H2, KBF, M, NC, NZ , NVAR, PWP, RAINTRT, SNOWCOR, 
  SNOWTRT, TAB1, TAB2, TAB4, THRT, TVS1, TVS2, WATERBODY
}
\seealso{
\link{write_COSEROreg_par} for writing new COSEROreg parameter files
}
\author{
Simon Frey
}
