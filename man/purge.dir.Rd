% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/purge.dir.r
\name{purge.dir}
\alias{purge.dir}
\title{Purge directory}
\usage{
purge.dir(x)
}
\arguments{
\item{x}{character string. Path of the dir to purge.}
}
\description{
Purge a directory. All files and subdirectories will be deleted.
}
\examples{
    #### Not run ####
    purge.dir(x="C:/TEMP")
}
\seealso{
\link{unlink}

\link{file.remove}
}
\author{
Simon Frey
}
