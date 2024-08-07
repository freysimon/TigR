% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.r
\name{head.list}
\alias{head.list}
\title{Return the first or last part of a list}
\usage{
head.list(obj, n = 6L, ...)
}
\arguments{
\item{obj}{a list object}

\item{n}{a single integer. If positive, prints the first n items for the
list and all entries in the list. If negative, prints all but the last
n items in the list.}

\item{...}{additional parameters passed on to head()}
}
\value{
a list of length n, with items in the list of length n
}
\description{
Returns the first or last part of a list. Instead of returning the first
n entries as the standard head() does, it attempts to call head()
recursively on the entries in the list. If it fails, it will return the
particular entry (standard behavior).
}
\examples{
    a <- list()
    a[[1]] <- c(1:100)
    a[[2]] <- c(500:1000)
    head.list(a)
    # same result
    head(a)
}
\references{
\code{https://gist.github.com/pimentel/256fc8c9b5191da63819}
}
\author{
pimentel
}
