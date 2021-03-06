% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tau.R
\name{tau}
\alias{tau}
\title{Spatial dependence parameter.}
\usage{
tau(y, location)
}
\arguments{
\item{y}{A numeric vector of a response variable}

\item{location}{A matrix of spatial locations}
}
\value{
A value of spatial dependence parameter.
}
\description{
Spatial dependence parameter.
}
\examples{
tau(y = sim[, 1], location = sim[, 2:3])

}
