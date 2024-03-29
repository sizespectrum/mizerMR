% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setMultipleResources.R
\name{valid_resource_rate}
\alias{valid_resource_rate}
\title{Return valid resource rate array}
\usage{
valid_resource_rate(params, resource_params = NULL, resource_rate = NULL)
}
\arguments{
\item{params}{A MizerParams object}

\item{resource_rate}{Array (resource x size) of the
intrinsic resource replenishment rate}
}
\value{
An array (resource x size) with the resource capacities
}
\description{
If \verb{resource rate} is given it is checked for validity. If it does not
have a comment, then it is given the comment "set manually". This is then
returned. If \verb{resource rate} is missing or NULL, but one was set by the
user and stored in \code{params} with a comment, then this is returned. Otherwise
a resource rate is calculated from \code{resource_params}. If this is NULL to
it is taken from \code{params}.
}
