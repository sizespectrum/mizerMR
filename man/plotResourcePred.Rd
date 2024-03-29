% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotResource.R
\name{plotResourcePred}
\alias{plotResourcePred}
\alias{plotlyResourcePred}
\title{Plot functions regarding the resources. Compatible with non mizerMR objects
Plot the mortality applied on the resource spectrum(s)}
\usage{
plotResourcePred(object, proportion = TRUE, return_data = FALSE)

plotlyResourcePred(object, proportion = TRUE, ...)
}
\arguments{
\item{object}{An object of class \linkS4class{MizerSim} or
\linkS4class{MizerParams}.}

\item{proportion}{A boolean value that determines whether values should be
displayed as proportions from 0 to 1 or with their actual values. Default is TRUE.}

\item{return_data}{A boolean value that determines whether the formatted data
used for the plot is returned instead of the plot itself. Default value is FALSE}

\item{...}{Other arguments (currently unused)}
}
\value{
A ggplot2 object, unless \code{return_data = TRUE}, in which case a data
frame with the four variables 'w', 'value', 'Predator', 'Resource' is returned.
}
\description{
Plot functions regarding the resources. Compatible with non mizerMR objects
Plot the mortality applied on the resource spectrum(s)
}
\examples{
\donttest{
plotResourcePred(NS_params)

# Returning the data frame
fr <- plotResourcePred(NS_params, return_data = TRUE)
str(fr)
}
}
\seealso{
\link{plotting_functions}

Other plotting functions: 
\code{\link{animateSpectra}()},
\code{\link{plotDiet}()},
\code{\link{plotResourceLevel}()},
\code{\link{plotSpectra}()}
}
\concept{plotting functions}
