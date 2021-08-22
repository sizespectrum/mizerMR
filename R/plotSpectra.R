#' Plot the abundance spectra
#'
#' Plots the number density multiplied by a power of the weight, with the power
#' specified by the `power` argument.
#'
#' When called with a \linkS4class{MizerSim} object, the abundance is averaged
#' over the specified time range (a single value for the time range can be used
#' to plot a single time step). When called with a \linkS4class{MizerParams}
#' object the initial abundance is plotted.
#'
#' @param object An object of class \linkS4class{MizerSim} or
#'   \linkS4class{MizerParams}.
#' @param species The species to be selected. Optional. By default all target
#'   species are selected. A vector of species names, or a
#'   numeric vector with the species indices, or a logical vector indicating for
#'   each species whether it is to be selected (TRUE) or not.
#' @inheritParams valid_resource_arg
#' @param time_range The time range (either a vector of values, a vector of min
#'   and max time, or a single value) to average the abundances over. Default is
#'   the final time step. Ignored when called with a \linkS4class{MizerParams}
#'   object.
#' @param wlim A numeric vector of length two providing lower and upper limits
#'   for the w axis. Use NA to refer to the existing minimum or maximum.
#' @param ylim A numeric vector of length two providing lower and upper limits
#'   for the y axis. Use NA to refer to the existing minimum or maximum. Any
#'   values below 1e-20 are always cut off.
#' @param power The abundance is plotted as the number density times the weight
#' raised to `power`. The default \code{power = 1} gives the biomass
#' density, whereas \code{power = 2} gives the biomass density with respect
#' to logarithmic size bins.
#' @param biomass `r lifecycle::badge("deprecated")`
#'  Only used if `power` argument is missing. Then
#'   \code{biomass = TRUE} is equivalent to \code{power=1} and
#'   \code{biomass = FALSE} is equivalent to \code{power=0}
#' @param total A boolean value that determines whether the total over all
#'   species and resources in the system is plotted as well. Note that even if
#'   the plot only shows a selection of species, the total is including all
#'   species. Default is FALSE.
#' @param background A boolean value that determines whether background species
#'   are included. Ignored if the model does not contain background species.
#'   Default is TRUE.
#' @param highlight Name or vector of names of the species to be highlighted.
#' @param return_data A boolean value that determines whether the formatted data
#' used for the plot is returned instead of the plot itself. Default value is FALSE
#' @param ... Other arguments (currently unused)
#'
#' @return A ggplot2 object, unless `return_data = TRUE`, in which case a data
#'   frame with the four variables 'w', 'value', 'Species', 'Legend' is
#'   returned.
#' @export
#' @family plotting functions
#' @seealso [plotting_functions]
plotSpectra <- function(object, species = NULL, resources = NULL,
                        time_range,
                        wlim = c(NA, NA), ylim = c(NA, NA),
                        power = 1, biomass = TRUE,
                        total = FALSE,
                        background = TRUE,
                        highlight = NULL, return_data = FALSE, ...) {

}

#' Helper function to assure validity of resources argument
#'
#' If the resources argument contains invalid resources, then these are
#' ignored but a warning is issued. If non of the resources is valid, then
#' an error is produced.
#'
#' @param object A MizerSim or MizerParams object from which the resources
#'   should be selected.
#' @param resources The resources to be selected. Optional. By default all
#'   resources are selected. A vector of resource names, or a numeric vector
#'   with the resource indices, or a logical vector indicating for each resource
#'   whether it is to be selected (TRUE) or not.
#' @param return.logical Whether the return value should be a logical vector.
#'   Default FALSE.
#'
#' @return A vector of resource names, in the same order as specified in the
#'   'resources' argument. If 'return.logical = TRUE' then a logical vector is
#'   returned instead, with length equal to the number of resources, with
#'   TRUE entry for each selected resource.
#' @export
#' @concept helper
valid_resource_arg <- function(object, resources = NULL, return.logical = FALSE) {

}
