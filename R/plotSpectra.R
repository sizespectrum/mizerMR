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
#' @inheritParams valid_resources_arg
#' @param wlim A numeric vector of length two providing lower and upper limits
#'   for the w axis. Use NA to refer to the existing minimum or maximum.
#' @param ylim A numeric vector of length two providing lower and upper limits
#'   for the y axis. Use NA to refer to the existing minimum or maximum. Any
#'   values below 1e-20 are always cut off.
#' @param power The abundance is plotted as the number density times the weight
#' raised to `power`. The default \code{power = 1} gives the biomass
#' density, whereas \code{power = 2} gives the biomass density with respect
#' to logarithmic size bins.
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
                        wlim = c(NA, NA), ylim = c(NA, NA),
                        power = 1,
                        total = FALSE,
                        background = TRUE,
                        highlight = NULL, return_data = FALSE, ...) {
    # If called with MizerSim, we want to use the final time step
    if (is(object, "MizerSim")) {
        params <- setInitialValues(object@params, object)
    } else if (is(object, "MizerParams")) {
        params <- object
    } else {
        stop("The first argument must be either a MizerSim or a MizerParams object")
    }
    # set n_pp to total plankton abundance so that the total in mizer's
    # plotSpectra() gives the right curve
    params@initial_n_pp <- colSums(params@initial_n_other[["MR"]])

    df <- mizer::plotSpectra(params, species = species,
                        time_range = time_range,
                        wlim = wlim, ylim - ylim,
                        power = power, total = total,
                        background = background,
                        highlight = highlight,
                        resource = FALSE,
                        return_data = TRUE) |>
        dplyr::rename(Spectra = Species)

    resources <- valid_resources_arg(params, resources)

    if (is.na(wlim[1])) {
        wlim[1] <- min(params@w) / 100
    }
    if (is.na(wlim[2])) {
        wlim[2] <- max(params@w_full)
    }
    rf <- melt(initialNResource(params)) |>
        dplyr::filter(value > 0,
                      w >= wlim[[1]], w <= wlim[[2]]) |>
        dplyr::mutate(Legend = resource) |>
        dplyr::rename(Spectra = resource)
    # Impose ylim
    if (!is.na(ylim[2])) {
        rf <- rf[rf$value <= ylim[2], ]
    }
    if (is.na(ylim[1])) {
        ylim[1] <- 1e-20
    }
    rf <- rf[rf$value > ylim[1], ]
    df <- rbind(df, rf)
    plotDataFrame(df, params, xtrans = "log10", ytrans = "log10")
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
valid_resources_arg <- function(object, resources = NULL, return.logical = FALSE) {
    # This is mostly a copy of `valid_species_arg()` from core mizer just with
    # `species` replaced by `resources` and `no_sp` replaced with `no_res`.
    if (is(object, "MizerSim")) {
        params <- object@params
    } else if (is(object, "MizerParams")) {
        params <- object
    } else {
        stop("The first argument must be a MizerSim or MizerParams object.")
    }
    assert_that(is.logical(return.logical))
    all_resources <- params@resource_params$resource
    no_res <- nrow(params@resource_params)
    # Set resources if missing to list of all resources
    if (is.null(resources)) {
        resources <- params@resource_params$resource
        if (length(resources) == 0) {  # There are no resources.
            if (return.logical) {
                return(rep(FALSE, no_res))
            } else {
                return(NULL)
            }
        }
    }
    if (is.logical(resources)) {
        if (length(resources) != no_res) {
            stop("The boolean `resources` argument has the wrong length")
        }
        if (return.logical) {
            return(resources)
        }
        return(all_resources[resources])
    }
    if (is.numeric(resources)) {
        if (!all(resources %in% (1:no_res))) {
            warning("A numeric 'resources' argument should only contain the ",
                    "integers 1 to ", no_res)
        }
        resources.logical <- 1:no_res %in% resources
        if (sum(resources.logical) == 0) {
            stop("None of the numbers in the resources argument are valid resource indices.")
        }
        if (return.logical) {
            return(resources.logical)
        }
        return(all_resources[resources])
    }
    invalid <- setdiff(resources, all_resources)
    if (length(invalid) > 0) {
        warning("The following resources do not exist: ",
                toString(invalid))
    }
    resources <- intersect(resources, all_resources)
    if (length(resources) == 0) {
        stop("The resources argument matches none of the resources in the params object")
    }
    if (return.logical) {
        return(all_resources %in% resources)
    }
    resources
}
