#' Plot the abundance spectra
#'
#' Plots the number density multiplied by a power of the weight. As in mizer
#' since version 3.3, the quantity that is plotted is chosen with the two
#' independent flags `biomass` and `per_log_size`; the power of the weight
#' multiplying the number density is their sum. The `power` argument is still
#' accepted and is the only way to ask for a power that is not such a sum.
#'
#' When called with a [mizer::MizerSim-class] object, the abundance is averaged
#' over the specified time range (a single value for the time range can be used
#' to plot a single time step). When called with a [mizer::MizerParams-class]
#' object the initial abundance is plotted.
#'
#' The resources are drawn on a length axis (`size_axis = "l"`) with the
#' weight-length relationship that mizer uses for its own resource: the `a` and
#' `b` entries of mizer's `resource_params` slot if they are given, and
#' otherwise mizer's defaults. This is the same relationship with which the
#' combined resource enters the total, so all lines can be read against each
#' other.
#'
#' @param object An object of class [mizer::MizerSim-class] or
#'   [mizer::MizerParams-class].
#' @param species The species to be selected. Optional. By default all target
#'   species are selected. A vector of species names, or a
#'   numeric vector with the species indices, or a logical vector indicating for
#'   each species whether it is to be selected (TRUE) or not.
#' @inheritParams valid_resources_arg
#' @param wlim A numeric vector of length two providing lower and upper limits
#'   for the w axis. Use NA to refer to the existing minimum or maximum.
#' @param llim A numeric vector of length two providing lower and upper limits
#'   for the length axis, used when `size_axis = "l"`. Use NA to refer to the
#'   existing minimum or maximum.
#' @param ylim A numeric vector of length two providing lower and upper limits
#'   for the y axis. Use NA to refer to the existing minimum or maximum. Any
#'   values below 1e-20 are always cut off.
#' @param time_range The time range to average over when called with a
#'   [mizer::MizerSim-class] object.
#' @param geometric_mean Whether to average abundances using a geometric mean
#'   when called with a [mizer::MizerSim-class] object.
#' @param power The abundance is plotted as the number density times the weight
#'   raised to `power`. Usually left unset, in which case it is
#'   `biomass + per_log_size`. Giving a `power` that contradicts the flags is an
#'   error.
#' @param biomass Whether to plot the biomass density rather than the number
#'   density. Default TRUE.
#' @param per_log_size Whether to plot the density with respect to logarithmic
#'   size rather than with respect to size. Default FALSE.
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
#' @param resource A boolean value that determines whether the resources are
#'   included in the plot. Default is TRUE. Which of the resources are shown is
#'   determined by the `resources` argument.
#' @param log_x Whether to use a logarithmic x axis. Default TRUE.
#' @param log_y Whether to use a logarithmic y axis. Default TRUE.
#' @param log Deprecated. Use `log_x` and `log_y` instead.
#' @param size_axis Whether to plot against weight (`"w"`, the default) or
#'   against length (`"l"`).
#' @param ... Other arguments (currently unused)
#'
#' @return A ggplot2 object, unless `return_data = TRUE`, in which case a data
#'   frame with the four variables 'w' (or 'l' for a length axis), 'value',
#'   'Spectra' and 'Legend' is returned.
#' @family plotting functions
#' @seealso [plotting_functions]
#' @export
#' @name plotSpectra
plotSpectra.mizerMRSim <- function(object, species = NULL,
                                   wlim = c(NA, NA), llim = c(NA, NA),
                                   ylim = c(NA, NA),
                                   power = NULL, biomass = NULL,
                                   per_log_size = NULL,
                                   total = FALSE,
                                   resource = TRUE,
                                   background = TRUE,
                                   highlight = NULL,
                                   log_x = TRUE, log_y = TRUE,
                                   log = NULL,
                                   size_axis = c("w", "l"),
                                   return_data = FALSE,
                                   ...,
                                   resources = NULL,
                                   time_range,
                                   geometric_mean = FALSE) {
    spectrum <- resolve_spectrum_power(power, biomass, per_log_size)
    size_axis <- mizer_fn("plot_size_axis")(size_axis)
    log_axes <- parsePlotLog(log, log_x = log_x, log_y = log_y)
    log_x <- log_axes$log_x
    log_y <- log_axes$log_y
    log <- NULL

    if (missing(time_range)) {
        time_range  <- max(as.numeric(dimnames(object@n)$time))
    }
    time_elements <- get_time_elements(object, time_range)
    mean_fn <- mean
    if (geometric_mean) {
        mean_fn <- function(x) {
            exp(mean(log(x)))
        }
    }

    params <- object@params
    wlim <- mr_spectra_wlim(params, wlim)
    # Present the combined resource as the built-in resource so that the total
    # calculated by mizer's method includes all resources.
    object@n_pp <- apply(NResource(object), c(1, 3), sum)
    df <- NextMethod(species = species, time_range = time_range,
                     geometric_mean = geometric_mean,
                     wlim = wlim, llim = llim, ylim = ylim,
                     power = spectrum$power, biomass = spectrum$biomass,
                     per_log_size = spectrum$per_log_size,
                     total = total,
                     resource = FALSE, background = background,
                     highlight = highlight,
                     log_x = log_x, log_y = log_y, log = NULL,
                     size_axis = size_axis,
                     return_data = TRUE, ...) %>%
        dplyr::rename(Spectra = Species)
    # mizer names the value column after the y-axis label; take the label from
    # there so that it always agrees with mizer, and normalise the name to
    # "value" so that rbind with the resource data frame works.
    y_label <- names(df)[[2]]
    names(df)[[2]] <- "value"

    resources <- valid_resources_arg(params, resources)
    if (resource && length(resources) > 0) {
        n_res <- apply(NResource(object)[time_elements, resources, ,
                                         drop = FALSE],
                       c(2, 3), mean_fn)
        rf <- mr_resource_spectra_data(params, n_res, spectrum = spectrum,
                                       size_axis = size_axis, wlim = wlim,
                                       llim = llim, ylim = ylim)
        df <- rbind(df, rf[, names(df), drop = FALSE])
    }
    if (return_data) {
        return(df)
    }
    mr_plot_spectra_data(df, params, y_label = y_label, size_axis = size_axis,
                         wlim = wlim, llim = llim, ylim = ylim,
                         log_x = log_x, log_y = log_y, highlight = highlight)
}

#' @rdname plotSpectra
#' @export
plotSpectra.mizerMR <- function(object, species = NULL,
                                wlim = c(NA, NA), llim = c(NA, NA),
                                ylim = c(NA, NA),
                                power = NULL, biomass = NULL,
                                per_log_size = NULL,
                                total = FALSE,
                                resource = TRUE,
                                background = TRUE,
                                highlight = NULL,
                                log_x = TRUE, log_y = TRUE,
                                log = NULL,
                                size_axis = c("w", "l"),
                                return_data = FALSE,
                                ...,
                                resources = NULL) {
    params <- object
    spectrum <- resolve_spectrum_power(power, biomass, per_log_size)
    size_axis <- mizer_fn("plot_size_axis")(size_axis)
    log_axes <- parsePlotLog(log, log_x = log_x, log_y = log_y)
    log_x <- log_axes$log_x
    log_y <- log_axes$log_y
    log <- NULL

    wlim <- mr_spectra_wlim(params, wlim)
    # Present the combined resource as the built-in resource so that the total
    # calculated by mizer's method includes all resources.
    object@initial_n_pp <- colSums(object@initial_n_other[["MR"]])

    df <- NextMethod(species = species, wlim = wlim, llim = llim, ylim = ylim,
                     power = spectrum$power, biomass = spectrum$biomass,
                     per_log_size = spectrum$per_log_size,
                     total = total,
                     background = background,
                     highlight = highlight,
                     resource = FALSE,
                     log_x = log_x, log_y = log_y, log = NULL,
                     size_axis = size_axis,
                     return_data = TRUE, ...) %>%
        dplyr::rename(Spectra = Species)
    # mizer names the value column after the y-axis label; take the label from
    # there so that it always agrees with mizer, and normalise the name to
    # "value" so that rbind with the resource data frame works.
    y_label <- names(df)[[2]]
    names(df)[[2]] <- "value"

    resources <- valid_resources_arg(params, resources)
    if (resource && length(resources) > 0) {
        n_res <- initialNResource(params)[resources, , drop = FALSE]
        rf <- mr_resource_spectra_data(params, n_res, spectrum = spectrum,
                                       size_axis = size_axis, wlim = wlim,
                                       llim = llim, ylim = ylim)
        df <- rbind(df, rf[, names(df), drop = FALSE])
    }
    if (return_data) {
        return(df)
    }
    mr_plot_spectra_data(df, params, y_label = y_label, size_axis = size_axis,
                         wlim = wlim, llim = llim, ylim = ylim,
                         log_x = log_x, log_y = log_y, highlight = highlight)
}

#' Default weight limits for a multiple-resource spectrum plot
#'
#' The same defaults that mizer uses when the resource is shown: from a
#' hundredth of the smallest consumer size to the largest resource size.
#'
#' @param params A \linkS4class{mizerMR} object.
#' @param wlim The weight limits supplied by the user.
#' @return The weight limits with any NA replaced by the default.
#' @keywords internal
mr_spectra_wlim <- function(params, wlim) {
    assert_that(length(wlim) == 2)
    if (is.na(wlim[1])) {
        wlim[1] <- min(params@w) / 100
    }
    if (is.na(wlim[2])) {
        wlim[2] <- max(params@w_full)
    }
    wlim
}

#' Assemble the resource part of a spectrum plot
#'
#' Turns an array of resource number densities into the same kind of plotting
#' data frame that mizer's `plotSpectra()` returns for the species, with the
#' size limits, the size axis and the value limits applied in the same order.
#'
#' @param params A \linkS4class{mizerMR} object.
#' @param n_res An array (resource x size) of resource number densities.
#' @param spectrum The list returned by [resolve_spectrum_power()].
#' @param size_axis Either "w" or "l".
#' @param wlim,llim,ylim Limits, with the weight limits already defaulted by
#'   [mr_spectra_wlim()].
#'
#' @return A data frame with the variables 'w' (or 'l'), 'value', 'Spectra' and
#'   'Legend'.
#' @keywords internal
mr_resource_spectra_data <- function(params, n_res, spectrum, size_axis,
                                     wlim, llim, ylim) {
    rf <- melt(n_res) %>%
        dplyr::filter(value > 0,
                      w >= wlim[[1]], w <= wlim[[2]]) %>%
        dplyr::rename(Spectra = resource)
    rf$Spectra <- as.character(rf$Spectra)
    rf$Legend <- rf$Spectra
    rf$value <- rf$value * rf$w^spectrum$power
    rf <- rf[, c("w", "value", "Spectra", "Legend"), drop = FALSE]
    rf <- mr_convert_density_axis(rf, params, size_axis,
                                  per_log_size = spectrum$per_log_size)
    if (identical(size_axis, "l")) {
        rf <- mizer_fn("filter_plot_length_limits")(rf, llim)
    }
    # Impose the limits on the displayed density, after its units have been
    # converted for a length axis, exactly as mizer does.
    if (!is.na(ylim[2])) {
        rf <- rf[rf$value <= ylim[2], ]
    }
    filter_min <- if (is.na(ylim[1])) 1e-20 else ylim[1]
    rf[rf$value > filter_min, ]
}

#' Draw an assembled multiple-resource spectrum plot
#'
#' @param df The plotting data frame.
#' @param params A \linkS4class{mizerMR} object.
#' @param y_label The label for the y axis.
#' @param size_axis Either "w" or "l".
#' @param wlim,llim,ylim Limits.
#' @param log_x,log_y Whether the axes are logarithmic.
#' @param highlight Species to highlight.
#' @return A ggplot object.
#' @keywords internal
mr_plot_spectra_data <- function(df, params, y_label, size_axis,
                                 wlim, llim, ylim, log_x, log_y,
                                 highlight = NULL) {
    plotDataFrame(df, params,
                  xlab = mizer_fn("plot_size_xlab")(size_axis),
                  ylab = y_label,
                  xtrans = if (log_x) "log10" else "identity",
                  ytrans = if (log_y) "log10" else "identity",
                  xlim = mizer_fn("plot_size_xlim")(wlim, size_axis, llim),
                  ylim = ylim,
                  highlight = highlight, legend_var = "Legend")
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
    all_resources <- resource_params(params)$resource
    no_res <- nrow(resource_params(params))
    # Set resources if missing to list of all resources
    if (is.null(resources)) {
        resources <- resource_params(params)$resource
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
