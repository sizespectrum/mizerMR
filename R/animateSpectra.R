#' Animation of the abundance spectra
#'
#' `r lifecycle::badge("experimental")`
#'
#' As for [plotSpectra()], the quantity that is animated is chosen with the two
#' independent flags `biomass` and `per_log_size`, following mizer since version
#' 3.3. The resources are converted to a length axis with the same
#' weight-length relationship that mizer uses for its own resource.
#'
#' @param x A [mizerMRSim-class] object.
#' @param species Name or vector of names of the species to be plotted. By
#'   default all species are plotted.
#' @param log_x Whether to use a logarithmic x axis. Default TRUE.
#' @param log_y Whether to use a logarithmic y axis. Default TRUE.
#' @param log Deprecated. Use `log_x` and `log_y` instead.
#' @param wlim A numeric vector of length two providing lower and upper limits
#'   for the w axis. Use NA to refer to the existing minimum or maximum.
#' @param llim A numeric vector of length two providing lower and upper limits
#'   for the length axis, used when `size_axis = "l"`. Use NA to refer to the
#'   existing minimum or maximum.
#' @param ylim A numeric vector of length two providing lower and upper limits
#'   for the y axis. Use NA to refer to the existing minimum or maximum.
#' @param tlim A numeric vector of length two providing lower and upper limits
#'   for the time axis. Use NA to refer to the existing minimum or maximum.
#' @param size_axis Whether to plot against weight (`"w"`, the default) or
#'   against length (`"l"`).
#' @param per_log_size Whether to plot the density with respect to logarithmic
#'   size rather than with respect to size. Default FALSE.
#' @param total A boolean value that determines whether the total over all
#'   species and resources in the system is plotted as well. On a length axis
#'   the total is summed at equal length, as in mizer. Default is FALSE.
#' @param background A boolean value that determines whether background species
#'   are included. Ignored if the model does not contain background species.
#'   Default is TRUE.
#' @param frame_duration Duration of each animation frame in milliseconds.
#'   Default 500.
#' @param transition_duration Duration of the transition between frames in
#'   milliseconds. Default equals `frame_duration`.
#' @param easing The easing function for transitions. Default "linear".
#' @param resource A boolean value that determines whether the resources are
#'   included. Default is TRUE. Which of the resources are shown is determined
#'   by the `resources` argument.
#' @param ... Other arguments (currently unused).
#' @param resources Name or vector of names of the resources to be plotted. By
#'   default all resources are plotted.
#' @param time_range The time range to animate over. Either a vector of values
#'   or a vector of min and max time. Default is the entire time range of the
#'   simulation.
#' @param power The abundance is plotted as the number density times the weight
#'   raised to `power`. Usually left unset, in which case it is
#'   `biomass + per_log_size`. Giving a `power` that contradicts the flags is an
#'   error.
#' @param biomass Whether to animate the biomass density rather than the number
#'   density. Default TRUE.
#'
#' @return A plotly object
#' @family plotting functions
#' @method animate mizerMRSim
#' @importFrom mizer animate
#' @export
#' @name animateSpectra
animate.mizerMRSim <- function(x,
                               species = NULL,
                               log_x = TRUE,
                               log_y = TRUE,
                               log = NULL,
                               wlim = c(NA, NA),
                               llim = c(NA, NA),
                               ylim = c(NA, NA),
                               tlim = c(NA, NA),
                               size_axis = c("w", "l"),
                               per_log_size = NULL,
                               total = FALSE,
                               background = TRUE,
                               frame_duration = 500,
                               transition_duration = frame_duration,
                               easing = "linear",
                               resource = TRUE,
                               ...,
                               resources = NULL,
                               time_range,
                               power = NULL,
                               biomass = NULL) {
    sim <- x
    assert_that(is.flag(total), is.flag(background), is.flag(resource),
                is.number(frame_duration), frame_duration >= 0,
                is.number(transition_duration), transition_duration >= 0,
                is.string(easing),
                length(wlim) == 2, length(llim) == 2,
                length(ylim) == 2, length(tlim) == 2)
    spectrum <- resolve_spectrum_power(power, biomass, per_log_size)
    size_axis <- mizer_fn("plot_size_axis")(size_axis)
    log_axes <- parsePlotLog(log, log_x = log_x, log_y = log_y)
    log_x <- log_axes$log_x
    log_y <- log_axes$log_y

    params <- sim@params
    species <- valid_species_arg(sim, species)
    if (!background && any(params@species_params$is_background)) {
        species <- setdiff(species,
                           params@species_params$species[
                               params@species_params$is_background])
    }
    resources <- valid_resources_arg(sim, resources)
    if (missing(time_range)) {
        time_range  <- as.numeric(dimnames(sim@n)$time)
    }
    time_elements <- get_time_elements(sim, time_range)
    nf <- melt(sim@n[time_elements,
                     as.character(dimnames(sim@n)$sp) %in% species,
                     , drop = FALSE]) %>%
        dplyr::rename(Spectra = sp)
    # Add resource ----
    if (resource && length(resources) > 0) {
        nf_res <- melt(NResource(sim)[time_elements, resources,
                                      , drop = FALSE]) %>%
            dplyr::rename(Spectra = resource)
        nf <- rbind(nf, nf_res)
    }
    nf$Spectra <- as.character(nf$Spectra)

    # Impose the time limits ----
    if (!is.na(tlim[1])) nf <- nf[nf$time >= tlim[1], ]
    if (!is.na(tlim[2])) nf <- nf[nf$time <= tlim[2], ]

    # Deal with the power of the weight ----
    y_label <- mizer_fn("spectra_y_label")(spectrum$power, size_axis,
                                           biomass = spectrum$biomass,
                                           per_log_size = spectrum$per_log_size)
    nf$value <- nf$value * nf$w^spectrum$power

    # Impose the weight limits before converting the axis, as mizer does ----
    wlim <- mr_spectra_wlim(params, wlim)
    nf <- nf[nf$w >= wlim[1] & nf$w <= wlim[2], ]

    nf <- mr_convert_density_axis(nf, params, size_axis,
                                  per_log_size = spectrum$per_log_size,
                                  resources = resources)
    x_var <- mizer_fn("plot_size_x_var")(size_axis)

    # Add total ----
    # The total is summed over the series as they are drawn, so that on a
    # length axis it is a sum at equal length rather than at equal weight.
    if (total) {
        total_dat <- dplyr::rename(nf, Species = "Spectra")
        total_dat <- mizer_fn("add_total_line")(total_dat, x_var = x_var,
                                                value_col = "value",
                                                by = "time")
        total_dat <- total_dat[total_dat$Species == "Total", ]
        nf <- rbind(nf, dplyr::rename(total_dat, Spectra = "Species"))
    }
    if (identical(size_axis, "l")) {
        nf <- mizer_fn("filter_plot_length_limits")(nf, llim)
    }
    if (log_y) {
        nf <- nf[nf$value > 1e-20, ]
    }

    nf %>%
        plotly::plot_ly() %>%
        plotly::add_lines(x = stats::as.formula(paste0("~", x_var)),
                          y = ~value,
                          color = ~Spectra, colors = params@linecolour,
                          frame = ~time,
                          line = list(simplify = FALSE)) %>%
        plotly::layout(
            xaxis = mizer_fn("plotly_axis")(
                nf[[x_var]], mizer_fn("plot_size_xlim")(wlim, size_axis, llim),
                log_x, mizer_fn("plot_size_xlab")(size_axis)),
            yaxis = mizer_fn("plotly_axis")(nf$value, ylim, log_y, y_label)) %>%
        plotly::animation_opts(frame = frame_duration,
                               transition = transition_duration,
                               easing = easing)
}
