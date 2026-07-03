#' Animation of the abundance spectra
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param x A [mizerMRSim-class] object.
#' @param species Name or vector of names of the species to be plotted. By
#'   default all species are plotted.
#' @param log_x Whether to use a logarithmic x axis. Default TRUE.
#' @param log_y Whether to use a logarithmic y axis. Default TRUE.
#' @param log Deprecated. Use `log_x` and `log_y` instead.
#' @param wlim A numeric vector of length two providing lower and upper limits
#'   for the w axis. Use NA to refer to the existing minimum or maximum.
#' @param llim Ignored (compatibility argument from mizer's generic).
#' @param ylim A numeric vector of length two providing lower and upper limits
#'   for the y axis. Use NA to refer to the existing minimum or maximum. Any
#'   values below 1e-20 are always cut off.
#' @param tlim Ignored (compatibility argument from mizer's generic).
#' @param size_axis Ignored (compatibility argument from mizer's generic).
#' @param total A boolean value that determines whether the total over all
#'   species in the system is plotted as well. Default is FALSE.
#' @param background Ignored (compatibility argument from mizer's generic).
#' @param frame_duration Duration of each animation frame in milliseconds.
#'   Default 500.
#' @param transition_duration Duration of the transition between frames in
#'   milliseconds. Default equals `frame_duration`.
#' @param easing The easing function for transitions. Default "linear".
#' @param resource Ignored (compatibility argument from mizer's generic).
#' @param ... Other arguments (currently unused).
#' @param resources Name or vector of names of the resources to be plotted. By
#'   default all resources are plotted.
#' @param time_range The time range to animate over. Either a vector of values
#'   or a vector of min and max time. Default is the entire time range of the
#'   simulation.
#' @param power The abundance is plotted as the number density times the weight
#'   raised to \code{power}. The default \code{power = 1} gives the biomass
#'   density, whereas \code{power = 2} gives the biomass density with respect
#'   to logarithmic size bins.
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
                               total = FALSE,
                               background = TRUE,
                               frame_duration = 500,
                               transition_duration = frame_duration,
                               easing = "linear",
                               resource = TRUE,
                               ...,
                               resources = NULL,
                               time_range,
                               power = 1) {
    sim <- x
    species <- valid_species_arg(sim, species)
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
    nf_res <- melt(NResource(sim)[time_elements,
                     resource_params(sim@params)$resource %in% resources,
                     , drop = FALSE]) %>%
        dplyr::rename(Spectra = resource)
    nf <- rbind(nf, nf_res)
    # Add total ----
    if (total) {
        # Calculate total community abundance
        fish_idx <- (length(sim@params@w_full) -
                         length(sim@params@w) + 1):length(sim@params@w_full)
        total_n <- apply(NResource(sim), MARGIN = c(1, 3), sum)
        total_n[, fish_idx] <- total_n[, fish_idx] +
            rowSums(aperm(sim@n, c(1, 3, 2)), dims = 2)
        nf_total <- melt(total_n[time_elements, , drop = FALSE])
        nf_total$sp <- "Total"
        nf <- rbind(nf, nf_total)
    }

    # Deal with power argument ----
    if (power %in% c(0, 1, 2)) {
        y_label <- c("Number density [1/g]", "Biomass density",
                     "Biomass density [g]")[power + 1]
    } else {
        y_label <- paste0("Number density * w^", power)
    }
    nf <- dplyr::mutate(nf, value = value * w^power)

    # Impose limits ----
    if (is.na(wlim[1])) wlim[1] <- min(sim@params@w) / 100
    if (is.na(wlim[2])) wlim[2] <- max(sim@params@w_full)
    if (is.na(ylim[1])) ylim[1] <- 10^-20
    if (is.na(ylim[2])) ylim[2] <- 10^20
    nf <- nf %>%
        dplyr::filter(value >= ylim[1],
               value <= ylim[2],
               w >= wlim[1],
               w <= wlim[2])

    nf %>%
        plotly::plot_ly() %>%
        plotly::add_lines(x = ~w, y = ~value,
                          color = ~Spectra, colors = sim@params@linecolour,
                          frame = ~time,
                          line = list(simplify = FALSE)) %>%
        plotly::layout(xaxis = list(type = "log", exponentformat = "power",
                                    title = "Size [g]"),
                       yaxis = list(type = "log", exponentformat = "power",
                                    title = y_label))
}

