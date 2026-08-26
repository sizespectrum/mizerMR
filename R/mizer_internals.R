# Access to mizer's plotting machinery -----------------------------------------
#
# mizerMR's plot methods have to reproduce exactly the axis handling, labelling
# and limit logic that mizer applies to its own spectra, so that a
# multiple-resource plot can be read against a single-resource one and so that
# the arguments of mizer's plotting generics mean the same thing here. mizer
# keeps that machinery unexported, so we reach into its namespace rather than
# duplicating it, which would silently drift out of step at the next mizer
# release. All the helpers used here exist in mizer (>= 3.3.0), the version
# this package depends on.

#' Get an unexported function from the mizer namespace
#'
#' @param name The name of the mizer function.
#' @return The function.
#' @keywords internal
mizer_fn <- function(name) {
    get(name, envir = asNamespace("mizer"))
}

#' Resolve the `power`, `biomass` and `per_log_size` arguments
#'
#' Since mizer 3.3 the quantity shown by a spectrum plot is described by two
#' independent flags, `biomass` and `per_log_size`, with the power of the weight
#' multiplying the number density being their sum. `power` is still accepted and
#' is the only way to ask for a power that is not such a sum. This is mizer's
#' own resolution, so that mizerMR accepts and rejects exactly the same
#' combinations as mizer does.
#'
#' @param power The power of the weight multiplying the number density, or NULL.
#' @param biomass Whether to show a biomass density rather than a number
#'   density, or NULL.
#' @param per_log_size Whether to show a density with respect to logarithmic
#'   size, or NULL.
#'
#' @return A list with entries `power`, `biomass` and `per_log_size`.
#' @keywords internal
resolve_spectrum_power <- function(power = NULL, biomass = NULL,
                                   per_log_size = NULL) {
    mizer_fn("resolve_spectrum_power")(power = power, biomass = biomass,
                                       per_log_size = per_log_size)
}

#' Convert resource plotting data to the requested size axis
#'
#' mizer's axis conversion looks up the weight-length relationship of each
#' series by name among the species parameters and knows only a single resource,
#' called "Resource". The resources of mizerMR are not species and have no
#' `a`/`b` of their own, so they are converted with the weight-length
#' relationship mizer uses for its resource: the `a` and `b` entries of the
#' `resource_params` slot that mizer maintains for its built-in resource (not
#' mizerMR's per-resource table of the same name), or mizer's defaults of
#' \eqn{a = \pi/6}, \eqn{b = 3}. That is the same relationship with which mizer
#' converts the combined resource when it forms the total, so all the lines in a
#' plot stay consistent with each other.
#'
#' Rows whose spectrum is not named in `resources` are passed to mizer
#' unchanged and are therefore converted with their own species parameters.
#'
#' @param plot_dat A data frame of plotting data with a `w` column.
#' @param params A \linkS4class{mizerMR} object.
#' @param size_axis Either "w" or "l".
#' @param per_log_size Whether to show the values with respect to logarithmic
#'   size.
#' @param density_wrt The size measure the values are a density with respect to,
#'   or `NA` if they are not a density and so need no Jacobian. By default
#'   deduced from `per_log_size`, which is right for a size spectrum.
#' @param spectra_col The name of the column holding the name of each series.
#' @param value_col The name of the column holding the values.
#' @param resources The names of the series that are resources. By default all
#'   of them are.
#'
#' @return The converted data frame, with the `w` column replaced by an `l`
#'   column if a length axis was requested.
#' @keywords internal
mr_convert_density_axis <- function(plot_dat, params, size_axis,
                                    per_log_size = FALSE,
                                    density_wrt = NULL,
                                    spectra_col = "Spectra",
                                    value_col = "value",
                                    resources = NULL) {
    if (nrow(plot_dat) == 0) {
        return(plot_dat)
    }
    if (is.null(density_wrt)) {
        density_wrt <- mizer_fn("spectrum_density_wrt")(per_log_size)
    }
    spectra <- as.character(plot_dat[[spectra_col]])
    is_resource <- if (is.null(resources)) {
        rep(TRUE, length(spectra))
    } else {
        spectra %in% resources
    }
    # Hand the resource rows to mizer under the name it reserves for a
    # resource, so that they are converted with the resource weight-length
    # relationship, and restore their own names afterwards.
    plot_dat$.mr_spectra <- plot_dat[[spectra_col]]
    plot_dat[[spectra_col]] <- ifelse(is_resource, "Resource", spectra)
    plot_dat <- mizer_fn("convert_plot_density_axis")(
        plot_dat, params, size_axis, density_wrt = density_wrt,
        per_log_size = per_log_size,
        species_col = spectra_col, value_col = value_col)
    plot_dat[[spectra_col]] <- plot_dat$.mr_spectra
    plot_dat$.mr_spectra <- NULL
    plot_dat
}
