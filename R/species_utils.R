#' Add species while specifying their resource interactions
#'
#' The MR interaction request is passed to the early transformation hook before
#' any new-species rates are evaluated. Other structural operations use mizer's
#' shared implementation and the same MR-owned transformation declarations.
#'
#' @inheritParams mizer::addSpecies
#' @param resource_interaction Interactions of new species with each resource.
#'   A vector for one species or a matrix with named species and resource axes.
#'   Omitted values default to one.
#' @return The updated multiple-resource model.
#' @export
#' @name addSpecies
addSpecies.mizerMR <- function(params, species_params,
                               gear_params = data.frame(),
                               initial_effort, interaction, steady = TRUE,
                               info_level = 0, ...,
                               resource_interaction = NULL) {
    if (!is.null(resource_interaction)) {
        species <- as.character(species_params$species)
        resources <- params$other_params$MR$resource_params$resource
        request <- resource_interaction
        if (is.null(dim(request))) {
            if (length(request) != length(species) * length(resources)) {
                stop("resource_interaction must have dimensions ",
                     length(species), " x ", length(resources), ".")
            }
            request <- matrix(request, nrow = length(species))
        }
        expected <- c(length(species), length(resources))
        if (!is.numeric(request) || any(!is.finite(request)) ||
                any(request < 0) ||
                !identical(dim(request), expected)) {
            stop("resource_interaction must have dimensions ", length(species),
                 " x ", length(resources), " with nonnegative finite values.")
        }
        if (is.null(rownames(request))) rownames(request) <- species
        if (is.null(colnames(request))) colnames(request) <- resources
        if (!setequal(rownames(request), species) ||
                !setequal(colnames(request), resources)) {
            stop("Resource interaction names must match ",
                 "new species and resources.")
        }
        params$other_params$MR$pending_interaction <-
            request[species, resources, drop = FALSE]
    }
    NextMethod()
}


#' Whether second-order bin-averaging is switched on
#'
#' Internal helper reading the `bin_average` entry of the model's
#' `second_order_w` slot via mizer's accessor.
#'
#' @param params A [mizer::MizerParams-class] object.
#' @return `TRUE` or `FALSE`.
#' @keywords internal
mr_bin_average <- function(params) {
    isTRUE(second_order_w(params)[["bin_average"]])
}

#' Bin-average of a power law over a restricted size range
#'
#' Internal helper. Returns the exact average of \eqn{w^d} over each grid bin
#' \eqn{[w_j, w_{j+1}]}, restricted to the size range \eqn{[w_{min}, w_{max}]}
#' (the power law is taken to be zero outside it) and divided by the full bin
#' width \eqn{\Delta w_j}. A bin straddling either knife edge gets the partial
#' average over the in-range part of the bin; bins entirely outside the range
#' get zero. This is the same finite-volume convention mizer uses internally,
#' extended with a lower cutoff for mizerMR's per-resource size ranges.
#'
#' @param w Numeric vector of left bin edges \eqn{w_j}.
#' @param dw Numeric vector of bin widths \eqn{\Delta w_j} (same length as `w`).
#' @param d Single numeric exponent of the power law.
#' @param w_min,w_max Lower and upper cutoffs of the size range. Default to no
#'   cutoff (`-Inf` and `Inf`).
#' @return A numeric vector (same length as `w`) of the restricted bin averages.
#' @keywords internal
power_law_bin_average <- function(w, dw, d, w_min = -Inf, w_max = Inf) {
    w_right <- w + dw
    # In-range integration limits for each bin, clipped to [w_min, w_max] and
    # kept within the bin so that bins outside the range collapse to zero width.
    lo <- pmin(pmax(w, w_min), w_right)
    hi <- pmax(pmin(w_right, w_max), lo)
    if (d == -1) {
        log(hi / lo) / dw
    } else {
        (hi ^ (d + 1) - lo ^ (d + 1)) / ((d + 1) * dw)
    }
}
