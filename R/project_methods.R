#' Multiple-resource project methods
#'
#' Internal helpers and S3 methods used to make mizer's rate and projection
#' generics work with the multiple-resource component.
#'
#' @param params A \linkS4class{mizerMR} object.
#' @param n A matrix of species abundances (species x size).
#' @param n_pp A resource abundance vector, or an MR resource array that will be
#'   replaced by the silenced built-in resource vector before calling mizer's
#'   base method.
#' @param n_other A list of abundances for other dynamical components.
#' @param effort Fishing effort by gear.
#' @param t Current projection time.
#' @param ... Further arguments passed along the mizer method chain.
#'
#' @return The return value depends on the method called: a rate array, a rate
#'   list, or a resource abundance vector.
#' @keywords internal
#' @name project_methods
NULL

#' Return the silenced built-in resource vector.
#'
#' @rdname project_methods
mizerMRBaseResource <- function(params) {
    get("initialNResource.MizerParams", asNamespace("mizer"))(params)
}

#' Replace MR resource arrays before calling base mizer methods.
#'
#' @rdname project_methods
mizerMRValidBaseResource <- function(params, n_pp) {
    if (!is.null(dim(n_pp)) || length(n_pp) != length(params@initial_n_pp)) {
        return(mizerMRBaseResource(params))
    }
    n_pp
}

#' Extend [mizer::getEncounter()] for mizerMR params.
#'
#' @rdname project_methods
#' @export
getEncounter.mizerMR <- function(params, n = initialN(params),
                                 n_pp = mizerMRBaseResource(params),
                                 n_other = initialNOther(params),
                                 t = 0, ...) {
    n_pp <- mizerMRValidBaseResource(params, n_pp)
    NextMethod(n = n, n_pp = n_pp, n_other = n_other, t = t)
}

#' Extend [mizer::getPredRate()] for mizerMR params.
#'
#' @rdname project_methods
#' @export
getPredRate.mizerMR <- function(params, n = initialN(params),
                                n_pp = mizerMRBaseResource(params),
                                n_other = initialNOther(params),
                                t = 0, ...) {
    n_pp <- mizerMRValidBaseResource(params, n_pp)
    NextMethod(n = n, n_pp = n_pp, n_other = n_other, t = t)
}

#' Extend [mizer::getResourceMort()] for mizerMR params.
#'
#' @rdname project_methods
#' @export
getResourceMort.mizerMR <- function(params, n = initialN(params),
                                    n_pp = mizerMRBaseResource(params),
                                    n_other = initialNOther(params),
                                    t = 0, ...) {
    n_pp <- mizerMRValidBaseResource(params, n_pp)
    NextMethod(n = n, n_pp = n_pp, n_other = n_other, t = t)
}

#' Extend [mizer::getRates()] for mizerMR params.
#'
#' @rdname project_methods
#' @export
getRates.mizerMR <- function(params, n = initialN(params),
                             n_pp = mizerMRBaseResource(params),
                             n_other = initialNOther(params),
                             effort, t = 0, ...) {
    n_pp <- mizerMRValidBaseResource(params, n_pp)
    if (missing(effort)) {
        NextMethod(n = n, n_pp = n_pp, n_other = n_other, t = t)
    } else {
        NextMethod(n = n, n_pp = n_pp, n_other = n_other, effort = effort,
                   t = t)
    }
}

#' Add MR resource encounter rates during projection.
#'
#' @rdname project_methods
#' @export
projectEncounter.mizerMR <- function(params, n, n_pp, n_other, t = 0, ...) {
    n_pp <- mizerMRValidBaseResource(params, n_pp)
    encounter <- NextMethod(n = n, n_pp = n_pp, n_other = n_other, t = t)
    n_mr <- n_other[["MR"]]
    if (is.null(n_mr)) {
        return(encounter)
    }

    zero_n <- n
    zero_n[] <- 0
    interaction_resource <- params@species_params$interaction_resource
    other_encounter <- params@other_encounter
    ext_encounter <- params@ext_encounter
    on.exit({
        params@species_params$interaction_resource <- interaction_resource
        params@other_encounter <- other_encounter
        params@ext_encounter <- ext_encounter
    })

    params@other_encounter <- list()
    params@ext_encounter[] <- 0
    for (resource in seq_len(nrow(n_mr))) {
        params@species_params$interaction_resource <-
            params@other_params[["MR"]]$interaction[, resource]
        n <- zero_n
        n_pp <- n_mr[resource, ]
        encounter <- encounter + NextMethod(n = n, n_pp = n_pp,
                                            n_other = n_other, t = t)
    }

    encounter
}

#' Compatibility wrapper for the MR encounter contribution.
#'
#' @rdname project_methods
#' @export
mizerMREncounter <- function(params, n, n_pp, n_other, t = 0, ...) {
    encounter <- mizerEncounter(
        params, n = n, n_pp = n_pp, n_other = n_other, t = t, ...)
    if (is.null(n_other[["MR"]])) {
        return(encounter)
    }
    encounter + mizerMRResourceEncounter(params, n_other)
}

#' Calculate the MR resource encounter contribution.
#'
#' @rdname project_methods
mizerMRResourceEncounter <- function(params, n_other) {
    idx_sp <- (length(params@w_full) - length(params@w) + 1):length(params@w_full)
    prey <- params@other_params[["MR"]]$interaction %*% n_other[["MR"]]
    prey <- sweep(prey, 2, params@w_full * params@dw_full, "*")
    avail_energy <- Re(base::t(mvfft(base::t(params@ft_pred_kernel_e) *
                                         mvfft(base::t(prey)),
                                     inverse = TRUE))) / length(params@w_full)
    avail_energy <- avail_energy[, idx_sp, drop = FALSE]
    avail_energy[avail_energy < 1e-18] <- 0

    encounter <- params@search_vol * avail_energy

    return(encounter)
}

#' Multiple-resource predation mortality
#'
#' Internal projection hook for predation mortality on multiple resources.
#'
#' @keywords internal
#' @export
#' @rdname project_methods
projectResourceMort.mizerMR <- function(params, n, n_pp, n_other, t = 0,
                                        pred_rate, ...) {
    resource_mort <- NextMethod()
    interaction <- params@other_params[["MR"]]$interaction
    if (is.null(interaction)) {
        return(resource_mort)
    }
    t(interaction) %*% pred_rate
}
