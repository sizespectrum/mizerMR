mizerMRBaseResource <- function(params) {
    get("initialNResource.MizerParams", asNamespace("mizer"))(params)
}

#' @export
getEncounter.mizerMR <- function(params, n = initialN(params),
                                 n_pp = mizerMRBaseResource(params),
                                 n_other = initialNOther(params),
                                 t = 0, ...) {
    if (!is.null(dim(n_pp)) || length(n_pp) != length(params@initial_n_pp)) {
        n_pp <- mizerMRBaseResource(params)
    }
    get("getEncounter.MizerParams", asNamespace("mizer"))(
        params, n = n, n_pp = n_pp, n_other = n_other, t = t, ...)
}

#' @export
getPredRate.mizerMR <- function(params, n = initialN(params),
                                n_pp = mizerMRBaseResource(params),
                                n_other = initialNOther(params),
                                t = 0, ...) {
    get("getPredRate.MizerParams", asNamespace("mizer"))(
        params, n = n, n_pp = n_pp, n_other = n_other, t = t, ...)
}

#' @export
getResourceMort.mizerMR <- function(params, n = initialN(params),
                                    n_pp = mizerMRBaseResource(params),
                                    n_other = initialNOther(params),
                                    t = 0, ...) {
    get("getResourceMort.MizerParams", asNamespace("mizer"))(
        params, n = n, n_pp = n_pp, n_other = n_other, t = t, ...)
}

#' @export
getRates.mizerMR <- function(params, n = initialN(params),
                             n_pp = mizerMRBaseResource(params),
                             n_other = initialNOther(params),
                             effort, t = 0, ...) {
    args <- list(params = params, n = n, n_pp = n_pp, n_other = n_other,
                 t = t, ...)
    if (!missing(effort)) {
        args$effort <- effort
    }
    do.call(get("getRates.MizerParams", asNamespace("mizer")), args)
}

#' @export
projectEncounter.mizerMR <- function(params, n, n_pp, n_other, t = 0, ...) {
    encounter <- NextMethod()
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
        encounter <- encounter + NextMethod()
    }

    encounter
}

#' @export
mizerMREncounter <- function(params, n, n_pp, n_other, t = 0, ...) {
    encounter <- mizerEncounter(
        params, n = n, n_pp = n_pp, n_other = n_other, t = t, ...)
    if (is.null(n_other[["MR"]])) {
        return(encounter)
    }
    encounter + mizerMRResourceEncounter(params, n_other)
}

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
