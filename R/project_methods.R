#' Project an MRMizerParams model forward in time
#'
#' S3 method for [project()] when called with an \linkS4class{MRMizerParams}
#' object. Delegates all work to mizer's standard `project()` and wraps the
#' result as an \linkS4class{MRMizerSim} so that mizerMR's accessor and
#' plotting methods are dispatched automatically on the simulation output.
#'
#' @param params An \linkS4class{MRMizerParams} object.
#' @param ... Further arguments passed to [mizer::project()].
#' @return An object of class \linkS4class{MRMizerSim}.
#' @export
project.MRMizerParams <- function(params, ...) {
    sim <- NextMethod()
    new("MRMizerSim", sim)
}

#' @rdname setMultipleResources
#' @export
getEncounter.MRMizerParams <- function(params, n = initialN(params),
                                       n_pp = params@initial_n_pp,
                                       n_other = initialNOther(params),
                                       t = 0, ...) {
    # Always pass params@initial_n_pp (the zeroed built-in resource) as n_pp,
    # even if the caller passed initialNResource(params) (which for MRMizerParams
    # returns the MR matrix and would fail mizer's length assertion).
    getEncounter(as(params, "MizerParams"),
                 n = n, n_pp = params@initial_n_pp,
                 n_other = n_other, t = t, ...)
}

#' @export
mizerMREncounter <- function(params, n, n_pp, n_other, ...) {
    idx_sp <- (length(params@w_full) - length(params@w) + 1):length(params@w_full)
    prey <- params@other_params[["MR"]]$interaction %*% n_other[["MR"]]
    prey[, idx_sp] <- prey[, idx_sp] + params@interaction %*% n
    prey <- sweep(prey, 2, params@w_full * params@dw_full, "*")
    avail_energy <- Re(base::t(mvfft(base::t(params@ft_pred_kernel_e) *
                                         mvfft(base::t(prey)),
                                     inverse = TRUE))) / length(params@w_full)
    avail_energy <- avail_energy[, idx_sp, drop = FALSE]
    avail_energy[avail_energy < 1e-18] <- 0

    encounter <- params@search_vol * avail_energy

    # Add contributions from other components
    for (i in seq_along(params@other_encounter)) {
        encounter <- encounter +
            do.call(params@other_encounter[[i]],
                    list(params = params,
                         n = n, n_pp = n_pp, n_other = n_other,
                         component = names(params@other_encounter)[[i]], ...))
    }
    return(encounter)
}
