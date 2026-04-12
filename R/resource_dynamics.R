#' @export
mizerMR_dynamics <- function(params, n_other, n_pp, rates, ...) {
    n_res <- n_other[["MR"]]
    no_res <- dim(n_res)[[1]]
    rp <- resource_params(params)
    rate <- params@other_params[["MR"]]$rate
    capacity <- params@other_params[["MR"]]$capacity
    interaction <- params@other_params[["MR"]]$interaction  # (no_sp x no_res)
    # Compute per-resource mortality from pred_rate: (no_res x no_w_full)
    resource_mort <- t(interaction) %*% rates$pred_rate
    new_n_res <- n_res
    rates_mod <- rates
    for (i in seq_len(no_res)) {
        rates_mod$resource_mort <- resource_mort[i, ]
        fn <- get0(rp$dynamics[[i]])
        new_n_res[i, ] <-
            fn(params, n_pp = n_res[i, ], n_other = n_other,
               rates = rates_mod,
               resource_rate = rate[i, ],
               resource_capacity = capacity[i, ],
               ...)
    }
    new_n_res
}

#' @export
mizerMRResourceMort <- function(params, n, n_pp, n_other, t, pred_rate, ...) {
    t(params@other_params[["MR"]]$interaction) %*% pred_rate
}

#' @rdname setMultipleResources
#' @export
getResourceMort.MRMizerParams <- function(params, ...) {
    mizerMRResourceMort(params,
                        n = initialN(params),
                        n_pp = mizer::initialNResource(params),
                        n_other = params@initial_n_other,
                        t = 0,
                        pred_rate = getPredRate(params))
}
