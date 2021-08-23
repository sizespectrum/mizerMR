#' @export
mizerMR_dynamics <- function(params, n_other, n_pp, rates, ...) {
    n_res <- n_other[["MR"]]
    no_res <- dim(n_res)[[1]]
    rp <- params@resource_params
    new_n_res <- n_res
    rates_mod <- rates
    for (i in seq_len(no_res)) {
        rates_mod$resource_mort <- rates$resource_mort[i, ]
        fn <- get0(rp$dynamics[[i]])
        new_n_res[i, ] <-
            fn(params, n_pp = n_res[i, ], n_other = n_other,
               rates = rates_mod,
               resource_rate = params@other_params[["MR"]]$rate[i, ],
               resource_capacity = params@other_params[["MR"]]$capacity[i, ],
               ...)
    }
    new_n_res
}

#' @export
mizerMRResourceMort <- function(params, n, n_pp, n_other, t, pred_rate, ...) {
    t(params@other_params[["MR"]]$interaction) %*% pred_rate
}
