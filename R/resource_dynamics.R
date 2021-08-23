#' @export
mizerMR_dynamics <- function(params, n_other, n_pp, ...) {
    n_res <- n_other[["MR"]]
    no_res <- dim(n_res)[[1]]
    rp <- params@resource_params
    new_n_res <- n_res
    for (i in seq_len(no_res)) {
        fn <- get0(rp$dynamics[[i]])
        new_n_res[i, ] <-
            fn(params, n_pp = n_res[i, ], n_other = n_other,
               resource_rate = params@other_params[["MR"]]$rate[i, ],
               resource_capacity = params@other_params[["MR"]]$rate[i, ],
               ...)
    }
    new_n_res
}
