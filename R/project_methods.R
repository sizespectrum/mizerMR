
mizerMR_encounter <- function(params, n, n_pp, n_other, ...) {
    idx_sp <- (length(params@w_full) - length(params@w) + 1):length(params@w_full)
    prey <- params@interaction_resource %*% n_other$resources
    prey[, idx_sp] <- prey[, idx_sp] + params@interaction %*% n
    prey <- sweep(prey, 2, params@w_full * params@dw_full, "*")
    avail_energy <- Re(base::t(mvfft(base::t(params@ft_pred_kernel_e) *
                                         mvfft(base::t(prey)),
                                     inverse = TRUE))) / length(params@w_full)
    avail_energy <- avail_energy[, idx_sp, drop = FALSE]
    avail_energy[avail_energy < 1e-18] <- 0

    params@search_vol * avail_energy
}
