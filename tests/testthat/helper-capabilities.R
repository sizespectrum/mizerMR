# Small models for the capability and early-hook regression tests.
small_mr_model <- function(second_order_w = FALSE) {
    p <- mizer::newMultispeciesParams(
        mizer::NS_species_params[c(1, 4, 11), ], no_w = 20,
        second_order_w = second_order_w, info_level = 0
    )
    rp <- as.data.frame(p$resource_params)
    rp$kappa <- rp$kappa / 2
    rp <- rbind(rp, rp)
    rp$resource <- c("small", "large")
    rp$w_min <- min(p$w_full)
    rp$w_max <- rp$w_pp_cutoff
    state <- rbind(p$initial_n_pp / 2, p$initial_n_pp / 2)
    dimnames(state) <- NULL
    setMultipleResources(p, resource_params = rp, initial_resource = state,
                         info_level = 0)
}
