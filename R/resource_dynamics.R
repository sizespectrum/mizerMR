mizerMR_dynamics <- function(params, n_other, rates, dt, ...) {

}


# Have to think about this: do we want to allow different dynamics functions
# for different resources?
semichemostat <- function(params, n_other, rates, dt, component,
                             ...) {
    c <- params@other_params[[component]]
    # name of interaction parameter for this component in species_params
    interaction_component <- paste0("interaction_", component)
    interaction <- params@species_params[[interaction_component]]
    mort <- as.vector(interaction  %*% rates$pred_rate)
    tmp <- c$rate * c$capacity / (c$rate + mort)
    return(tmp - (tmp - n_other[[component]]) * exp(-(c$rate + mort) * dt))
}
