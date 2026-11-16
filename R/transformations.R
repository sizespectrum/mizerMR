#' Apply the MR-owned part of a model transformation
#' @param params The candidate with the new core species and grid layout.
#' @param before The original model before the operation.
#' @param operation The canonical transformation operation.
#' @param details Explicit species/grid maps or scaling factor from mizer.
#' @param ... Unused context.
#' @return The candidate with the MR state and parameters updated before any
#'   dependent rate calculations.
#' @keywords internal
#' @export
mrTransform <- function(params, before, operation, details, ...) {
    old <- before$other_params$MR
    state <- before$initial_n_other$MR
    if (operation == "scale_abundance") {
        params$initial_n_other$MR <- state * details$factor
        params$other_params$MR$capacity <- old$capacity * details$factor
        params$other_params$MR$resource_params$kappa <-
            old$resource_params$kappa * details$factor
        return(params)
    }
    if (operation == "scale_rates") {
        params$other_params$MR$rate <- old$rate * details$factor
        params$other_params$MR$resource_params$r_pp <-
            old$resource_params$r_pp * details$factor
        return(params)
    }
    # Species ownership is explicit: only rows of the resource interaction
    # matrix depend on species. No dimensions-based guesses are made.
    interaction <- matrix(
        1, nrow = length(details$new_species), ncol = ncol(old$interaction),
        dimnames = list(sp = details$new_species,
                        resource = colnames(old$interaction))
    )
    old_rows <- details$new_to_old
    keep <- !is.na(old_rows)
    interaction[keep, ] <- old$interaction[old_rows[keep], , drop = FALSE]
    if (!is.null(old$pending_interaction)) {
        request <- old$pending_interaction
        interaction[rownames(request), ] <- request[, colnames(interaction),
                                                    drop = FALSE]
    }
    params$other_params$MR$interaction <- interaction
    params$other_params$MR$pending_interaction <- NULL
    if (identical(details$old_w_full, details$new_w_full)) return(params)

    # Overlapping bins retain the author's actual arrays, including frozen
    # values. New bins use the resource's declared power laws; state starts at
    # its carrying capacity there. The maps are supplied by the core operation.
    mr <- params$other_params$MR
    resources <- mr$resource_params$resource
    shape <- list(resource = resources, w = names(params$initial_n_pp))
    template <- array(0, dim = c(length(resources), length(params$w_full)),
                      dimnames = shape)
    params$initial_n_other$MR <- template
    params$other_params$MR$rate <- template
    params$other_params$MR$capacity <- template
    arrays <- mrPowerLawArrays(params, mr$resource_params, template)
    rate <- arrays$rate
    capacity <- arrays$capacity
    state_new <- capacity
    overlap <- details$new_w_full_to_old
    keep <- !is.na(overlap)
    rate[, keep] <- old$rate[, overlap[keep], drop = FALSE]
    capacity[, keep] <- old$capacity[, overlap[keep], drop = FALSE]
    state_new[, keep] <- state[, overlap[keep], drop = FALSE]
    comment(rate) <- comment(old$rate)
    comment(capacity) <- comment(old$capacity)
    params$other_params$MR$rate <- rate
    params$other_params$MR$capacity <- capacity
    params$initial_n_other$MR <- state_new
    params
}

#' Keep the silenced core resource explicitly zero during transformations
#' @inheritParams mrTransform
#' @return The candidate with its prescribed core resource still zero.
#' @keywords internal
#' @export
mrTransformCoreResource <- function(params, ...) {
    params$initial_n_pp[] <- 0
    params
}
