#' Set up multiple resources
#'
#' @param params A MizerParams object
#' @param resource_params A data frame with the resource parameters
#' @param resource_interaction Optional interaction matrix between species and
#'   resources (predator species x prey resource). Entries should be numbers
#'   between 0 and 1. By default all entries are 1.
#' @param resource_capacity Optional. Array (resource x size) of the
#'   intrinsic resource carrying capacities
#' @param resource_rate Optional. Array (resource x size) of intrinsic
#'   resource growth rates
#' @param initial_resource Optional. Array (resource x size) of initial values
#' @export
setMultipleResources <- function(params,
                                 resource_params = params@resource_params,
                                 resource_interaction = NULL,
                                 resource_capacity = NULL,
                                 resource_rate = NULL,
                                 initial_resource = resource_capacity) {
    params <- validParams(params)
    rp <- validResourceParams(params@resource_params)
    params@resource_params <- rp

    resource_capacity <- valid_resource_capacity(params, resource_capacity)
    resource_rate <- valid_resource_rate(params, resource_rate)
    resource_interaction <- valid_resource_interaction(params,
                                                       resource_interaction)

    if (!"MR" %in% names(params@n_other)) {
        # Still need to set this up
        if (is.null(resource_capacity)) {
            resource_capacity <- default_resource_capacity(params)
        }
        if (is.null(resource_rate)) {
            resource_capacity <- default_resource_rate(params)
        }
        if (is.null(resource_interaction)) {
            resource_interaction <- default_resource_interaction(params)
        }
        params <- setRateFunction(params, "Encounter", "mizerMR_encounter")
        params <- setComponent(
            params = params, component = "MR",
            initial_value = resource_capacity,
            dynamics_fun =  "mizerNR_dynamics",
            component_params = list(rate = resource_rate,
                                    capacity = resource_capacity,
                                    interaction = resource_interaction))
    }

}
#' Create a new multi-resource, multi-species model
#'
#' @param species_params A data frame with the species parameters, with one
#'   row for each species.
#' @param interaction Optional interaction matrix between species (predator
#'   species x prey species). Entries should be numbers between 0 and 1. By
#'   default all entries are 1.
#' @inheritParams setMultipleResources
#' @param gear_params A data frame with the gear parameters, with one
#'   row for each gear-species pair.
#' @param no_w The number of size bins in the consumer spectrum.
#' @param min_w Sets the size of the eggs of all species for which this is not
#'   given in the `w_min` column of the `species_params` dataframe.
#' @param max_w The largest size of the consumer spectrum. By default this is
#'   set to the largest `w_inf` specified in the `species_params` data
#'   frame.
#'
#' @return An object of class MizerMRParams
#' @export
newMRparams <- function(species_params,
                        interaction = NULL,
                        resource_params,
                        resource_interaction = NULL,
                        gear_params = data.frame(),
                        no_w = 100,
                        min_w = 0.001,
                        max_w = NA) {
    resource_params <- validResourceParams(resource_params)

    params <- newMultispeciesParams(
        species_params = species_params,
        gear_params = gear_params,
        interaction = interaction,
        no_w = no_w, min_w = min_w, max_w = max_w)

    params@resource_params <- resource_params
    params <- setMR(params, resource_interaction)

}



#' Return valid resource capacity array
#'
#' If `resource capacity` is given it is checked for validity. If it does not
#' have a comment, then it is given the comment "set manually". This is then
#' returned. If `resource capacity` is missing or NULL, but one was set by the
#' user and stored in `params` with a comment, then this is returned. Otherwise
#' a resource capacity is calculated from the resource params in `params`.
#' @param params A MizerParams object
#' @param resource_capacity
#'
#' @return An array (resource x size) with the resource capacities
valid_resource_capacity <- function(params, resource_capacity = NULL) {
    if (!"MR" %in% names(params@initial_n_other)) {
        stop("params does not have multiple resources set up.")
    }
    mr <- getComponent(params, "MR")
    if (!is.null(resource_capacity)) {
        if (!identical(dim(resource_capacity),
                       dim(mr$component_params$capacity))) {
            stop("`resource_capacity` should be an array with dim ",
                 dim(resource_capacity))
        }
        if (!is.null(dimnames(resource_capacity)) &&
            !identical(dimnames(resource_capacity),
                       dimnames(mr$component_params$capacity))) {
            stop("`resource_capacity` has wrong dimnames.")
        }
        dimnames(resource_capacity) <- dimnames(mr$component_params$capacity)
        if (any(resource_capacity < 0)) {
            stop("The resource capacities should be everywhere positive.")
        }
        if (is.null(comment(resource_capacity))) {
            comment(resource_capacity) <- "set manually"
        }
        return(resource_capacity)
    }

    if (!is.null(comment(mr$component_params$capacity))) {
        return(mr$component_params$capacity)
    }

    resource_capacity <- mr$component_params$capacity
    resource_capacity[] <- 0
    # TODO: vectorise this
    rp <- params@resource_params
    no_r <- nrow(rp)
    for (i in seq_len(no_r)) {
        w_sel <- params@w_full >= rp$w_min & params@w_full <= rp$w_max
        resource_capacity[i, w_sel] <- rp$kappa[[i]] *
            params@w_full[w_sel] ^ -rp$lambda[[i]]
    }

    resource_capacity
}

#' Return valid resource rate array
#'
#' If `resource rate` is given it is checked for validity. If it does not
#' have a comment, then it is given the comment "set manually". This is then
#' returned. If `resource rate` is missing or NULL, but one was set by the
#' user and stored in `params` with a comment, then this is returned. Otherwise
#' a resource rate is calculated from the resource params in `params`.
#' @param params A MizerParams object
#' @param resource_rate
#'
#' @return An array (resource x size) with the resource capacities
valid_resource_rate <- function(params, resource_rate = NULL) {
    if (!"MR" %in% names(params@initial_n_other)) {
        stop("params does not have multiple resources set up.")
    }
    mr <- getComponent(params, "MR")
    if (!is.null(resource_rate)) {
        if (!identical(dim(resource_rate),
                       dim(mr$component_params$rate))) {
            stop("`resource_rate` should be an array with dim ",
                 dim(resource_rate))
        }
        if (!is.null(dimnames(resource_rate)) &&
            !identical(dimnames(resource_rate),
                       dimnames(mr$component_params$rate))) {
            stop("`resource_rate` has wrong dimnames.")
        }
        dimnames(resource_rate) <- dimnames(mr$component_params$rate)
        if (any(resource_rate < 0)) {
            stop("The resource rate should be everywhere positive.")
        }
        if (is.null(comment(resource_rate))) {
            comment(resource_rate) <- "set manually"
        }
        return(resource_rate)
    }

    if (!is.null(comment(mr$component_params$rate))) {
        return(mr$component_params$rate)
    }

    resource_rate <- mr$component_params$rate
    resource_rate[] <- 0
    # TODO: vectorise this
    rp <- params@resource_params
    no_r <- nrow(rp)
    for (i in seq_len(no_r)) {
        w_sel <- params@w_full >= rp$w_min & params@w_full <= rp$w_max
        resource_rate[i, w_sel] <- rp$r_pp[[i]] *
            params@w_full[w_sel] ^ (params@species_params$n[[i]] - 1)
    }

    resource_rate
}


#' Return valid resource interaction array
#'
#' If `resource interaction` is given it is checked for validity. Otherwise
#' a matrix full of ones is returned.
#' @param params A MizerParams object
#' @param resource_interaction
#'
#' @return An array (resource x size)
valid_resource_interaction <- function(params, resource_interaction = NULL) {
    if (!"MR" %in% names(params@initial_n_other)) {
        stop("params does not have multiple resources set up.")
    }
    mr <- getComponent(params, "MR")
    if (!is.null(resource_interaction)) {
        if (!identical(dim(resource_interaction),
                       dim(mr$component_params$interaction))) {
            stop("`resource_interaction` should be an array with dim ",
                 dim(resource_interaction))
        }
        if (!is.null(dimnames(resource_interaction)) &&
            !identical(dimnames(resource_interaction),
                       dimnames(mr$component_params$interaction))) {
            stop("`resource_interaction` has wrong dimnames.")
        }
        dimnames(resource_interaction) <- dimnames(mr$component_params$interaction)
        if (any(resource_interaction < 0)) {
            stop("The resource interaction should be everywhere positive.")
        }
        return(resource_interaction)
    }

    mr$component_params$interaction
}
