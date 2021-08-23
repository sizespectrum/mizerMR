#' Set up multiple resources
#'
#' @param params A MizerParams object
#' @param resource_params A data frame with the resource parameters
#' @param resource_interaction Optional interaction matrix between species and
#'   resources (predator species x prey resource). By default all entries are 1.
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
    rp <- validResourceParams(resource_params, params@w_full[[1]])
    params@resource_params <- rp

    if (!"MR" %in% names(params@initial_n_other)) {
        # Still need to set this up
        params@resource_dynamics <- "resource_constant"
        params <- setRateFunction(params, "Encounter", "mizerMREncounter")
        params <- setRateFunction(params, "ResourceMort", "mizerMRResourceMort")
        # make empty parameters
        no_sp <- nrow(params@species_params)
        no_res <- nrow(rp)
        no_w_full <- length(params@w_full)
        w_names <- names(params@initial_n_pp)
        r_names <- as.list(rp$resource)
        sp_names <- dimnames(params@initial_n)[[1]]
        template <- array(dim = c(no_res, no_w_full),
                          dimnames = list(resource = r_names, w = w_names))
        interaction_default <-
            array(1, dim = c(no_sp, no_res),
                  dimnames = list(sp = sp_names, resource = r_names))
        params <- setComponent(
            params = params, component = "MR",
            initial_value = template,
            dynamics_fun =  "mizerMR_dynamics",
            component_params = list(rate = template,
                                    capacity = template,
                                    interaction = interaction_default))
    }

    resource_capacity <- valid_resource_capacity(params, resource_capacity)
    resource_rate <- valid_resource_rate(params, resource_rate)
    resource_interaction <- valid_resource_interaction(params,
                                                       resource_interaction)
    initial_resource <- valid_initial_resource(params, initial_resource)
    params <- setComponent(
        params = params, component = "MR",
        initial_value = initial_resource,
        dynamics_fun =  "mizerMR_dynamics",
        component_params = list(rate = resource_rate,
                                capacity = resource_capacity,
                                interaction = resource_interaction))
}

#' @rdname setMultipleResources
#' @export
`resource_capacity` <- function(params) {
    getComponent(params, "MR")$component_params$capacity
}

#' @rdname setMultipleResources
#' @param value Value to assign
#' @export
`resource_capacity<-` <- function(params, value) {
    setMultipleResources(params, resource_capacity = value)
}

#' @rdname setMultipleResources
#' @export
`resource_rate` <- function(params) {
    getComponent(params, "MR")$component_params$rate
}

#' @rdname setMultipleResources
#' @export
`resource_rate<-` <- function(params, value) {
    setMultipleResources(params, resource_rate = value)
}

#' @rdname setMultipleResources
#' @export
`resource_interaction` <- function(params) {
    getComponent(params, "MR")$component_params$interaction
}

#' @rdname setMultipleResources
#' @export
`resource_interaction<-` <- function(params, value) {
    setMultipleResources(params, resource_interaction = value)
}

#' @rdname setMultipleResources
#' @export
`initialNResource` <- function(params) {
    getComponent(params, "MR")$initial_value
}

#' @rdname setMultipleResources
#' @export
`initialNResource<-` <- function(params, value) {
    setMultipleResources(params, initial_resource = value)
}


#' Return valid resource capacity array
#'
#' If `resource capacity` is given it is checked for validity. If it does not
#' have a comment, then it is given the comment "set manually". This is then
#' returned. If `resource capacity` is missing or NULL, but one was set by the
#' user and stored in `params` with a comment, then this is returned. Otherwise
#' a resource capacity is calculated from the resource params in `params`.
#' @param params A MizerParams object
#' @param resource_capacity Array (resource x size) of the
#'   intrinsic resource carrying capacities
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
                 paste(dim(mr$component_params$capacity), collapse = ", "))
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
    no_res <- nrow(rp)
    for (i in seq_len(no_res)) {
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
#' @param resource_rate Array (resource x size) of the
#'   intrinsic resource replenishment rate
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
                 paste(dim(mr$component_params$rate), collapse = ", "))
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
    no_res <- nrow(rp)
    for (i in seq_len(no_res)) {
        w_sel <- params@w_full >= rp$w_min & params@w_full <= rp$w_max
        resource_rate[i, w_sel] <- rp$r_pp[[i]] *
            params@w_full[w_sel] ^ (params@species_params$n[[i]] - 1)
    }

    resource_rate
}


#' Return valid resource interaction array
#'
#' If `resource interaction` is given it is checked for validity and returned.
#' Otherwise the value stored in `params` is returned.
#' @param params A MizerParams object
#' @param resource_interaction Interaction matrix between species and
#'   resources (predator species x prey resource). By default all entries are 1.
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
                 paste(dim(mr$component_params$interaction), collapse = ", "))
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


#' Return valid initial resource array
#'
#' If `initial_resource` is given it is checked for validity and returned.
#' Otherwise the value stored in `params` is returned.
#' @param params A MizerParams object
#' @param initial_resource Array (resource x size) of initial values
#'
#' @return An array (resource x size)
valid_initial_resource <- function(params, initial_resource = NULL) {
    if (!"MR" %in% names(params@initial_n_other)) {
        stop("params does not have multiple resources set up.")
    }
    mr <- getComponent(params, "MR")
    if (!is.null(initial_resource)) {
        if (!identical(dim(initial_resource),
                       dim(mr$initial_value))) {
            stop("`initial_resource` should be an array with dim ",
                 paste(dim(mr$initial_value), collapse = ", "))
        }
        if (!is.null(dimnames(initial_resource)) &&
            !identical(dimnames(initial_resource),
                       dimnames(mr$initial_value))) {
            stop("`initial_resource` has wrong dimnames.")
        }
        dimnames(initial_resource) <- dimnames(mr$initial_value)
        if (any(initial_resource < 0)) {
            stop("The resource interaction should be everywhere positive.")
        }
        return(initial_resource)
    }

    mr$initial_value
}
