#' Set up multiple resources
#'
#' Sets the parameters of the multiple size-structured resources: their
#' replenishment rates, their carrying capacities, the interaction of the
#' consumer species with them and their initial abundances.
#'
#' By default the rate and the capacity are calculated from the power-law
#' coefficients in the [resource_params()] data frame. If you give either a
#' `resource_rate`, a `resource_capacity` or a `resource_level`, the other one
#' is by default determined by the requirement that each resource replenishes at
#' the same rate at which it is consumed, see the section "Balancing the
#' resources" below. You should therefore only give one of them.
#'
#' If you provide the `resource_level` then that sets the `resource_capacity` to
#' the current resource abundance divided by the resource level. So in that case
#' you should not specify `resource_capacity` as well.
#'
#' A rate or capacity that you provide yourself is remembered: it is marked with
#' a comment and is from then on no longer recalculated from the resource
#' parameters. Use `reset = TRUE` to go back to the values calculated from the
#' resource parameters.
#'
#' @section Balancing the resources:
#'
#' You would usually set the resource dynamics only after having finished the
#' calibration of the steady state. Balancing then preserves that steady state:
#' the resource parameters are chosen so that each resource replenishes at
#' exactly the rate at which it is consumed and the current resource abundances
#' therefore do not change. Your choice of resource dynamics only affects the
#' dynamics around the steady state. The higher the resource rate or the lower
#' the resource capacity the less sensitive the model will be to changes in the
#' competition for the resources.
#'
#' Balancing happens by default whenever you give exactly one of
#' `resource_rate`, `resource_capacity` or `resource_level`, and then determines
#' the other one. Set `balance = FALSE` if you do not want that. Setting
#' `balance = TRUE` when you give none of them recalculates the capacities from
#' the current rates.
#'
#' Unlike in mizer, where [mizer::setResource()] only ever changes the resource,
#' this function is also the place where the resource interaction, the resource
#' parameters and the initial abundances are set, and those calls do not balance
#' by default.
#'
#' Each resource is balanced with the balancing function belonging to its own
#' dynamics, see [balanceResources()], which also describes what happens to
#' resources whose dynamics cannot be balanced.
#'
#' @param params A MizerParams object
#' @param object A mizerMR object
#' @param resource_params A data frame with the resource parameters
#' @param resource_interaction Optional interaction matrix between species and
#'   resources (predator species x prey resource). By default all entries are 1.
#' @param resource_capacity Optional. Array (resource x size) of the
#'   intrinsic resource carrying capacities
#' @param resource_rate Optional. Array (resource x size) of intrinsic
#'   resource growth rates
#' @param resource_level Optional. The ratio between the current resource
#'   abundance and the resource capacity. Either a single number, a vector with
#'   one value for each resource or an array (resource x size). Must be greater
#'   than 0 and at most 1, except at sizes where a resource vanishes, where it
#'   can be `NaN`. This determines the resource capacity, so do not specify both
#'   this and `resource_capacity`.
#' @param initial_resource Optional. Array (resource x size) of initial values
#' @param balance By default the resource parameters are set so that each
#'   resource replenishes at the same rate at which it is consumed whenever you
#'   supply exactly one of `resource_rate`, `resource_capacity` or
#'   `resource_level`. Set to FALSE if you do not want the balancing, or to TRUE
#'   to balance also when you supply none of them. See the section "Balancing
#'   the resources".
#' @param reset If set to TRUE, then the resource rate and capacity are reset to
#'   the values calculated from the resource parameters, even if they were
#'   previously overwritten with custom values. If set to FALSE (default) then
#'   such custom values are kept.
#' @param info_level Controls the amount of information the function reports
#'   about the choices it makes, see [mizer::default_info_level()].
#' @export
setMultipleResources <- function(params,
                                 resource_params = NULL,
                                 resource_interaction = NULL,
                                 resource_capacity = NULL,
                                 resource_rate = NULL,
                                 resource_level = NULL,
                                 initial_resource = NULL,
                                 balance = NULL,
                                 reset = FALSE,
                                 info_level = mizer::default_info_level()) {
    # Collect the reports raised here and in the mizer calls below so that the
    # user gets them together at the end of the call.
    mizer::with_info_level(info_level = info_level, {
        setMultipleResourcesInternal(
            params, resource_params = resource_params,
            resource_interaction = resource_interaction,
            resource_capacity = resource_capacity,
            resource_rate = resource_rate,
            resource_level = resource_level,
            initial_resource = initial_resource,
            balance = balance, reset = reset,
            info_level = info_level)
    })
}

#' @rdname setMultipleResources
#' @keywords internal
setMultipleResourcesInternal <- function(params,
                                         resource_params = NULL,
                                         resource_interaction = NULL,
                                         resource_capacity = NULL,
                                         resource_rate = NULL,
                                         resource_level = NULL,
                                         initial_resource = NULL,
                                         balance = NULL,
                                         reset = FALSE,
                                         info_level =
                                             mizer::default_info_level()) {
    params <- validParams(params, info_level = info_level)
    if (is.null(resource_params)) {
        resource_params <- resource_params(params)
    }
    rp <- validResourceParams(resource_params, w_full(params)[[1]])
    no_sp <- nrow(species_params(params))
    no_res <- nrow(rp)
    no_w_full <- length(w_full(params))

    # If there is no MR component yet then we need to create it. We'll
    # fill it in properly later
    creating <- is.null(getComponent(params, "MR"))
    if (creating) {
        # Set built-in mizer resource to 0
        mizer::initialNResource(params) <- 0
        # and keep it zero
        resource_dynamics(params) <- "resource_constant"

        # make empty parameters
        w_names <- names(mizer::initialNResource(params))
        r_names <- as.list(rp$resource)
        sp_names <- dimnames(initialN(params))[[1]]
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
                                    interaction = interaction_default,
                                    resource_params = rp))
    }

    assert_that(is.flag(reset))
    if (!is.null(resource_capacity) && !is.null(resource_level)) {
        stop("You should specify only either 'resource_level' or 'resource_capacity'.")
    }
    # Remember what the user asked for. Whichever of the rate and the capacity
    # is not given is the one that the balancing determines.
    resource_rate_user <- resource_rate
    resource_capacity_user <- resource_capacity %||% resource_level

    if (reset) {
        if (!is.null(resource_rate_user) || !is.null(resource_capacity_user)) {
            warning("Because you set `reset = TRUE`, the values you provided for `resource_capacity`, `resource_rate` or `resource_level` will be ignored and values will be calculated from the resource parameters.")
            resource_rate <- NULL
            resource_capacity <- NULL
            resource_level <- NULL
            resource_rate_user <- NULL
            resource_capacity_user <- NULL
        }
        # Without their comments the rate and the capacity are recalculated
        # from the resource parameters below.
        comment(params@other_params[["MR"]]$rate) <- NULL
        comment(params@other_params[["MR"]]$capacity) <- NULL
    }

    # The resource level is a statement about the current abundances, so those
    # have to be determined before the capacity that the level implies.
    initial_resource <- valid_initial_resource(params, initial_resource)
    if (!is.null(resource_level)) {
        resource_capacity <- mrCapacityFromLevel(resource_level,
                                                 initial_resource)
    }
    resource_capacity <-
        valid_resource_capacity(params, resource_params = rp,
                                resource_capacity = resource_capacity)
    resource_rate <-
        valid_resource_rate(params, resource_params = rp,
                            resource_rate = resource_rate)
    resource_interaction <- valid_resource_interaction(params,
                                                       resource_interaction)
    # If no initial resource set yet then set to resource capacity
    if (anyNA(initial_resource)) {
        initial_resource <- resource_capacity
    }

    # By default we balance only when the user gave exactly one of the rate,
    # the capacity or the level, because then the other one is what the
    # balancing is for. A newly created component has no steady state to
    # preserve: its abundances are set to the capacity.
    num_args_user <- (!is.null(resource_rate_user)) +
        (!is.null(resource_capacity_user))
    if (is.null(balance)) {
        balance <- !creating && num_args_user == 1
    }
    assert_that(is.flag(balance))
    if (balance && num_args_user > 1) {
        stop("You should only provide either the `resource_rate` or the `resource_capacity` (or `resource_level`) because the other is determined by the requirement that the resources replenish at the same rate at which they are consumed.")
    }

    colours <- rp$colour
    names(colours) <- rp$resource
    params <- setColours(params, colours)
    linetypes <- rep("solid", no_res)
    names(linetypes) <- rp$resource
    params <- setLinetypes(params, linetypes)

    params <- setComponent(
        params = params, component = "MR",
        initial_value = initial_resource,
        dynamics_fun =  "mizerMR_dynamics",
        component_params = list(rate = resource_rate,
                                capacity = resource_capacity,
                                interaction = resource_interaction,
                                resource_params = rp))

    # Record mizerMR in the object's extension chain. The version stamp is set
    # only when the component is first created (the object then conforms to the
    # installed mizerMR); ordinary modifications preserve the existing stamp so
    # that a pending upgrade is not masked. See mizer::recordExtension().
    if (creating) {
        params <- mizer::recordExtension(
            params, "mizerMR",
            version = as.character(utils::packageVersion("mizerMR")))
    } else {
        params <- mizer::recordExtension(params, "mizerMR")
    }
    # The balancing needs the mortality that the resources experience in the
    # model as it now is, so it has to come after the component has been set,
    # and after the object has been given its extension class, without which
    # `getResourceMort()` would not know about the multiple resources.
    params <- mizer::coerceToExtensionClass(params)
    if (balance) {
        params <- balanceComponent(params, derive_rate =
                                       !is.null(resource_capacity_user))
    }
    params
}

#' Replace the rate or the capacity by their balanced values
#'
#' Calls [balanceResources()] and stores the result in the MR component. The
#' derived array is marked with a comment so that it is not overwritten by the
#' values calculated from the resource parameters at the next call to
#' [setMultipleResources()].
#'
#' @param params A MizerParams object with multiple resources set up.
#' @param derive_rate If TRUE the rate is calculated from the capacity,
#'   otherwise the capacity is calculated from the rate.
#' @return The updated MizerParams object.
#' @keywords internal
balanceComponent <- function(params, derive_rate) {
    mr <- getComponent(params, "MR")
    balanced <- balanceResources(
        params,
        resource_rate = if (derive_rate) NULL else mr$component_params$rate,
        resource_capacity = if (derive_rate) mr$component_params$capacity
                            else NULL)
    if (derive_rate) {
        rate <- mr$component_params$rate
        rate[] <- balanced$resource_rate
        comment(rate) <- "set by balancing"
        params@other_params[["MR"]]$rate <- rate
    } else {
        capacity <- mr$component_params$capacity
        capacity[] <- balanced$resource_capacity
        comment(capacity) <- "set by balancing"
        params@other_params[["MR"]]$capacity <- capacity
    }
    params
}

#' @rdname setMultipleResources
#' @export
`resource_capacity` <- function(params) {
    mr <- getComponent(params, "MR")
    if (is.null(mr)) {
        return(mizer::resource_capacity(params))
    }
    mr$component_params$capacity
}

#' @rdname setMultipleResources
#' @param value Value to assign
#' @export
`resource_capacity<-` <- function(params, balance = NULL, value) {
    if (is.null(getComponent(params, "MR"))) {
        return(mizer::`resource_capacity<-`(params, balance = balance,
                                            value = value))
    }
    setMultipleResources(params, resource_capacity = value, balance = balance)
}

#' @rdname setMultipleResources
#' @export
`resource_rate` <- function(params) {
    mr <- getComponent(params, "MR")
    if (is.null(mr)) {
        return(mizer::resource_rate(params))
    }
    mr$component_params$rate
}

#' @rdname setMultipleResources
#' @export
`resource_rate<-` <- function(params, balance = NULL, value) {
    if (is.null(getComponent(params, "MR"))) {
        return(mizer::`resource_rate<-`(params, balance = balance,
                                        value = value))
    }
    setMultipleResources(params, resource_rate = value, balance = balance)
}

#' @rdname setMultipleResources
#' @export
`resource_level` <- function(params) {
    mr <- getComponent(params, "MR")
    if (is.null(mr)) {
        return(mizer::resource_level(params))
    }
    MRArrayResourceBySize(unclass(mr$initial_value) /
                              unclass(mr$component_params$capacity),
                          value_name = "Resource level", units = "",
                          type = "proportion", params = params)
}

#' @rdname setMultipleResources
#' @export
`resource_level<-` <- function(params, balance = NULL, value) {
    if (is.null(getComponent(params, "MR"))) {
        return(mizer::`resource_level<-`(params, balance = balance,
                                         value = value))
    }
    setMultipleResources(params, resource_level = value, balance = balance)
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
initialNResource.mizerMR <- function(object) {
    mr <- getComponent(object, "MR")
    if (is.null(mr)) {
        return(NextMethod())
    }
    MRArrayResourceBySize(mr$initial_value, value_name = "Number density",
                          units = "1/g", type = "density", params = object)
}

#' @rdname setMultipleResources
#' @export
`initialNResource<-.mizerMR` <- function(params, value) {
    if (is.null(getComponent(params, "MR"))) {
        return(NextMethod())
    }
    initial_resource <- value
    value <- 0 * mizerMRBaseResource(params)
    params <- NextMethod()
    setMultipleResources(params, initial_resource = initial_resource)
}


#' Return valid resource capacity array
#'
#' If `resource capacity` is given it is checked for validity. If it does not
#' have a comment, then it is given the comment "set manually". This is then
#' returned. If `resource capacity` is missing or NULL, but one was set by the
#' user and stored in `params` with a comment, then this is returned. Otherwise
#' a resource capacity is calculated from `resource_params`. If this is NULL to
#' it is taken from `params`.
#' @param params A MizerParams object
#' @param resource_params A data frame with the resource parameters
#' @param resource_capacity Array (resource x size) of the
#'   intrinsic resource carrying capacities
#'
#' @return An array (resource x size) with the resource capacities
#' @keywords internal
valid_resource_capacity <- function(params, resource_params = NULL,
                                    resource_capacity = NULL) {
    mr <- getComponent(params, "MR")
    if (is.null(mr)) {
        stop("params does not have multiple resources set up.")
    }
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

    # We need to calculate capacity from resource_params
    if (is.null(resource_params)) {
        resource_params <- resource_params(params)
    }
    rp <- resource_params
    resource_capacity <- mr$component_params$capacity
    resource_capacity[] <- 0
    bin_average <- mr_bin_average(params)
    wf <- w_full(params)
    dwf <- dw_full(params)
    # TODO: vectorise this
    no_res <- nrow(rp)
    for (i in seq_len(no_res)) {
        if (bin_average) {
            # Exact bin average of kappa * w^(-lambda) over each bin, restricted
            # to the resource size range, so the capacity is the finite-volume
            # cell average consumed by the bin-integrated encounter convolution.
            resource_capacity[i, ] <- rp$kappa[[i]] *
                power_law_bin_average(wf, dwf, -rp$lambda[[i]],
                                      w_min = rp$w_min[[i]],
                                      w_max = rp$w_max[[i]])
        } else {
            w_sel <- wf >= rp$w_min[[i]] & wf <= rp$w_max[[i]]
            resource_capacity[i, w_sel] <- rp$kappa[[i]] *
                wf[w_sel] ^ -rp$lambda[[i]]
        }
    }

    resource_capacity
}

#' Return valid resource rate array
#'
#' If `resource rate` is given it is checked for validity. If it does not
#' have a comment, then it is given the comment "set manually". This is then
#' returned. If `resource rate` is missing or NULL, but one was set by the
#' user and stored in `params` with a comment, then this is returned. Otherwise
#' a resource rate is calculated from `resource_params`. If this is NULL to
#' it is taken from `params`.
#' @param params A MizerParams object
#' @param resource_params A data frame with the resource parameters
#' @param resource_rate Array (resource x size) of the
#'   intrinsic resource replenishment rate
#'
#' @return An array (resource x size) with the resource capacities
#' @keywords internal
valid_resource_rate <- function(params, resource_params = NULL,
                                resource_rate = NULL) {
    mr <- getComponent(params, "MR")
    if (is.null(mr)) {
        stop("params does not have multiple resources set up.")
    }
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

    # We need to calculate capacity from resource_params
    if (is.null(resource_params)) {
        resource_params <- resource_params(params)
    }
    rp <- resource_params
    resource_rate <- mr$component_params$rate
    resource_rate[] <- 0
    bin_average <- mr_bin_average(params)
    wf <- w_full(params)
    dwf <- dw_full(params)
    # TODO: vectorise this
    no_res <- nrow(rp)
    for (i in seq_len(no_res)) {
        if (bin_average) {
            # Exact bin average of r_pp * w^(n-1) over each bin, restricted to
            # the resource size range, so the relaxation rate is consistent with
            # the finite-volume cell-average resource density.
            resource_rate[i, ] <- rp$r_pp[[i]] *
                power_law_bin_average(wf, dwf, rp$n[[i]] - 1,
                                      w_min = rp$w_min[[i]],
                                      w_max = rp$w_max[[i]])
        } else {
            w_sel <- wf >= rp$w_min[[i]] & wf <= rp$w_max[[i]]
            resource_rate[i, w_sel] <- rp$r_pp[[i]] *
                wf[w_sel] ^ (rp$n[[i]] - 1)
        }
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
#' @keywords internal
valid_resource_interaction <- function(params, resource_interaction = NULL) {
    mr <- getComponent(params, "MR")
    if (is.null(mr)) {
        stop("params does not have multiple resources set up.")
    }
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
#' @keywords internal
valid_initial_resource <- function(params, initial_resource = NULL) {
    mr <- getComponent(params, "MR")
    if (is.null(mr)) {
        stop("params does not have multiple resources set up.")
    }
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
            stop("The initial resource should be everywhere positive.")
        }
        return(initial_resource)
    }

    # Return the current initial value
    mr$initial_value
}
