# Balancing the resources ------------------------------------------------
#
# mizer's `setResource()` sets the rate and the capacity of its single resource
# so that the resource replenishes at exactly the rate at which it is consumed
# and the current resource abundance is therefore a steady state of the
# resource dynamics. This file does the same for the several resources of a
# mizerMR model, one resource at a time and each with its own dynamics.

#' A single-resource view of one resource of a mizerMR model
#'
#' An S4 subclass of [mizer::MizerParams-class] that presents one resource of a
#' multiple-resource model in the way mizer presents its single built-in
#' resource: the `rr_pp`, `cc_pp` and `initial_n_pp` slots hold the
#' replenishment rate, the capacity and the abundance of that one resource, and
#' the extra `resource_mort` slot holds the mortality that this resource
#' experiences.
#'
#' The class exists so that the balancing functions mizer provides for its
#' resource dynamics ([mizer::balance_resource_semichemostat()] and
#' [mizer::balance_resource_logistic()]), and any that a user writes for their
#' own dynamics, can be used unchanged for each resource of a mizerMR model.
#' Those functions take a MizerParams object and call [mizer::getResourceMort()]
#' and [mizer::initialNResource()] on it.
#'
#' The mortality cannot be left to be recalculated from such a single-resource
#' object, because the predation mortality on one resource depends on the
#' feeding level of the predators and hence on *all* the resources together. So
#' the view carries the mortality that was calculated in the full
#' multiple-resource model and the `getResourceMort()` method for this class
#' simply hands it back.
#'
#' @slot resource_mort The predation mortality on the resource, as calculated in
#'   the full multiple-resource model.
#' @seealso [balanceResources()]
#' @keywords internal
setClass("mizerMRResourceView", contains = "MizerParams",
         slots = c(resource_mort = "numeric"))

#' Resource mortality of a single-resource view
#'
#' Returns the mortality that was stored in the view when it was created, see
#' [mizerMRResourceView-class].
#'
#' @param params A `mizerMRResourceView` object.
#' @param n Unused.
#' @param n_pp Unused.
#' @param n_other Unused.
#' @param t Unused.
#' @param ... Unused.
#' @return The stored mortality vector.
#' @keywords internal
#' @export
getResourceMort.mizerMRResourceView <- function(params, n = initialN(params),
                                                n_pp = params@initial_n_pp,
                                                n_other = initialNOther(params),
                                                t = 0, ...) {
    params@resource_mort
}

#' Make a single-resource view of one resource
#'
#' @param base A plain [mizer::MizerParams-class] object holding the model
#'   without its extension classes.
#' @param resource_mort The mortality experienced by the resource.
#' @param abundance The number density of the resource.
#' @param resource_rate The replenishment rate of the resource.
#' @param resource_capacity The carrying capacity of the resource.
#' @return A `mizerMRResourceView` object.
#' @keywords internal
mrResourceView <- function(base, resource_mort, abundance, resource_rate,
                           resource_capacity) {
    base@initial_n_pp[] <- abundance
    base@rr_pp[] <- resource_rate
    base@cc_pp[] <- resource_capacity
    comment(base@rr_pp) <- NULL
    comment(base@cc_pp) <- NULL
    methods::new("mizerMRResourceView", base,
                 resource_mort = as.numeric(resource_mort))
}

#' Balance the resources
#'
#' Calculates the resource parameters that make each resource replenish at
#' exactly the rate at which it is consumed, so that the current resource
#' abundances are a steady state of the resource dynamics. This is the
#' multiple-resource analogue of the balancing that [mizer::setResource()] does
#' for mizer's single built-in resource.
#'
#' You will usually not call this function yourself but let
#' [setMultipleResources()] call it for you, see the section "Balancing the
#' resources" there.
#'
#' Exactly one of `resource_rate` and `resource_capacity` should be given; the
#' other one is calculated from the requirement that replenishment balances
#' consumption. Whichever one you do not give is taken from the model, both for
#' the resources that are balanced and for those that are not.
#'
#' Each resource is balanced by the balancing function belonging to its own
#' dynamics function, which is the function whose name is obtained by prefixing
#' the entry in the `dynamics` column of [resource_params()] with `balance_`.
#' Resources whose dynamics has no such function are left unchanged, because
#' their dynamics has no steady state that could be arranged in this way. This
#' is for example the case for a resource held constant with
#' [mizer::resource_constant()], which is at steady state whatever its
#' parameters.
#'
#' The mortality that each resource experiences is calculated in the full
#' multiple-resource model with [mizer::getResourceMort()], because the
#' predation mortality on any one resource depends on the feeding level of the
#' predators and hence on all the resources together. Outside the size range of
#' a resource both its abundance and its capacity are zero, so there is nothing
#' to balance there and the consumption at those sizes is ignored.
#'
#' @param params A MizerParams object with multiple resources set up.
#' @param resource_rate Optional. An array (resource x size) with the
#'   replenishment rate for each resource.
#' @param resource_capacity Optional. An array (resource x size) with the
#'   carrying capacity for each resource.
#'
#' @return A list with entries `resource_rate` and `resource_capacity`, each an
#'   array (resource x size).
#' @seealso [setMultipleResources()],
#'   [mizer::balance_resource_semichemostat()]
#' @export
#' @family functions for setting parameters
balanceResources <- function(params, resource_rate = NULL,
                             resource_capacity = NULL) {
    mr <- getComponent(params, "MR")
    if (is.null(mr)) {
        stop("params does not have multiple resources set up.")
    }
    if (is.null(resource_rate) && is.null(resource_capacity)) {
        stop("Both `resource_capacity` and `resource_rate` were NULL.")
    }
    if (!is.null(resource_rate) && !is.null(resource_capacity)) {
        stop("You should only provide either the `resource_rate` or the `resource_capacity` because the other is determined by the requirement that the resources replenish at the same rate at which they are consumed.")
    }
    rp <- resource_params(params)
    # The arrays we work with. The one that was given replaces the one stored
    # in the model, the other one is the model's own and is what the balancing
    # falls back on where it is not determined.
    rate <- mr$component_params$rate
    capacity <- mr$component_params$capacity
    comment(rate) <- NULL
    comment(capacity) <- NULL
    if (!is.null(resource_rate)) {
        if (!identical(dim(as.array(resource_rate)), dim(rate))) {
            stop("`resource_rate` should be an array with dim ",
                 toString(dim(rate)), ".")
        }
        rate[] <- resource_rate
    }
    if (!is.null(resource_capacity)) {
        if (!identical(dim(as.array(resource_capacity)), dim(capacity))) {
            stop("`resource_capacity` should be an array with dim ",
                 toString(dim(capacity)), ".")
        }
        capacity[] <- resource_capacity
    }
    # The mortality has to be the one from the full multiple-resource model, so
    # the object needs its extension class for `getResourceMort()` to dispatch
    # to the mizerMR method.
    params <- mizer::coerceToExtensionClass(params)
    mort <- unclass(getResourceMort(params))
    NR <- unclass(mr$initial_value)
    if (!identical(dim(mort), dim(NR))) {
        stop("The resource mortality does not have one row for each resource. Is mizerMR registered as an extension of this model?")
    }

    # The view is built from the model without its extension classes so that
    # the mizer accessors called by the balancing functions see a single
    # resource rather than the multiple-resource arrays.
    base <- methods::as(params, "MizerParams")
    base@extensions <- character()

    unbalanced <- character()
    for (i in seq_len(nrow(rp))) {
        balance_fn <- mrBalanceFunction(rp$dynamics[[i]])
        if (!is.function(balance_fn)) {
            unbalanced <- c(unbalanced, rp$resource[[i]])
            next
        }
        # Where the resource neither exists nor has room to exist there is
        # nothing to balance, so the consumption there is ignored. Without this
        # the balancing would object to a rate of zero at sizes outside the
        # size range of the resource, where consumers do still feed on the
        # other resources.
        mu <- mort[i, ]
        mu[NR[i, ] == 0 & capacity[i, ] == 0] <- 0
        view <- mrResourceView(base, resource_mort = mu,
                               abundance = NR[i, ],
                               resource_rate = rate[i, ],
                               resource_capacity = capacity[i, ])
        balanced <- balance_fn(
            view,
            resource_rate = if (is.null(resource_rate)) NULL else rate[i, ],
            resource_capacity = if (is.null(resource_capacity)) NULL else
                capacity[i, ])
        rate[i, ] <- balanced$resource_rate
        capacity[i, ] <- balanced$resource_capacity
    }
    if (length(unbalanced) > 0) {
        mizer::signal_info(
            "balance",
            paste0("The following resources were not balanced because there ",
                   "is no balancing function for their dynamics: ",
                   toString(unbalanced), "."),
            level = 2, severity = "info", unhandled = "drop")
    }

    list(resource_rate = rate, resource_capacity = capacity)
}

#' Find the balancing function belonging to a resource dynamics function
#'
#' The balancing function is the one whose name is the name of the dynamics
#' function prefixed with `balance_`, following the convention of
#' [mizer::setResource()]. mizer's own balancing functions are looked up in the
#' mizer namespace as well, so that they are found even when mizer has only
#' been loaded and not attached.
#'
#' @param dynamics The name of the resource dynamics function.
#' @return The balancing function, or NULL if there is none.
#' @keywords internal
mrBalanceFunction <- function(dynamics) {
    fn <- get0(paste0("balance_", dynamics))
    if (!is.function(fn)) {
        fn <- get0(paste0("balance_", dynamics),
                   envir = asNamespace("mizer"))
    }
    if (is.function(fn)) fn else NULL
}

#' Convert a resource level to a resource capacity
#'
#' The resource level is the ratio of the current resource abundance to the
#' resource capacity, so this divides the abundance by the level. Where the
#' level is `NaN`, which is only allowed where the resource abundance vanishes,
#' the capacity is set to zero.
#'
#' @param resource_level A single number, a vector with one value for each
#'   resource, or an array (resource x size).
#' @param initial_resource An array (resource x size) with the current resource
#'   abundances.
#' @return An array (resource x size) with the resource capacities.
#' @keywords internal
mrCapacityFromLevel <- function(resource_level, initial_resource) {
    assert_that(is.numeric(resource_level))
    if (anyNA(initial_resource)) {
        stop("You can only set the `resource_level` for resources that already have initial abundances. Provide the `initial_resource` or the `resource_capacity` instead.")
    }
    no_res <- nrow(initial_resource)
    level <- initial_resource
    level[] <- NA_real_
    if (length(resource_level) == 1 ||
        (is.null(dim(resource_level)) && length(resource_level) == no_res)) {
        # A vector with one value per resource fills the array by column, so
        # each resource gets its value at every size.
        level[] <- resource_level
    } else if (identical(dim(resource_level), dim(initial_resource))) {
        level[] <- resource_level
    } else {
        stop("The `resource_level` should be a single number, a vector with one value for each of the ", no_res,
             " resources, or an array with dim ",
             paste(dim(initial_resource), collapse = ", "), ".")
    }
    NR <- unclass(initial_resource)
    # The resource level is allowed to be NaN only where the resource vanishes
    if (any(NR > 0 & is.nan(level))) {
        stop("The resource level must be defined everywhere where the current resource is non-vanishing.")
    }
    if (any(NR > 0 & (level <= 0 | level > 1))) {
        stop("The `resource_level` must always be greater than 0 and at most 1.")
    }
    capacity <- NR / level
    capacity[is.nan(level)] <- 0
    capacity
}
