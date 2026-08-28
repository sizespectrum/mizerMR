#' Multiple-resource methods for model rescaling, calibration and reporting
#'
#' S3 methods that make mizer's `scaleModel()`, `scaleRates()`, `setResource()`,
#' `tuneSteadyState()` and `summary()` aware of the multiple-resource component.
#' The base `MizerParams` methods only know about the single built-in resource,
#' which `setMultipleResources()` silences, so they would otherwise ignore (or,
#' for `scaleModel()` and `setResource()`, error on) the resources stored in the
#' `MR` component.
#'
#' @param params A \linkS4class{mizerMR} object.
#' @param factor The factor by which to rescale.
#' @param object A \linkS4class{mizerMR} object.
#' @param solver The solver to use, see [mizer::tuneSteadyState()].
#' @param effort The fishing effort to use throughout.
#' @param preserve Which of the reproduction parameters to preserve, see
#'   [mizer::tuneSteadyState()].
#' @param info_level Controls the amount of information the function reports
#'   about the choices it makes, see [mizer::default_info_level()].
#' @param ... Further arguments passed along the mizer method chain.
#'
#' @return For `scaleModel()`, `scaleRates()`, `setResource()` and
#'   `tuneSteadyState()`: the updated `mizerMR` object. For `summary()`: the
#'   object, invisibly.
#' @name params_methods
NULL

#' Rescale a multiple-resource model
#'
#' Extends [mizer::scaleModel()] so that the resource carrying capacities and
#' abundances of all resources are rescaled consistently. The base method scales
#' the fish spectra and the resource *abundances* (held in `initial_n_other`),
#' but not the resource *capacities* and `kappa` coefficients, which live in the
#' `MR` component's parameters. The resource replenishment *rate* is left
#' unchanged, exactly as the built-in resource rate is in the base method, so
#' that the steady state is preserved.
#'
#' @rdname params_methods
#' @importFrom mizer scaleModel
#' @export
scaleModel.mizerMR <- function(params, factor, ...) {
    assert_that(is.number(factor), factor > 0)

    # Scale the resource capacities and abundance coefficients that the base
    # scaleModel() does not know about. They live in the MR component's
    # parameters rather than in the cc_pp / resource_params slots.
    op <- params@other_params
    cap <- op[["MR"]]$capacity
    if (!is.null(cap)) {
        co <- comment(cap)
        cap <- cap * factor
        comment(cap) <- co
        op[["MR"]]$capacity <- cap
    }
    if (!is.null(op[["MR"]]$resource_params)) {
        op[["MR"]]$resource_params$kappa <-
            op[["MR"]]$resource_params$kappa * factor
    }
    params@other_params <- op

    # Delegate the fish scaling and the resource *abundance* scaling (held in
    # initial_n_other) to the base method. We coerce to the plain class, and
    # clear the extension chain so that the base method's internal
    # `validParams()` does not re-apply the mizerMR class (which would make
    # `initialNResource<-` dispatch back to the MR setter, expecting a
    # resource-by-size array rather than the base vector).
    ext <- params@extensions
    base <- methods::as(params, "MizerParams")
    base@extensions <- character()
    base <- mizer::scaleModel(base, factor = factor, ...)
    base@extensions <- ext
    mizer::coerceToExtensionClass(base)
}

#' Rescale the rates of a multiple-resource model
#'
#' Extends [mizer::scaleRates()] so that the resource replenishment rate of all
#' resources is rescaled by `factor` along with the consumer rates. Without this
#' the base method would scale only the silenced built-in resource rate, leaving
#' the active resource rates untouched and breaking the rescaling invariant
#' (resource replenishment keeping pace with the rescaled search volume).
#'
#' @rdname params_methods
#' @importFrom mizer scaleRates
#' @export
scaleRates.mizerMR <- function(params, factor, ...) {
    params <- NextMethod()
    rate <- params@other_params[["MR"]]$rate
    if (!is.null(rate)) {
        co <- comment(rate)
        rate <- rate * factor
        comment(rate) <- co
        params@other_params[["MR"]]$rate <- rate
    }
    mizer::coerceToExtensionClass(params)
}

#' Set the built-in resource of a multiple-resource model
#'
#' Extends [mizer::setResource()]. In a multiple-resource model the built-in
#' single resource is silenced and plays no part in the dynamics, so changing
#' it with `setResource()` has no effect on the simulation. This method warns
#' when the user tries to change the resource rate, capacity or level, and
#' directs them to [setMultipleResources()] and the related setters. Calls that
#' only change, for example, the built-in resource dynamics (as mizer does
#' internally) pass through silently. The work is delegated to the base method
#' on the plain class so that the base accessors return the (vector-valued)
#' built-in resource instead of the resource-by-size arrays of the MR component.
#'
#' @rdname params_methods
#' @importFrom mizer setResource
#' @export
setResource.mizerMR <- function(params, ...) {
    args <- list(...)
    touches <- intersect(c("resource_rate", "resource_capacity",
                           "resource_level"), names(args))
    if (length(touches) > 0 &&
        any(!vapply(args[touches], is.null, logical(1))) &&
        !is.null(getComponent(params, "MR"))) {
        # The user asked for something that is not going to happen, so this is
        # reported at severity "warning" and shown even when nothing is
        # collecting reports. See mizer::signal_info().
        mizer::signal_info(
            "resource",
            paste0("This is a multiple-resource model. `setResource()` ",
                   "changes only the silenced built-in resource and does not ",
                   "affect the dynamics. Use `setMultipleResources()`, ",
                   "`resource_rate<-`, `resource_capacity<-` or ",
                   "`resource_level<-` instead."),
            level = 1, severity = "warning", unhandled = "show")
    }
    ext <- params@extensions
    base <- methods::as(params, "MizerParams")
    base@extensions <- character()
    base <- do.call(mizer::setResource, c(list(base), args))
    base@extensions <- ext
    mizer::coerceToExtensionClass(base)
}

#' Tune a multiple-resource model to a steady state
#'
#' Extends [mizer::tuneSteadyState()] so that the multiple resources get the
#' treatment that mizer gives its single built-in resource: they are held at
#' their stored abundances while the consumer spectra are solved for, and
#' afterwards their capacities are rebalanced, with [balanceResources()], so
#' that those held abundances are a steady state of the resource dynamics under
#' the new spectra. The rates are preserved and the capacities derived from
#' them, which is the choice the base method makes for `cc_pp`.
#'
#' Without this the resources would be held fixed during the search and then
#' handed back with the parameters they came in with, so the model returned
#' would sit at a fixed point of the consumer dynamics but not of the resource
#' dynamics. mizer reports exactly that for the components it does not know how
#' to handle; because this method does handle the `MR` component, it takes it
#' out of that report by pinning it itself for the duration of the search, which
#' is what the base method does with every component anyway.
#'
#' The `"convergence"` attribute of the result is preserved. Its `residual`
#' entry covers the consumers only — mizer keeps components out of that
#' criterion — so it is not changed by the rebalancing. What the rebalancing
#' moves is `attr(getSteadyResidual(params), "other")`, the rate of change of
#' the resources themselves.
#'
#' @rdname params_methods
#' @importFrom mizer tuneSteadyState
#' @export
tuneSteadyState.mizerMR <- function(params, solver = c("project", "newton"),
                                    effort = params@initial_effort,
                                    preserve = c("reproduction_level",
                                                 "erepro", "R_max"),
                                    info_level = mizer::default_info_level(),
                                    ...) {
    if (is.null(getComponent(params, "MR"))) {
        return(NextMethod())
    }
    # Hold the resources at their stored abundances ourselves. The base method
    # pins every component this way in any case; doing it here as well says
    # that mizerMR has taken responsibility for this one, which keeps it out of
    # the report about components mizer cannot handle.
    mr_dynamics <- params@other_dynamics[["MR"]]
    params@other_dynamics[["MR"]] <- "constant_other"
    object <- NextMethod()

    # `setMultipleResources()` returns a fresh object that drops attributes.
    conv <- attr(object, "convergence")
    is_sim <- is(object, "MizerSim")
    tuned <- if (is_sim) object@params else object
    tuned@other_dynamics[["MR"]] <- mr_dynamics
    tuned <- setMultipleResources(tuned, balance = TRUE,
                                  info_level = info_level)
    if (is_sim) {
        object@params <- tuned
    } else {
        object <- tuned
    }
    if (!is.null(conv)) {
        attr(object, "convergence") <- conv
    }
    object
}

#' Summarise a multiple-resource model
#'
#' Extends the `summary()` method for `MizerParams` objects. The base method
#' reports the resource size spectrum from the silenced built-in resource, which
#' is empty for a multiple-resource model. This method instead reports the
#' overall size range spanned by all resources combined.
#'
#' @rdname params_methods
#' @export
summary.mizerMR <- function(object, ...) {
    n_mr <- object@initial_n_other[["MR"]]
    if (!is.null(n_mr)) {
        # Make the base summary report the combined extent of all resources by
        # temporarily presenting their total as the built-in resource.
        object@initial_n_pp[] <- colSums(n_mr)
    }
    NextMethod()
}
