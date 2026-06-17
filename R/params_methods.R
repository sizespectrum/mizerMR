#' Multiple-resource methods for model rescaling and reporting
#'
#' S3 methods that make mizer's `scaleModel()`, `scaleRates()`, `setResource()`
#' and `summary()` aware of the multiple-resource component. The base
#' `MizerParams` methods only know about the single built-in resource, which
#' `setMultipleResources()` silences, so they would otherwise ignore (or, for
#' `scaleModel()` and `setResource()`, error on) the resources stored in the
#' `MR` component.
#'
#' @param params A \linkS4class{mizerMR} object.
#' @param factor The factor by which to rescale.
#' @param object A \linkS4class{mizerMR} object.
#' @param ... Further arguments passed along the mizer method chain.
#'
#' @return For `scaleModel()`, `scaleRates()` and `setResource()`: the updated
#'   `mizerMR` object. For `summary()`: the object, invisibly.
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
    if (!is.null(op$other$MR$resource_params)) {
        op$other$MR$resource_params$kappa <-
            op$other$MR$resource_params$kappa * factor
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
        warning("This is a multiple-resource model. `setResource()` changes ",
                "only the silenced built-in resource and does not affect the ",
                "dynamics. Use `setMultipleResources()`, `resource_rate<-` or ",
                "`resource_capacity<-` instead.", call. = FALSE)
    }
    ext <- params@extensions
    base <- methods::as(params, "MizerParams")
    base@extensions <- character()
    base <- do.call(mizer::setResource, c(list(base), args))
    base@extensions <- ext
    mizer::coerceToExtensionClass(base)
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
