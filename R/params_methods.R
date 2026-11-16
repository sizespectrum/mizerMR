#' Multiple-resource methods for model rescaling, calibration and reporting
#'
#' The MR component declares scaling hooks and tuning strategies to mizer.
#' These methods retain the resource setter and summary compatibility helpers;
#' scaling and tuning use the shared mizer implementations directly.
#'
#' @param params A \linkS4class{mizerMR} object.
#' @param object A \linkS4class{mizerMR} object.
#' @param ... Further arguments passed along the mizer method chain.
#'
#' @return For `setResource()`: the updated `mizerMR` object. For `summary()`:
#'   the object, invisibly.
#' @name params_methods
NULL

#' Set the built-in resource of a multiple-resource model
#'
#' Extends [mizer::setResource()]. In a multiple-resource model the built-in
#' single resource is silenced and plays no part in the dynamics, so changing
#' it with `setResource()` has no effect on the simulation. This method warns
#' when the user tries to change the resource rate, capacity or level, and
#' directs them to [setMultipleResources()] and the related setters. Calls that
#' only change, for example, the built-in resource dynamics (as mizer does
#' internally) pass through silently. The work is delegated to the base method
#' with only the MR layer removed so that the accessors return the
#' vector-valued built-in resource instead of the resource-by-size arrays of
#' the MR component.
#'
#' @rdname params_methods
#' @importFrom mizer setResource
#' @export
setResource.mizerMR <- function(params, ...) {
    args <- list(...)
    touches <- intersect(c("resource_rate", "resource_capacity",
                           "resource_level"), names(args))
    changes <- vapply(touches, function(field) {
        value <- args[[field]]
        if (is.null(value)) return(FALSE)
        current <- switch(field, resource_rate = params$rr_pp,
                          resource_capacity = params$cc_pp,
                          resource_level = params$initial_n_pp / params$cc_pp)
        if (!is.numeric(value) || !length(value) %in% c(1L, length(current))) {
            return(TRUE)
        }
        !isTRUE(all.equal(rep(as.numeric(value), length.out = length(current)),
                          as.numeric(current), check.attributes = FALSE))
    }, logical(1))
    if (any(changes) && !is.null(getComponent(params, "MR"))) {
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
            level = 1, severity = "warning", unhandled = "show"
        )
    }
    ext <- params@extensions
    if (isS4(params)) {
        base <- mizer:::upgrade_s4_to_s3(params)
    } else {
        base <- params
    }
    base <- mrCoreView(base)
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
