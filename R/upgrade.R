#' Upgrade a mizerMR object from an earlier version of mizerMR
#'
#' This is the `mizerMR` method of the `utils::upgrade()` generic (on which
#' mizer registers its own methods). It performs only the mizerMR-specific
#' migration and is invoked by mizer's
#' upgrade orchestrator (from `validParams()` / `readParams()`) when the version
#' stamp recorded in `params@extensions[["mizerMR"]]` is older than the installed
#' mizerMR, or is missing. The orchestrator records the new stamp afterwards, so
#' this method must **not** stamp the version itself and must **not** call
#' `NextMethod()`. It is written to be idempotent.
#'
#' Migrations performed:
#' * The `resource_params` data frame used to be stored under
#'   `other_params(params)[["MR"]]$resource_params`, i.e. in
#'   `params@other_params$other$MR$resource_params`. It now lives alongside the
#'   other MR component parameters in `params@other_params$MR$resource_params`.
#'   This method moves it to the new location.
#'
#' @param object A \linkS4class{mizerMR} object to be upgraded.
#' @param ... Unused.
#'
#' @return The upgraded object.
#' @exportS3Method utils::upgrade
#' @keywords internal
upgrade.mizerMR <- function(object, ...) {
    old <- object@other_params$other$MR$resource_params
    if (!is.null(old)) {
        object@other_params[["MR"]]$resource_params <- old
        object@other_params$other$MR$resource_params <- NULL
        if (length(object@other_params$other$MR) == 0) {
            object@other_params$other$MR <- NULL
        }
    }
    object
}
