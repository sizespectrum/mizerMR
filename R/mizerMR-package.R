#' @keywords internal
#' @import mizerExperimental ggplot2 methods assertthat
#' @importFrom plotly ggplotly
#' @importFrom stats mvfft
#' @importFrom utils globalVariables
#' @importFrom rlang signal
#' @importFrom dplyr %>% filter
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

globalVariables(c("expect_equal"))

#' S4 subclass of MizerParams for models with multiple resources
#'
#' Created by [setMultipleResources()] or [newMRParams()]. Inherits all slots
#' from \linkS4class{MizerParams} and triggers S3 dispatch to mizerMR-specific
#' methods for plotting, accessors, and projection.
#'
#' @exportClass MRMizerParams
setClass("MRMizerParams", contains = "MizerParams")

#' S4 subclass of MizerSim for simulations with multiple resources
#'
#' Returned by [project()] when called on an \linkS4class{MRMizerParams} object.
#' Inherits all slots from \linkS4class{MizerSim} and triggers S3 dispatch to
#' mizerMR-specific methods for result accessors and plotting.
#'
#' @exportClass MRMizerSim
setClass("MRMizerSim", contains = "MizerSim")
