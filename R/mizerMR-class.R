#' mizerMR marker classes
#'
#' S3 marker classes for [MizerParams] and [MizerSim] that enable S3 dispatch
#' for MR-specific methods.
#'
#' Objects of class `mizerMR` are created by [setMultipleResources()].
#' Objects of class `mizerMRSim` are returned automatically by [project()]
#' when called on a `mizerMR` params object.
#'
#' [setMultipleResources()] records mizerMR on the model with
#' [mizer::recordExtension()] and uses [mizer::coerceToExtensionClass()] to
#' restore its S3 dispatch chain. Loading a package does not change existing
#' models' extension chains.
#'
#' @name mizerMR-class
#' @aliases mizerMRSim-class
#' @keywords internal
NULL
