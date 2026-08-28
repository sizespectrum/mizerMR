#' mizerMR marker classes
#'
#' S3 marker classes for [MizerParams] and [MizerSim] that enable S3 dispatch
#' for MR-specific methods.
#'
#' Objects of class `mizerMR` are created by [setMultipleResources()].
#' Objects of class `mizerMRSim` are returned automatically by [project()]
#' when called on a `mizerMR` params object.
#'
#' The classes are managed by mizer when the package is loaded: `.onLoad()` calls
#' [mizer::registerExtension()], which recognises mizerMR as a dispatching
#' extension from the S3 methods it registers for its marker class and inserts
#' `mizerMR` into the S3 class vector in dispatch order relative to any other
#' extension packages loaded in the same session.
#'
#' @name mizerMR-class
#' @aliases mizerMRSim-class
#' @keywords internal
NULL
