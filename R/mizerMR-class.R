#' mizerMR marker classes
#'
#' S4 marker subclasses of [MizerParams] and [MizerSim] that enable S3 dispatch
#' for MR-specific methods.
#'
#' Objects of class `mizerMR` are created by [setMultipleResources()].
#' Objects of class `mizerMRSim` are returned automatically by [project()]
#' when called on a `mizerMR` params object.
#'
#' @name mizerShelf-class
NULL

#' @export
setClass("mizerMR", contains = "MizerParams")

#' @export
setClass("mizerMRSim", contains = "MizerSim")
