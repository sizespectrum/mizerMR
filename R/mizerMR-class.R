#' mizerMR marker classes
#'
#' S4 marker subclasses of [MizerParams] and [MizerSim] that enable S3 dispatch
#' for MR-specific methods.
#'
#' Objects of class `mizerMR` are created by [setMultipleResources()].
#' Objects of class `mizerMRSim` are returned automatically by [project()]
#' when called on a `mizerMR` params object.
#'
#' The classes are **not** defined statically. Instead mizer creates them when
#' the package is loaded: `.onLoad()` calls [mizer::registerExtension()], which
#' recognises mizerMR as a dispatching extension from the S3 methods it
#' registers for its marker class and inserts `mizerMR` at the correct place in
#' the S4 hierarchy relative to any other extension packages loaded in the same
#' session. This lets mizerMR be chained with other extensions (for example
#' mizerReef) in either load order. A static `contains = "MizerParams"`
#' definition would fix mizerMR as a direct sibling of every other extension and
#' prevent such chaining, because a sealed class cannot be re-parented.
#'
#' @name mizerMR-class
#' @aliases mizerMRSim-class
#' @keywords internal
NULL
