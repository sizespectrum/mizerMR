#' @keywords internal
#' @import mizerExperimental ggplot2 methods assertthat
#' @importFrom mizer addSpecies animateSpectra expandSizeGrid finalNResource getDiet getEncounter getPredRate getRates getResourceMort initialNResource initialNResource<- NResource plotDiet plotSpectra projectEncounter projectResourceMort removeSpecies renameSpecies
#' @importFrom mizer plotDataFrame plotHover parsePlotLog apply_wlim
#' @importFrom plotly ggplotly
#' @importFrom stats mvfft
#' @importFrom utils globalVariables
#' @importFrom rlang signal %||%
#' @importFrom dplyr %>% filter
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL

globalVariables(c("expect_equal"))

#' Register mizerMR with mizer
#'
#' Registers the package as a mizer extension when the namespace is loaded.
#'
#' @param libname Library path supplied by R.
#' @param pkgname Package name supplied by R.
#' @return Called for its side effect.
#' @keywords internal
.onLoad <- function(libname, pkgname) {
    mizer::registerExtension(pkgname, requirement = "sizespectrum/mizerMR")
}
