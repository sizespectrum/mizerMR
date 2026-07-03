#' @keywords internal
#' @import mizerExperimental ggplot2 methods assertthat
#' @importFrom mizer addSpecies animateSpectra animate expandSizeGrid finalNResource getDiet getEncounter getPredRate getRates getResourceMort initialNResource NResource plotDiet plotSpectra projectEncounter projectResourceMort removeSpecies renameSpecies scaleModel scaleRates setResource plotDataFrame plotHover parsePlotLog apply_wlim validParams w_full dw_full getComponent setComponent setInitialValues species_params initialN initialNOther NOther idxFinalT get_time_elements valid_species_arg getFeedingLevel melt mizerEncounter newMultispeciesParams second_order_w setColours setLinetypes
#' @importFrom mizer "initialNResource<-" "resource_dynamics<-"
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

globalVariables(c("expect_equal", "sp", "value", "w", "resource",
                  "Predator", "Species", "time"))

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
