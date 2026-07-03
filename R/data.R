#' South-East Australian reef ecosystem model
#'
#' A mizerMR model object representing a size-structured multispecies
#' model of the South-East Australian (SEA) rocky reef coast ecosystem.
#' The model contains 17 species and three size-structured background resource
#' spectra representing distinct primary production pathways:
#' \describe{
#'   \item{pl}{Pelagic plankton resource}
#'   \item{aa}{Algal/attached benthic resource}
#'   \item{bb}{Benthic invertebrate resource}
#' }
#'
#' The original model code and parameter derivations are available at
#' \url{https://github.com/astaaudzi/SEAmodel}.
#'
#' @format A [mizerMR-class] object (an S4 object extending
#'   [mizer::MizerParams-class]) with 17 species and 3 resources.
#'   The multiple resources are stored in `params@@other_params[["MR"]]`.
#'
#' @references Audzijonyte A, Delius GW, Stuart-Smith RD, Novaglio C, Edgar GJ,
#'   Barrett NS, Blanchard JL (2023). Changes in sea floor productivity are
#'   crucial to understanding the impact of climate change in temperate coastal
#'   ecosystems according to a new size-based model.
#'   \emph{PLoS Biology} 21(12): e3002392.
#'   \doi{10.1371/journal.pbio.3002392}
#'
#' @source Created with [setMultipleResources()].
#'   Original parameter derivations: \url{https://github.com/astaaudzi/SEAmodel}.
#' @seealso [setMultipleResources()], [newMRParams()]
#' @name SEAmodel
"SEAmodel"
