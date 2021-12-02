setClass("MRParams", contains = "MizerParams")

#' Create a new multi-resource, multi-species model
#'
#' @param species_params A data frame with the species parameters, with one
#'   row for each species.
#' @param interaction Optional interaction matrix between species (predator
#'   species x prey species). Entries should be numbers between 0 and 1. By
#'   default all entries are 1.
#' @inheritParams setMultipleResources
#' @param gear_params A data frame with the gear parameters, with one
#'   row for each gear-species pair.
#' @param no_w The number of size bins in the consumer spectrum.
#' @param min_w Sets the size of the eggs of all species for which this is not
#'   given in the `w_min` column of the `species_params` dataframe.
#' @param max_w The largest size of the consumer spectrum. By default this is
#'   set to the largest `w_inf` specified in the `species_params` data
#'   frame.
#'
#' @return An object of class MizerMRParams
#' @export
newMRParams <- function(species_params,
                        interaction = NULL,
                        resource_params,
                        resource_interaction = NULL,
                        gear_params = data.frame(),
                        no_w = 100,
                        min_w = 0.001,
                        max_w = NA) {
    params <- newMultispeciesParams(
        species_params = species_params,
        gear_params = gear_params,
        interaction = interaction,
        no_w = no_w, min_w = min_w, max_w = max_w)

    resource_params <- validResourceParams(resource_params,
                                           w_full(params)[[1]])
    params <- setMultipleResources(params, resource_params = resource_params,
                                   resource_interaction = resource_interaction)

}
