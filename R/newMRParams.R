#' Create a new multi-resource, multi-species model
#'
#' @params species_params A data frame with the species parameters, with one
#'   row for each species.
#' @params resource_params A data frame with the resource parameters, with one
#'   row for each resource.
#' @params gear_params A data frame with the gear parameters, with one
#'   row for each gear-species pair.
#' @param interaction Optional interaction matrix between species (predator
#'   species x prey species). Entries should be numbers between 0 and 1. By
#'   default all entries are 1.
#' @param interation_resource Optional interaction matrix between species and
#'   resources (predator species x prey resource). Entries should be numbers
#'   between 0 and 1. By default all entries are 1.
#'
#' @return An object of class MizerMRParams
#' @export
newMRparams <- function(species_params,
                        resource_params,
                        gear_params,
                        interaction,
                        interaction_resource) {

}
