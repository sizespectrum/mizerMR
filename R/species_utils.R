#' Add species to an MRMizerParams model
#'
#' Extends [mizer::addSpecies()] to handle the multiple-resource interaction
#' matrix. New species are added with a resource interaction of 1 for all
#' resources by default, or as specified by `resource_interaction`.
#'
#' @param params An \linkS4class{MRMizerParams} object.
#' @param species_params A data frame with the species parameters of the new
#'   species.
#' @param resource_interaction Optional matrix (new species x resources) of
#'   interaction values between new species and each resource. Defaults to 1
#'   for all new species and resources.
#' @inheritParams mizer::addSpecies
#' @return An \linkS4class{MRMizerParams} object with the new species added.
#' @export
#' @name addSpecies
addSpecies.MRMizerParams <- function(params, species_params,
                                     interaction,
                                     gear_params = data.frame(),
                                     initial_effort,
                                     resource_interaction = NULL,
                                     info_level = 0, ...) {
    # Save MR state
    rp <- resource_params(params)
    old_interaction <- resource_interaction(params)
    old_sp_names <- rownames(old_interaction)
    no_res <- nrow(rp)

    # Strip MR so mizer's addSpecies can compute encounter rates normally
    p_stripped <- .strip_mr(params)

    # Call mizer's addSpecies
    if (missing(interaction)) {
        p <- addSpecies(p_stripped, species_params,
                        gear_params = gear_params,
                        info_level = info_level, ...)
    } else {
        p <- addSpecies(p_stripped, species_params,
                        interaction = interaction,
                        gear_params = gear_params,
                        info_level = info_level, ...)
    }
    if (!missing(initial_effort)) {
        p@initial_effort[names(initial_effort)] <- initial_effort
    }

    # Determine new species names
    all_sp_names <- p@species_params$species
    new_sp_names <- setdiff(all_sp_names, old_sp_names)
    no_new_sp <- length(new_sp_names)

    # Build new resource_interaction: default to 1 for new species
    new_interaction <- matrix(
        1, nrow = length(all_sp_names), ncol = no_res,
        dimnames = list(sp = all_sp_names, resource = colnames(old_interaction))
    )
    new_interaction[old_sp_names, ] <- old_interaction
    if (!is.null(resource_interaction)) {
        if (is.null(dim(resource_interaction))) {
            resource_interaction <- matrix(
                resource_interaction, nrow = no_new_sp, ncol = no_res,
                dimnames = list(sp = new_sp_names,
                                resource = colnames(old_interaction))
            )
        }
        if (!identical(dim(resource_interaction), c(no_new_sp, no_res))) {
            stop("`resource_interaction` must have dimensions ",
                 no_new_sp, " x ", no_res,
                 " (new species x resources).")
        }
        new_interaction[new_sp_names, ] <- resource_interaction
    }

    # Rebuild the MR component on the expanded params
    setMultipleResources(p, resource_params = rp,
                         resource_interaction = new_interaction)
}


#' Remove species from an MRMizerParams model
#'
#' Extends [mizer::removeSpecies()] to also trim the resource interaction
#' matrix.
#'
#' @param params An \linkS4class{MRMizerParams} object.
#' @inheritParams mizer::removeSpecies
#' @return An \linkS4class{MRMizerParams} object with the specified species removed.
#' @export
#' @name removeSpecies
removeSpecies.MRMizerParams <- function(params, species) {
    old_interaction <- resource_interaction(params)

    # Provide initial resource values so the MR encounter function works
    # during any internal validation inside mizer's removeSpecies
    p <- as(params, "MizerParams")
    p@initial_n_other[["MR"]] <- getComponent(params, "MR")$initial_value

    p <- removeSpecies(p, species)

    # Trim the interaction matrix to the remaining species
    remaining_sp <- p@species_params$species
    p@other_params[["MR"]]$interaction <-
        old_interaction[remaining_sp, , drop = FALSE]

    new("MRMizerParams", p)
}


#' Rename species in an MRMizerParams model
#'
#' Extends [mizer::renameSpecies()] to also update the row names of the
#' resource interaction matrix.
#'
#' @param params An \linkS4class{MRMizerParams} object.
#' @inheritParams mizer::renameSpecies
#' @return An \linkS4class{MRMizerParams} object with the species renamed.
#' @export
#' @name renameSpecies
renameSpecies.MRMizerParams <- function(params, replace) {
    old_interaction <- resource_interaction(params)

    # Provide initial resource values so the MR encounter function works
    # during any internal validation inside mizer's renameSpecies
    p <- as(params, "MizerParams")
    p@initial_n_other[["MR"]] <- getComponent(params, "MR")$initial_value

    p <- renameSpecies(p, replace)

    # Update row names in the interaction matrix
    rownames(old_interaction) <- p@species_params$species
    p@other_params[["MR"]]$interaction <- old_interaction

    new("MRMizerParams", p)
}


#' Expand the size grid of an MRMizerParams model
#'
#' Extends [mizer::expandSizeGrid()] to handle \linkS4class{MRMizerParams}
#' objects. The resource rate, capacity, and initial abundance arrays are
#' recalculated on the new grid from the resource parameters.
#'
#' @param params An \linkS4class{MRMizerParams} object.
#' @inheritParams mizer::expandSizeGrid
#' @return An \linkS4class{MRMizerParams} object with an expanded size grid.
#' @export
#' @name expandSizeGrid
expandSizeGrid.MRMizerParams <- function(params,
                                         new_min_w = min(params@w),
                                         new_max_w = max(params@w),
                                         preserve_species =
                                             params@species_params$species) {
    rp <- resource_params(params)
    interaction <- resource_interaction(params)

    p_stripped <- .strip_mr(params)
    p <- mizer::expandSizeGrid(p_stripped, new_min_w = new_min_w,
                               new_max_w = new_max_w,
                               preserve_species = preserve_species)

    setMultipleResources(p, resource_params = rp,
                         resource_interaction = interaction)
}


# Internal helper: strip the MR extension from params so that mizer's own
# methods (addSpecies, expandSizeGrid) can run without triggering the MR
# encounter function.  Also restores the built-in resource to the state
# implied by the stored resource_params so that steadySingleSpecies can find
# a valid solution.
.strip_mr <- function(params) {
    p <- as(params, "MizerParams")

    # Remove all MR-specific registrations
    p@extensions <- p@extensions[names(p@extensions) != "mizerMR"]
    p@rates_funcs[["Encounter"]] <- "mizerEncounter"
    p@other_dynamics[["MR"]]  <- NULL
    p@other_mort[["MR"]]      <- NULL
    p@other_encounter[["MR"]] <- NULL
    p@other_params[["MR"]]    <- NULL
    p@other_params$other[["MR"]] <- NULL
    p@initial_n_other[["MR"]] <- NULL

    # Restore built-in resource from the mizer resource_params so that
    # mizer's steadySingleSpecies can find a valid growth solution.
    rp <- p@resource_params
    wf <- w_full(p)
    pp <- rp$kappa * wf ^ (-rp$lambda) * (wf <= rp$w_pp_cutoff)
    p@initial_n_pp[] <- pp
    p@cc_pp[]        <- pp
    p@rr_pp[]        <- rp$r_pp * wf ^ (rp$n - 1) * (wf <= rp$w_pp_cutoff)
    p@resource_dynamics <- "resource_semichemostat"

    p
}
