#' Add species to a mizerMR model
#'
#' Extends [mizer::addSpecies()] to handle the multiple-resource interaction
#' matrix. New species are added with a resource interaction of 1 for all
#' resources by default, or as specified by `resource_interaction`.
#'
#' @param params A \linkS4class{mizerMR} object.
#' @param species_params A data frame with the species parameters of the new
#'   species.
#' @param resource_interaction Optional matrix (new species x resources) of
#'   interaction values between new species and each resource. Defaults to 1
#'   for all new species and resources.
#' @inheritParams mizer::addSpecies
#' @return A \linkS4class{mizerMR} object with the new species added.
#' @export
#' @name addSpecies
addSpecies.mizerMR <- function(params, species_params,
                               gear_params = data.frame(),
                               initial_effort,
                               interaction,
                               steady = TRUE,
                               info_level = 0, ...,
                               resource_interaction = NULL) {
    # Save MR state
    rp <- resource_params(params)
    old_interaction <- resource_interaction(params)
    old_sp_names <- rownames(old_interaction)
    no_res <- nrow(rp)

    # Strip MR so mizer's addSpecies can compute encounter rates normally
    params <- .strip_mr(params)

    # Call mizer's addSpecies
    if (missing(interaction)) {
        p <- NextMethod()
    } else {
        p <- NextMethod()
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


#' Remove species from a mizerMR model
#'
#' Extends [mizer::removeSpecies()] to also trim the resource interaction
#' matrix.
#'
#' @section Compensating for the removed species:
#' With `compensate = TRUE`, mizer adjusts the external rates of the remaining
#' species so that at the initial state they keep the encounter rate, mortality
#' and diffusion that they had before the removal, see [mizer::removeSpecies()].
#' The food that the remaining species found in the multiple resources is
#' included in the encounter rate that is restored.
#'
#' The removed species also no longer eat the resources, so the resources they
#' fed on are rebalanced with [balanceResources()], as mizer rebalances its own
#' resource: the capacity of each such resource is set so that at the initial
#' state it replenishes at exactly the rate at which it is now consumed. The
#' resource rate is kept. This replaces a capacity that you had set manually.
#' A resource that was at steady state therefore stays at steady state, and
#' one that was not is put at steady state. Resources whose consumption did not
#' change are left alone. A resource whose dynamics has no balancing function
#' cannot be rebalanced, and you are warned about it, unless it is held
#' constant with [mizer::resource_constant()], which needs no balancing.
#'
#' @param params A \linkS4class{mizerMR} object.
#' @param compensate If `TRUE`, the remaining species keep the rates they had
#'   before the removal and the resources are rebalanced. See the section
#'   "Compensating for the removed species". Default is `FALSE`.
#' @inheritParams mizer::removeSpecies
#' @return A \linkS4class{mizerMR} object with the specified species removed.
#' @export
#' @name removeSpecies
removeSpecies.mizerMR <- function(params, species, compensate = FALSE,
                                  info_level = mizer::default_info_level(),
                                  ...) {
    assert_that(is.flag(compensate), noNA(compensate))
    if (compensate) {
        if (is.null(getComponent(params, "MR"))) {
            return(NextMethod())
        }
        # mizer calculates the compensation on the full model, with the
        # abundances of the removed species set to zero, so the MR component
        # has to stay in place for it rather than be stripped: the encounter
        # rate and the feeding level that mizer restores include the food from
        # the multiple resources.
        old_mort <- unclass(getResourceMort(params))
        old_interaction <- resource_interaction(params)
        # Hold the resources fixed ourselves while mizer compensates, as
        # `tuneSteadyState.mizerMR()` does. That says that mizerMR takes care of
        # this component, which keeps it out of mizer's warning about the
        # components it does not compensate.
        mr_dynamics <- params@other_dynamics[["MR"]]
        params@other_dynamics[["MR"]] <- "constant_other"
        p <- NextMethod()
        p@other_dynamics[["MR"]] <- mr_dynamics
        p@other_params[["MR"]]$interaction <-
            old_interaction[p@species_params$species, , drop = FALSE]
        return(mizer::with_info_level(info_level = info_level, {
            mrRebalanceAfterRemoval(p, old_mort)
        }))
    }

    rp <- resource_params(params)
    capacity <- resource_capacity(params)
    rate <- resource_rate(params)
    initial_resource <- initialNResource(params)
    old_interaction <- resource_interaction(params)

    # Strip MR entirely so the rebuilt params do not carry a stale
    # 12-species interaction matrix into setMultipleResources below.
    params <- .strip_mr(params)

    p <- NextMethod()

    # Trim the interaction matrix to the remaining species
    remaining_sp <- p@species_params$species
    interaction <- old_interaction[remaining_sp, , drop = FALSE]
    setMultipleResources(p, resource_params = rp,
                         resource_interaction = interaction,
                         resource_capacity = capacity,
                         resource_rate = rate,
                         initial_resource = initial_resource)
}

#' Rebalance the resources after species have been removed
#'
#' Called by [removeSpecies()] with `compensate = TRUE` once the species have
#' been removed. Rebalances the capacity of each resource whose mortality has
#' changed, keeping its rate, so that at the initial state it replenishes at
#' exactly the rate at which it is now consumed.
#'
#' A resource on which the removed species did not feed keeps its mortality,
#' because the compensation restores the feeding levels of the remaining
#' species. It is restored only up to round-off, though, so the comparison
#' with the mortality before the removal cannot be exact, as it is in mizer,
#' which has only one resource to rebalance. A resource counts as changed if
#' its mortality changed anywhere by more than `tol` times its largest
#' mortality.
#'
#' @param params A \linkS4class{mizerMR} object from which the species have
#'   been removed.
#' @param old_mort The resource mortality (resource x size) before the removal.
#' @param tol The tolerance for an unchanged mortality, relative to the largest
#'   mortality of that resource.
#' @return The \linkS4class{mizerMR} object with the rebalanced capacities.
#' @keywords internal
mrRebalanceAfterRemoval <- function(params, old_mort, tol = 1e-8) {
    change <- abs(unclass(getResourceMort(params)) - old_mort)
    changed <- apply(change, 1, max) > tol * apply(abs(old_mort), 1, max)
    if (!any(changed)) {
        return(params)
    }
    rp <- resource_params(params)
    # `resource_constant()` keeps the resource where it is anyway
    no_balance <- changed & rp$dynamics != "resource_constant" &
        vapply(rp$dynamics, function(d) is.null(mrBalanceFunction(d)),
               logical(1))
    if (any(no_balance)) {
        mizer::signal_info(
            "resource_capacity", paste0(
                "There is no balancing function for the dynamics of the ",
                "resource", if (sum(no_balance) > 1) "s " else " ",
                toString(rp$resource[no_balance]), ", so ",
                if (sum(no_balance) > 1) "their capacities" else "its capacity",
                " could not be rebalanced after the removed species stopped ",
                "eating ", if (sum(no_balance) > 1) "them" else "it", ". ",
                if (sum(no_balance) > 1) "These resources are" else
                    "This resource is",
                " therefore not compensated for their removal."),
            level = 1, severity = "warning", unhandled = "show")
    }
    balanceComponent(params, derive_rate = FALSE, resources = changed)
}


#' Rename species in a mizerMR model
#'
#' Extends [mizer::renameSpecies()] to also update the row names of the
#' resource interaction matrix.
#'
#' @param params A \linkS4class{mizerMR} object.
#' @inheritParams mizer::renameSpecies
#' @return A \linkS4class{mizerMR} object with the species renamed.
#' @export
#' @name renameSpecies
renameSpecies.mizerMR <- function(params, replace, ...) {
    rp <- resource_params(params)
    capacity <- resource_capacity(params)
    rate <- resource_rate(params)
    initial_resource <- initialNResource(params)
    old_interaction <- resource_interaction(params)

    # Strip MR entirely so the rebuilt params do not carry stale row-names
    # into setMultipleResources below.
    params <- .strip_mr(params)

    p <- NextMethod()

    # Update row names in the interaction matrix
    rownames(old_interaction) <- p@species_params$species
    setMultipleResources(p, resource_params = rp,
                         resource_interaction = old_interaction,
                         resource_capacity = capacity,
                         resource_rate = rate,
                         initial_resource = initial_resource)
}


#' Expand the size grid of a mizerMR model
#'
#' Extends [mizer::expandSizeGrid()] to handle \linkS4class{mizerMR}
#' objects. The resource rate, capacity, and initial abundance arrays are
#' recalculated on the new grid from the resource parameters.
#'
#' @param params A \linkS4class{mizerMR} object.
#' @inheritParams mizer::expandSizeGrid
#' @return A \linkS4class{mizerMR} object with an expanded size grid.
#' @export
#' @name expandSizeGrid
expandSizeGrid.mizerMR <- function(params,
                                   new_min_w = min(params@w),
                                   new_max_w = max(params@w),
                                   preserve_species =
                                       params@species_params$species,
                                   ...) {
    rp <- resource_params(params)
    interaction <- resource_interaction(params)

    params <- .strip_mr(params)
    p <- NextMethod()

    setMultipleResources(p, resource_params = rp,
                         resource_interaction = interaction)
}


#' Strip the MR extension from params
#'
#' Removes the MR extension state so that mizer's own methods can run without
#' triggering the MR encounter method. Also restores the built-in resource from
#' the stored mizer resource parameters so that steady-state calculations have a
#' valid single-resource spectrum.
#'
#' @param params A \linkS4class{mizerMR} object.
#' @return A [mizer::MizerParams-class] object without the MR extension.
#' @keywords internal
.strip_mr <- function(params) {
    if (isS4(params)) {
        p <- mizer:::upgrade_s4_to_s3(params)
    } else {
        p <- params
    }
    class(p) <- "MizerParams"

    # Remove all MR-specific registrations
    p@extensions <- p@extensions[names(p@extensions) != "mizerMR"]
    p@rates_funcs[["Encounter"]] <- "mizerEncounter"
    p@other_dynamics[["MR"]]  <- NULL
    p@other_mort[["MR"]]      <- NULL
    p@other_encounter[["MR"]] <- NULL
    p@other_params[["MR"]]    <- NULL
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

#' Whether second-order bin-averaging is switched on
#'
#' Internal helper reading the `bin_average` entry of the model's
#' `second_order_w` slot via mizer's accessor.
#'
#' @param params A [mizer::MizerParams-class] object.
#' @return `TRUE` or `FALSE`.
#' @keywords internal
mr_bin_average <- function(params) {
    isTRUE(second_order_w(params)[["bin_average"]])
}

#' Bin-average of a power law over a restricted size range
#'
#' Internal helper. Returns the exact average of \eqn{w^d} over each grid bin
#' \eqn{[w_j, w_{j+1}]}, restricted to the size range \eqn{[w_{min}, w_{max}]}
#' (the power law is taken to be zero outside it) and divided by the full bin
#' width \eqn{\Delta w_j}. A bin straddling either knife edge gets the partial
#' average over the in-range part of the bin; bins entirely outside the range
#' get zero. This is the same finite-volume convention mizer uses internally,
#' extended with a lower cutoff for mizerMR's per-resource size ranges.
#'
#' @param w Numeric vector of left bin edges \eqn{w_j}.
#' @param dw Numeric vector of bin widths \eqn{\Delta w_j} (same length as `w`).
#' @param d Single numeric exponent of the power law.
#' @param w_min,w_max Lower and upper cutoffs of the size range. Default to no
#'   cutoff (`-Inf` and `Inf`).
#' @return A numeric vector (same length as `w`) of the restricted bin averages.
#' @keywords internal
power_law_bin_average <- function(w, dw, d, w_min = -Inf, w_max = Inf) {
    w_right <- w + dw
    # In-range integration limits for each bin, clipped to [w_min, w_max] and
    # kept within the bin so that bins outside the range collapse to zero width.
    lo <- pmin(pmax(w, w_min), w_right)
    hi <- pmax(pmin(w_right, w_max), lo)
    if (d == -1) {
        log(hi / lo) / dw
    } else {
        (hi ^ (d + 1) - lo ^ (d + 1)) / ((d + 1) * dw)
    }
}
