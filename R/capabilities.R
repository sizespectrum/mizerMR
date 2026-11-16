# MR's declarations are assembled from its implemented resource laws. Unknown
# author laws remain projectable but acquire no inferred analysis or balancing.
mrKnownDynamics <- function(params) {
    laws <- sub("^mizer::", "", params$other_params$MR$resource_params$dynamics)
    length(laws) && all(laws %in% c("resource_semichemostat",
                                    "resource_logistic", "resource_constant"))
}

mrTransformations <- function() {
    operations <- c("add_species", "remove_species", "reorder_species",
                    "rename_species", "size_grid", "scale_abundance",
                    "scale_rates")
    setNames(lapply(operations, function(operation) {
        list(action = "update", transform_fun = "mizerMR::mrTransform")
    }), operations)
}

mrTuning <- function() {
    make_strategy <- function(adjust, preserve) {
        list(action = "balance",
             hold = list(states = "component:MR", rates = character()),
             evolve = character(),
             adjust = setNames(list(list(path = c("other_params", "MR", adjust),
                                         lower = 0, upper = Inf)), adjust),
             preserve = setNames(list(list(path = c("other_params", "MR",
                                                    preserve))), preserve),
             balance_fun = "mizerMR::mrBalance",
             target_fun = "mizerMR::mrTarget",
             solvers = c("project", "newton", "balance"),
             settings = list(adjust = adjust))
    }
    list(default = "preserve_rate", strategies = list(
        preserve_rate = make_strategy("capacity", "rate"),
        preserve_capacity = make_strategy("rate", "capacity"),
        evolve = list(action = "none", evolve = "component:MR",
                      solvers = "project")
    ))
}

mrCapabilityDefinition <- function(params) {
    known <- mrKnownDynamics(params)
    known_time <- known && mrKnownProviderLayer(params) &&
        mizer_fn("builtin_autonomous_rates")(mrCoreView(params))
    list(dynamics_fun = "mizerMR_dynamics",
         rate_fun = if (known) "mizerMR::mrEquation" else NULL,
         integration_order = if (known) 1L else NULL,
         convergence_fun = if (known) "mizerMR::mrConvergence" else NULL,
         role = "dynamic", prescribed_mask_fun = "mizerMR::mrPrescribedMask",
         autonomous = if (known_time) TRUE else NULL,
         stationary = if (known_time) TRUE else NULL,
         equation_rates_fun = if (known) "mizerMR::mrEquationRates" else NULL,
         tuning = if (known) mrTuning() else NULL,
         transformations = if (known) mrTransformations() else NULL,
         biomass_fun = "mizerMR::mrBiomass",
         biomass_filters = c("min_w", "max_w", "min_l", "max_l"))
}

# Inspect the provider chain with only MR's own known dispatch layer removed.
# This never runs a calculation on the stripped copy or modifies the real model.
mrCoreView <- function(params) {
    keep <- names(params$extensions) != "mizerMR"
    params$extensions <- params$extensions[keep]
    class(params) <- setdiff(class(params), "mizerMR")
    params
}

# Removing our layer for inspection must not hide an additional method that an
# author registered on the mizerMR class itself.
mrKnownProviderLayer <- function(params) {
    generics <- c("project", "project_simple", "projectRates",
                  "projectEncounter", "projectFeedingLevel",
                  "projectEReproAndGrowth", "projectERepro", "projectEGrowth",
                  "projectDiffusion", "projectPredRate", "projectPredMort",
                  "projectFMort", "projectMort", "projectRDI", "projectRDD",
                  "projectResourceMort", "balanceRDD", "setParams",
                  "setInteraction", "setPredKernel", "setMaxIntakeRate",
                  "setMetabolicRate", "setExtMort", "setExtEncounter",
                  "setExtDiffusion", "setSearchVolume", "setReproduction",
                  "setFishing", "setResource", "setColours", "setLinetypes")
    all(vapply(generics, function(generic) {
        actual <- getS3method(generic, "mizerMR", optional = TRUE)
        expected <- get0(paste0(generic, ".mizerMR"),
                         envir = asNamespace("mizerMR"), inherits = FALSE)
        identical(actual, expected)
    }, logical(1)))
}

# Standard MR adds encounter and mortality through its S3 methods, not through
# callbacks stored on the component. Such author callbacks own unknown data and
# must survive migration without gaining automatic analysis or update support.
mrKnownComponentProviders <- function(params) {
    identical(params$other_dynamics$MR, "mizerMR_dynamics") &&
        is.null(params$other_encounter$MR) && is.null(params$other_mort$MR)
}

# Saved capability templates retain their author's choices across upgrades.
# Check the boundary of our implementations on use, including templates written
# by previews that did not inspect all component providers or setter methods.
mrRequireProviders <- function(params, construction = FALSE) {
    step <- params$other_dynamics$MR
    declared_step <- params$capabilities$components$MR$dynamics_fun
    known_step <- identical(step, "mizerMR_dynamics") ||
        (identical(step, "constant_other") &&
             identical(declared_step, "mizerMR_dynamics"))
    supported <- known_step && is.null(params$other_encounter$MR) &&
        is.null(params$other_mort$MR) && mrKnownProviderLayer(params)
    if (construction) {
        binding <- mizer_fn("transformation_provider_binding")(
            mrCoreView(params)
        )
        supported <- supported && all(endsWith(
            unlist(binding$construction_methods, use.names = FALSE), "@mizer"
        ))
    }
    if (!supported) {
        stop("MR's built-in capability callbacks do not support these ",
             "modified component or dispatch providers. Supply authored ",
             "callbacks and capability definitions for the modified model.")
    }
    invisible(NULL)
}

registerMRCapabilities <- function(params, adopt_processes = TRUE) {
    params <- mizer::coerceToExtensionClass(params)
    if (!mrKnownComponentProviders(params)) return(params)
    params <- mizer::setComponent(params, "MR",
                                  definition = mrCapabilityDefinition(params))
    params$other_params$MR$capability_version <- 1L
    if (!adopt_processes || !mrKnownProviderLayer(params)) return(params)
    base <- mrCoreView(params)
    transformations <- function(owner) {
        definition <- mizer_fn("default_provider_transformations")(base, owner)
        definition$transformations
    }
    consumer <- mizer_fn("default_provider_tuning")(base, "consumers")
    if (!is.null(consumer$tuning)) {
        params <- mizer::setProcess(
            params, "consumers",
            convergence_fun = "builtin_consumer_assessment",
            role = "dynamic", prescribed_mask_fun = NULL,
            autonomous = if (mizer_fn("builtin_autonomous_rates")(base)) {
                TRUE
            } else {
                NULL
            },
            stationary = TRUE,
            equation_rates_fun = "builtin_consumer_equation_rates",
            tuning = consumer$tuning,
            transformations = transformations("consumers")
        )
    }
    if (identical(params$resource_dynamics, "resource_constant") &&
            all(params$initial_n_pp == 0)) {
        zero_transforms <- lapply(transformations("resource"), function(x) {
            list(action = "update",
                 transform_fun = "mizerMR::mrTransformCoreResource",
                 paths = list("initial_n_pp"))
        })
        params <- mizer::setProcess(
            params, "resource", role = "prescribed", prescribed_mask_fun = NULL,
            convergence_fun = NULL, autonomous = TRUE, stationary = TRUE,
            equation_rates_fun = "builtin_prescribed_equation_rates",
            rate_fun = "equation_resource_constant",
            tuning = list(
                default = "unchanged",
                strategies = list(
                    unchanged = list(
                        action = "none",
                        hold = list(states = "resource", rates = character())
                    )
                )
            ),
            transformations = zero_transforms
        )
    }
    reproduction <- mizer_fn("default_provider_tuning")(base, "reproduction")
    if (!is.null(reproduction$tuning)) {
        params <- mizer::setProcess(
            params, "reproduction",
            definition = mizer::mizerReproductionDefinition(params),
            transformations = transformations("reproduction")
        )
    }
    params
}

#' Structural inputs in the MR component
#' @param params A multiple-resource model.
#' @param ... Unused context.
#' @return A logical resource-by-size mask. Only explicitly constant laws are
#'   prescribed; zero abundances, capacities and rates do not imply inputs.
#' @keywords internal
#' @export
mrPrescribedMask <- function(params, ...) {
    mrRequireProviders(params)
    mask <- params$initial_n_other$MR == 0
    mask[] <- FALSE
    laws <- params$other_params$MR$resource_params$dynamics
    constant <- sub("^mizer::", "", laws) == "resource_constant"
    mask[constant, ] <- TRUE
    mask
}

#' Effective equation quantities for multiple resources
#' @param params A multiple-resource model.
#' @param n_other Component states.
#' @param rates The complete effective model rates.
#' @param ... Unused context.
#' @return Named source and loss arrays entering the MR equations.
#' @keywords internal
#' @export
mrEquationRates <- function(params, n_other, rates, ...) {
    mrRequireProviders(params)
    if (!mrKnownDynamics(params)) {
        stop("MR equation support is unavailable for an unknown resource law.")
    }
    state <- n_other$MR
    mort <- rates$resource_mort
    if (!is.numeric(mort) || !identical(dim(mort), dim(state)) ||
            any(!is.finite(mort))) {
        stop("MR needs effective resource mortality with ",
             "resource-state dimensions.")
    }
    mr <- params$other_params$MR
    source <- loss <- state * 0
    laws <- sub("^mizer::", "", mr$resource_params$dynamics)
    for (i in seq_len(nrow(state))) {
        if (laws[i] == "resource_semichemostat") {
            source[i, ] <- mr$rate[i, ] * mr$capacity[i, ]
            loss[i, ] <- (mr$rate[i, ] + mort[i, ]) * state[i, ]
        } else if (laws[i] == "resource_logistic") {
            source[i, ] <- mr$rate[i, ] * state[i, ]
            loss[i, ] <- mort[i, ] * state[i, ]
            growth <- mr$rate[i, ] != 0
            if (any(growth & mr$capacity[i, ] <= 0)) {
                stop("Logistic equation support requires positive capacity ",
                     "where replenishment is nonzero, including empty bins.")
            }
            active <- state[i, ] != 0 & growth
            loss[i, active] <- loss[i, active] +
                mr$rate[i, active] * state[i, active]^2 / mr$capacity[i, active]
        }
    }
    list(source = source, loss = loss)
}

#' Exact instantaneous MR derivative
#' @inheritParams mrEquationRates
#' @return A numeric resource-by-size derivative array.
#' @keywords internal
#' @export
mrEquation <- function(params, n_other, rates, ...) {
    terms <- mrEquationRates(params, n_other, rates)
    terms$source - terms$loss
}

#' Quantitative convergence measures for each resource
#' @inheritParams mrEquationRates
#' @param derivative Optional derivative supplied by mizer.
#' @param dynamic_mask Coordinates included in the assessment.
#' @param tol,atol Relative and absolute tolerances.
#' @param criterion State residual or biomass drift.
#' @return A data frame of resource-specific residuals and tolerances.
#' @keywords internal
#' @export
mrConvergence <- function(params, n_other, rates, derivative = NULL,
                          dynamic_mask = NULL, tol = 0.05, atol = 1e-12,
                          criterion = "state", ...) {
    if (is.null(derivative)) derivative <- mrEquation(params, n_other, rates)
    state <- n_other$MR
    if (!is.null(dynamic_mask)) {
        derivative[!dynamic_mask] <- 0
        state[!dynamic_mask] <- 0
    }
    weighting <- mizer_fn("bin_average_weight")(params$w_full, params) *
        params$dw_full
    value <- if (criterion == "biomass") {
        abs(as.numeric(derivative %*% weighting))
    } else {
        as.numeric(abs(derivative) %*% weighting)
    }
    data.frame(measure = rownames(state), value = value,
               tolerance = atol + tol * as.numeric(abs(state) %*% weighting),
               units = "g/year")
}

#' Measure the biomass of every MR resource
#' @inheritParams mrEquationRates
#' @param filters Explicit biomass size filters.
#' @return A data frame of named biomass series and their styles.
#' @keywords internal
#' @export
mrBiomass <- function(params, n_other, filters = list(), ...) {
    rp <- params$other_params$MR$resource_params
    values <- vapply(filters, function(x) {
        is.numeric(x) && length(x) == 1L
    }, logical(1))
    result <- data.frame(series = rp$resource, value = NA_real_,
                         label = rp$resource, colour = rp$colour,
                         linetype = rp$linetype,
                         source = paste0("component:MR/", rp$resource))
    if (any(!values)) {
        result$available <- FALSE
        result$reason <- "MR biomass requires scalar size filters."
        return(result)
    }
    if (anyNA(unlist(filters, use.names = FALSE))) {
        stop("MR biomass size limits must not be NA or NaN.")
    }
    state <- n_other$MR
    expected <- c(nrow(rp), length(params$w_full))
    if (!is.numeric(state) || !identical(dim(state), expected) ||
            any(!is.finite(state)) || any(state < 0)) {
        stop("MR biomass requires finite non-negative resource abundances ",
             "on the resource-by-size grid.")
    }
    lower <- filters$min_w %||% min(params$w_full)
    upper <- filters$max_w %||% max(params$w_full)
    if (!is.null(filters$min_l) || !is.null(filters$max_l)) {
        lengths <- unlist(filters[c("min_l", "max_l")], use.names = FALSE)
        if (any(lengths < 0)) {
            stop("MR biomass length limits must be non-negative.")
        }
        mapping <- mizer_fn("resource_length_params")(params)
        valid <- vapply(mapping, function(x) {
            is.numeric(x) && length(x) == 1L && is.finite(x) && x > 0
        }, logical(1))
        if (!all(valid)) {
            stop("MR biomass requires positive finite ",
                 "weight-length parameters.")
        }
        if (!is.null(filters$min_l)) {
            lower <- mapping$a * filters$min_l^mapping$b
        }
        if (!is.null(filters$max_l)) {
            upper <- mapping$a * filters$max_l^mapping$b
        }
    }
    if (anyNA(c(lower, upper))) {
        stop("MR biomass size limits could not be converted to weights.")
    }
    selected <- params$w_full >= lower & params$w_full <= upper
    bin_weight <- mizer_fn("bin_average_weight")
    weighting <- bin_weight(params$w_full * selected, params) * params$dw_full
    result$value <- as.numeric(n_other$MR %*% weighting)
    result
}

#' Independently construct the effective-rate targets for MR tuning
#' @inheritParams mrEquationRates
#' @param settings Selected strategy settings.
#' @return Named target source and loss arrays.
#' @keywords internal
#' @export
mrTarget <- function(params, n_other, rates, settings, ...) {
    terms <- mrEquationRates(params, n_other, rates)
    mr <- params$other_params$MR
    state <- n_other$MR
    laws <- sub("^mizer::", "", mr$resource_params$dynamics)
    for (i in seq_len(nrow(state))) {
        if (laws[i] == "resource_constant") next
        if (settings$adjust == "capacity") {
            if (laws[i] == "resource_semichemostat") {
                terms$source[i, ] <- terms$loss[i, ]
            } else {
                terms$loss[i, ] <- terms$source[i, ]
            }
        } else {
            n <- state[i, ]
            k <- mr$capacity[i, ]
            mu <- rates$resource_mort[i, ]
            if (any(n > 0 & k <= n & mu > 0)) {
                stop("Resource capacity must exceed abundance to balance loss.")
            }
            flux <- terms$source[i, ]
            if (laws[i] == "resource_semichemostat") {
                use <- k != n
                flux[use] <- mu[use] * n[use] * k[use] / (k[use] - n[use])
            } else {
                use <- n > 0 & k != n
                flux[use] <- mu[use] * n[use] / (1 - n[use] / k[use])
            }
            terms$source[i, ] <- terms$loss[i, ] <- flux
        }
    }
    terms
}

mrBalancedParameters <- function(params, state, mortality, adjust) {
    mr <- params$other_params$MR
    output <- mr[[adjust]]
    laws <- sub("^mizer::", "", mr$resource_params$dynamics)
    if (!mrKnownDynamics(params)) stop("Unknown MR law cannot be balanced.")
    for (i in seq_len(nrow(state))) {
        n <- state[i, ]
        r <- mr$rate[i, ]
        k <- mr$capacity[i, ]
        mu <- mortality[i, ]
        if (laws[i] == "resource_constant") next
        if (adjust == "capacity") {
            if (any(n > 0 & r == 0 & mu > 0)) {
                stop("Resource loss cannot balance a zero replenishment rate.")
            }
            if (laws[i] == "resource_semichemostat") {
                active <- r > 0
                output[i, active] <- n[active] * (1 + mu[active] / r[active])
            } else {
                active <- n > 0 & (r > 0 | mu > 0)
                if (any(active & r <= mu)) {
                    stop("Logistic replenishment must exceed ",
                         "resource mortality.")
                }
                output[i, active] <- n[active] / (1 - mu[active] / r[active])
            }
        } else {
            active <- n > 0
            if (any(active & k <= n & mu > 0)) {
                stop("Resource capacity must exceed abundance to balance loss.")
            }
            if (laws[i] == "resource_semichemostat") {
                use <- k != n
                output[i, use] <- mu[use] * n[use] / (k[use] - n[use])
            } else {
                use <- active & k != n
                output[i, use] <- mu[use] / (1 - n[use] / k[use])
            }
        }
    }
    if (any(!is.finite(output)) || any(output < 0)) {
        stop("The selected MR preservation strategy is infeasible.")
    }
    output
}

#' Balance the selected MR parameter against independently captured targets
#' @inheritParams mrTarget
#' @param targets Effective equation quantities captured before balancing.
#' @return The updated candidate model, with state unchanged.
#' @keywords internal
#' @export
mrBalance <- function(params, n_other, rates, settings, targets, ...) {
    mrRequireProviders(params)
    adjusted <- mrBalancedParameters(params, n_other$MR, rates$resource_mort,
                                     settings$adjust)
    comment(adjusted) <- "set by balancing"
    params$other_params$MR[[settings$adjust]] <- adjusted
    params
}
