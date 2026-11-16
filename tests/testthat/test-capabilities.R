test_that("MR explicitly declares equations, convergence and biomass", {
    for (scheme in c(FALSE, TRUE)) {
        p <- small_mr_model(scheme)
        definition <- getComponent(p, "MR")$definition
        expect_identical(definition$rate_fun, "mizerMR::mrEquation")
        expect_identical(definition$autonomous, TRUE)
        measurement <- mizer::getBiomass(p, include = "component:MR")
        weight <- mizer_fn("bin_average_weight")(p$w_full, p) * p$dw_full
        expect_equal(as.numeric(measurement),
                     as.numeric(p$initial_n_other$MR %*% weight))
        expect_identical(names(measurement),
                         c("component:MR/small", "component:MR/large"))
        assessment <- mizer::getSteadyAssessment(p)
        expect_length(assessment$missing, 0)
        expect_equal(sum(assessment$measures$owner == "component:MR"), 2)
        expect_true("resource" %in% assessment$prescribed)
        expect_false(any(mrPrescribedMask(p)))
        expect_true(all(p$initial_n_pp == 0))
        sim <- project(p, t_max = 0.2, dt = 0.1, t_save = 0.1,
                       progress_bar = FALSE, subcycle = TRUE)
        biomass <- mizer::getBiomass(sim, include = "components")
        expect_equal(ncol(biomass), 2)
        expect_true(all(attr(biomass, "coverage")$available))
    }
})

test_that("MR tuning strategies balance without changing their held state", {
    p <- small_mr_model()
    initialNResource(p) <- initialNResource(p) / 2
    original <- p$initial_n_other$MR
    for (strategy in c("preserve_rate", "preserve_capacity")) {
        q <- suppressWarnings(mizer::tuneSteadyState(
            p, strategies = setNames(strategy, "component:MR"),
            t_max = 0.2, dt = 0.1, t_check = 0.1, progress_bar = FALSE,
            info_level = 0
        ))
        expect_equal(q$initial_n_other$MR, original)
        kept <- if (strategy == "preserve_rate") "rate" else "capacity"
        expect_equal(q$other_params$MR[[kept]], p$other_params$MR[[kept]])
        terms <- mrEquationRates(q, q$initial_n_other, getRates(q))
        expect_equal(terms$source, terms$loss, ignore_attr = TRUE)
        expect_identical(q$other_dynamics$MR, "mizerMR_dynamics")
        expect_length(attr(q, "convergence")$assessment$missing, 0)
    }
    expect_error(mizer::tuneSteadyState(
        p, solver = "newton", strategies = c("component:MR" = "evolve"),
        info_level = 0
    ), "solver|Newton|component")
})

test_that("Unknown resource laws gain no inferred support", {
    rlang::local_bindings(
        author_resource = function(n_pp, ...) n_pp,
        balance_author_resource = function(...) stop("must not infer me"),
        .env = globalenv()
    )
    p <- small_mr_model()
    rp <- resource_params(p)
    rp$dynamics <- "author_resource"
    p <- setMultipleResources(p, resource_params = rp, balance = FALSE)
    definition <- getComponent(p, "MR")$definition
    expect_null(definition$rate_fun)
    expect_null(definition$convergence_fun)
    expect_null(definition$tuning)
    expect_null(definition$autonomous)
    expect_true(all(attr(mizer::getBiomass(p, include = "components"),
                         "coverage")$available))
    expect_error(mizer::tuneSteadyState(p, t_max = 1, info_level = 0),
                 "capabilit|support|component")
    expect_error(mizer::scaleModel(p, 2), "support|transformation")
    expect_error(balanceResources(p, resource_rate = resource_rate(p)),
                 "No balancing capability")
})

test_that("Constant MR rows are explicit prescribed inputs", {
    p <- small_mr_model()
    rp <- resource_params(p)
    rp$dynamics <- c("resource_constant", "resource_semichemostat")
    p <- setMultipleResources(p, resource_params = rp, balance = FALSE)
    mask <- mrPrescribedMask(p)
    expect_true(all(mask[1, ]))
    expect_false(any(mask[2, ]))
    rates <- getRates(p)
    derivative <- mrEquation(p, p$initial_n_other, rates)
    expect_equal(unname(derivative[1, ]), rep(0, ncol(derivative)))
})


test_that("MR adoption detects additional effective S3 rate providers", {
    p <- small_mr_model()
    rlang::local_bindings(projectRDI.mizerMR = function(params, ...) {
        NextMethod()
    }, .env = globalenv())
    expect_false(mrKnownProviderLayer(p))
    p$other_params$MR$capability_version <- NULL
    q <- registerMRCapabilities(p)
    expect_null(getComponent(q, "MR")$definition$autonomous)
    expect_false(mizer_fn("tuning_binding_current")(
        q, getProcess(q, "reproduction"), "reproduction"
    ))
    expect_error(mizer::tuneSteadyState(q, t_max = 1, info_level = 0),
                 "capabilit|support|stationary")
})


test_that("MR analytic balancing retains feasible zero-rate parameters", {
    p <- small_mr_model()
    p$other_params$MR$resource_params$dynamics <- "resource_logistic"
    p$other_params$MR$rate[] <- 0
    mortality <- p$initial_n_other$MR * 0
    capacity <- mrBalancedParameters(p, p$initial_n_other$MR, mortality,
                                     "capacity")
    expect_equal(capacity, p$other_params$MR$capacity)
    mortality[1, 1] <- 1
    p$initial_n_other$MR[1, 1] <- 1
    expect_error(mrBalancedParameters(p, p$initial_n_other$MR, mortality,
                                      "capacity"), "zero replenishment")
})


test_that("Zero logistic growth has a defined loss equation at zero capacity", {
    p <- small_mr_model()
    p$other_params$MR$resource_params$dynamics <- "resource_logistic"
    p$other_params$MR$rate[] <- 0
    p$other_params$MR$capacity[] <- 0
    state <- p$initial_n_other
    state$MR[] <- 1
    mortality <- state$MR * 2
    derivative <- mrEquation(p, state, list(resource_mort = mortality))
    expect_equal(derivative, -mortality * state$MR)
})


test_that("Empty logistic bins do not hide an undefined equation domain", {
    p <- small_mr_model()
    p$other_params$MR$resource_params$dynamics <- "resource_logistic"
    p$other_params$MR$rate[1, 1] <- 1
    p$other_params$MR$capacity[1, 1] <- 0
    p$initial_n_other$MR[1, 1] <- 0
    rates <- list(resource_mort = p$initial_n_other$MR * 0)
    expect_error(mrEquation(p, p$initial_n_other, rates),
                 "including empty bins")
    expect_error(mizer::getSteadyAssessment(p), "including empty bins")
    expect_false(mrPrescribedMask(p)[1, 1])
})

test_that("MR adoption does not hide unknown drivers or construction methods", {
    for (generic in c("project", "project_simple", "setPredKernel")) {
        local({
            method <- paste0(generic, ".mizerMR")
            rlang::local_bindings(!!!setNames(list(function(params, ...) {
                NextMethod()
            }), method), .env = globalenv())
            p <- small_mr_model()
            expect_false(mrKnownProviderLayer(p))
            expect_null(getComponent(p, "MR")$definition$autonomous)
            expect_error(mizer::addSpecies(
                p, data.frame(species = "Added", w_max = 100), steady = FALSE
            ), "support|transformation")
        })
    }
})

test_that("MR biomass distinguishes unsupported filters from invalid inputs", {
    p <- small_mr_model()
    for (scheme in c(FALSE, TRUE)) {
        second_order_w(p) <- c(bin_average = scheme)
        expect_error(getBiomass(p, include = "component:MR", min_l = -1),
                     "length limits must be non-negative")
        expect_error(getBiomass(p, include = "component:MR", min_l = NA_real_),
                     "must not be NA")
        unsupported <- getBiomass(p, include = "component:MR", min_w = c(1, 2))
        expect_true(all(!attr(unsupported, "coverage")$available))
        state <- p$initial_n_other
        state$MR[1, 1] <- -1
        expect_error(getBiomass(p, include = "component:MR", n_other = state),
                     "non-negative resource abundances")
        q <- p
        q$resource_params$b <- -1
        expect_error(getBiomass(q, include = "component:MR", min_l = 1),
                     "positive finite weight-length")
    }
})
