# Tests for scaleModel, scaleRates, setResource, tuneSteadyState and summary
# methods for mizerMR

make_mr_params <- function() {
    rp <- data.frame(
        resource = c("small", "large"),
        kappa = c(1e11, 2e11),
        lambda = c(2.05, 1.95),
        r_pp = c(4, 8),
        w_min = c(NA, 1e-3),
        w_max = c(1e-2, 10),
        stringsAsFactors = FALSE
    )
    setMultipleResources(NS_params, resource_params = rp)
}

test_that("scaleModel rescales resources consistently and preserves the steady state", {
    params <- make_mr_params()
    factor <- 3
    scaled <- scaleModel(params, factor = factor)

    expect_s4_class(scaled, "mizerMR")
    # Capacities and abundances scale by the factor, the rate is unchanged.
    expect_equal(resource_capacity(scaled), resource_capacity(params) * factor,
                 ignore_attr = TRUE)
    expect_equal(initialNResource(scaled), initialNResource(params) * factor,
                 ignore_attr = TRUE)
    expect_equal(resource_rate(scaled), resource_rate(params),
                 ignore_attr = TRUE)
    expect_equal(resource_params(scaled)$kappa,
                 resource_params(params)$kappa * factor)
    # Fish biomass scales by the factor.
    expect_equal(getBiomass(scaled), getBiomass(params) * factor,
                 ignore_attr = TRUE)
    # Steady state is preserved: encounter and resource mortality are invariant.
    expect_equal(getEncounter(scaled), getEncounter(params), ignore_attr = TRUE)
    expect_equal(getResourceMort(scaled), getResourceMort(params),
                 ignore_attr = TRUE)
})

test_that("scaleRates rescales the resource replenishment rate", {
    params <- make_mr_params()
    factor <- 2
    scaled <- scaleRates(params, factor = factor)

    expect_s4_class(scaled, "mizerMR")
    expect_equal(resource_rate(scaled), resource_rate(params) * factor,
                 ignore_attr = TRUE)
    # Consumer search volume is scaled by the base method.
    expect_equal(getSearchVolume(scaled), getSearchVolume(params) * factor,
                 ignore_attr = TRUE)
    # Resource capacity is not a rate and is left unchanged.
    expect_equal(resource_capacity(scaled), resource_capacity(params),
                 ignore_attr = TRUE)
})

test_that("setResource warns for resource changes but still works", {
    params <- make_mr_params()
    expect_warning(p2 <- setResource(params, resource_rate = 5),
                   "multiple-resource model")
    expect_s4_class(p2, "mizerMR")
    # The MR resources are untouched.
    expect_equal(resource_rate(p2), resource_rate(params), ignore_attr = TRUE)
    # A dynamics-only call (as used internally by mizer) does not warn.
    expect_no_warning(
        setResource(params, resource_dynamics = "resource_constant"))
})

test_that("summary reports the combined resource extent without warnings", {
    params <- make_mr_params()
    out <- expect_no_warning(capture.output(summary(params)))
    res_line <- grep("maximum size", out, value = TRUE)
    # The resource maximum-size line must be finite (the base method would
    # report -Inf for the silenced built-in resource).
    expect_false(any(grepl("-Inf", res_line)))
})

test_that("the setResource report obeys info_level", {
    params <- make_mr_params()
    # The report is a warning because the user asked for something that is not
    # going to happen, and it is collected by an enclosing `with_info_level()`.
    expect_warning(
        mizer::with_info_level(info_level = 3,
                               setResource(params, resource_rate = 5)),
        "multiple-resource model")
    expect_no_warning(
        mizer::with_info_level(info_level = 0,
                               setResource(params, resource_rate = 5)))
})


# A model whose two resources between them reproduce the North Sea resource, so
# that it starts out close to a steady state of the consumer dynamics.
make_tune_params <- function() {
    rp <- as.data.frame(NS_params@resource_params)
    rp$kappa <- rp$kappa / 2
    rp <- rbind(rp, rp)
    rp$resource <- c("res1", "res2")
    initial <- array(dim = c(2, length(NS_params@w_full)))
    initial[1, ] <- NS_params@initial_n_pp / 2
    initial[2, ] <- NS_params@initial_n_pp / 2
    setMultipleResources(NS_params, resource_params = rp,
                         initial_resource = initial)
}

# The largest relative change of the resources over one year of their own
# dynamics, which vanishes exactly when they are balanced.
mr_resource_drift <- function(params) {
    NR <- unclass(initialNResource(params))
    new <- unclass(mizerMR_dynamics(params, n = initialN(params),
                                    n_pp = params@initial_n_pp,
                                    n_other = initialNOther(params),
                                    rates = getRates(params), t = 0, dt = 1))
    max(abs((new - NR)[NR > 0] / NR[NR > 0]))
}

test_that("tuneSteadyState balances the resources", {
    params <- make_tune_params()
    expect_gt(mr_resource_drift(params), 1e-4)
    tuned <- suppressWarnings(
        suppressMessages(tuneSteadyState(params, t_max = 20,
                                         progress_bar = FALSE)))
    expect_lt(mr_resource_drift(tuned), 1e-10)
    # The component is handed back with its own dynamics
    expect_identical(tuned@other_dynamics[["MR"]], "mizerMR_dynamics")
    # and the convergence diagnostic survives the rebalancing
    expect_true(is.list(attr(tuned, "convergence")))
    # and the resources still project
    expect_no_error(project(tuned, t_max = 0.1, dt = 0.1, t_save = 0.1))
})

test_that("tuneSteadyState does not report the MR component as unhandled", {
    params <- make_tune_params()
    # Other warnings are the model's own business, so only the report about
    # components mizer cannot handle is asserted against.
    suppressWarnings(expect_no_warning(
        suppressMessages(tuneSteadyState(params, t_max = 5,
                                         progress_bar = FALSE)),
        message = "dynamics of their own"))
})
