# Tests for scaleModel, scaleRates, setResource and summary methods for mizerMR

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
