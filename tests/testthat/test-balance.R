# Tests for the balancing of the resources, see R/balance.R

# A two-resource model with different size ranges whose resources are at half
# their carrying capacity, so that there is something to balance.
mr_test_params <- function(dynamics = "resource_semichemostat",
                           w_max = c(1e-2, 10)) {
    rp <- data.frame(resource = c("small", "large"),
                     kappa = c(0.05, 0.2),
                     lambda = c(2.05, 2),
                     r_pp = c(4, 2),
                     w_min = c(NA, 1e-4),
                     w_max = w_max,
                     dynamics = dynamics,
                     stringsAsFactors = FALSE)
    params <- setMultipleResources(NS_params, resource_params = rp)
    initialNResource(params) <- initialNResource(params) / 2
    params
}

# The resource abundances after one time step of the resource dynamics. They
# are unchanged precisely when the resources are balanced.
mr_step <- function(params, dt = 1) {
    n_other <- initialNOther(params)
    unclass(mizerMR_dynamics(params, n = initialN(params),
                             n_pp = params@initial_n_pp,
                             n_other = n_other,
                             rates = getRates(params), t = 0, dt = dt))
}

# Is the model at a steady state of the resource dynamics?
expect_resources_balanced <- function(params) {
    expect_equal(mr_step(params), unclass(initialNResource(params)),
                 ignore_attr = TRUE)
}

test_that("Setting the resource level balances the resources", {
    params <- mr_test_params()
    expect_false(isTRUE(all.equal(mr_step(params),
                                  unclass(initialNResource(params)),
                                  check.attributes = FALSE)))

    resource_level(params) <- 0.4
    level <- unclass(resource_level(params))
    expect_equal(unname(level[!is.nan(level)]),
                 rep(0.4, sum(!is.nan(level))))
    # The resources are now at a steady state
    expect_resources_balanced(params)
    # and the semichemostat balance equation holds
    NR <- unclass(initialNResource(params))
    mort <- unclass(getResourceMort(params))
    expect_equal(unclass(resource_rate(params)) *
                     (unclass(resource_capacity(params)) - NR),
                 mort * NR, ignore_attr = TRUE)
})

test_that("Setting the resource capacity balances the rate", {
    params <- mr_test_params()
    capacity <- unclass(resource_capacity(params)) * 4
    resource_capacity(params) <- capacity
    expect_equal(unclass(resource_capacity(params)), capacity,
                 ignore_attr = TRUE)
    expect_resources_balanced(params)
})

test_that("Setting the resource rate balances the capacity", {
    params <- mr_test_params()
    rate <- unclass(resource_rate(params)) * 3
    resource_rate(params) <- rate
    expect_equal(unclass(resource_rate(params)), rate, ignore_attr = TRUE)
    expect_resources_balanced(params)
    # Outside the size range of a resource there is nothing to balance
    outside <- unclass(resource_capacity(params))[1, ] > 0
    expect_true(any(outside))
    expect_equal(unclass(resource_capacity(params))[1, !outside],
                 rep(0, sum(!outside)), ignore_attr = TRUE)
})

test_that("Balancing works for logistic resource dynamics", {
    params <- mr_test_params(dynamics = "resource_logistic")
    resource_level(params) <- 0.4
    expect_resources_balanced(params)
    # The logistic balance equation holds where the resource lives
    NR <- unclass(initialNResource(params))
    cc <- unclass(resource_capacity(params))
    mort <- unclass(getResourceMort(params))
    sel <- NR > 0
    expect_equal((unclass(resource_rate(params)) * (1 - NR / cc))[sel],
                 mort[sel])
})

test_that("Resources whose dynamics cannot be balanced are left alone", {
    params <- mr_test_params(dynamics = c("resource_semichemostat",
                                          "resource_constant"))
    rate <- unclass(resource_rate(params))
    expect_message(resource_level(params) <- 0.4, "were not balanced")
    # The constant resource keeps its rate, which its dynamics does not use
    expect_equal(unclass(resource_rate(params))[2, ], rate[2, ],
                 ignore_attr = TRUE)
    # The first resource is balanced
    expect_false(isTRUE(all.equal(unclass(resource_rate(params))[1, ],
                                  rate[1, ], check.attributes = FALSE)))
    expect_equal(mr_step(params)[1, ],
                 unclass(initialNResource(params))[1, ], ignore_attr = TRUE)
})

test_that("balance = FALSE switches the balancing off", {
    params <- mr_test_params()
    rate <- unclass(resource_rate(params))
    resource_level(params, balance = FALSE) <- 0.4
    expect_equal(unclass(resource_rate(params)), rate, ignore_attr = TRUE)
    level <- unclass(resource_level(params))
    expect_equal(unname(level[!is.nan(level)]), rep(0.4, sum(!is.nan(level))))

    # and balance = TRUE switches it on even when nothing is given
    params <- mr_test_params()
    p2 <- setMultipleResources(params, balance = TRUE)
    expect_equal(unclass(resource_rate(p2)), unclass(resource_rate(params)),
                 ignore_attr = TRUE)
    expect_resources_balanced(p2)
})

test_that("Creating the resources does not balance them", {
    rp <- data.frame(resource = c("small", "large"),
                     w_max = c(1e-2, 10))
    params <- setMultipleResources(NS_params, resource_params = rp)
    # The resources start at their carrying capacity
    expect_equal(unclass(initialNResource(params)),
                 unclass(resource_capacity(params)), ignore_attr = TRUE)
    expect_null(comment(resource_rate(params)))
    expect_null(comment(resource_capacity(params)))
})

test_that("Balanced values are kept by later calls", {
    params <- mr_test_params()
    resource_level(params) <- 0.4
    rate <- unclass(resource_rate(params))
    expect_identical(comment(resource_rate(params)), "set by balancing")
    p2 <- setMultipleResources(params)
    expect_equal(unclass(resource_rate(p2)), rate, ignore_attr = TRUE)
    # but reset = TRUE returns to the values from the resource parameters
    p3 <- setMultipleResources(params, reset = TRUE)
    expect_null(comment(resource_rate(p3)))
    expect_equal(unclass(resource_rate(p3)),
                 unclass(resource_rate(mr_test_params())), ignore_attr = TRUE)
    expect_warning(setMultipleResources(params, reset = TRUE,
                                        resource_level = 0.5),
                   "will be ignored")
})

test_that("The resource level can be given in several shapes", {
    params <- mr_test_params()
    p1 <- setMultipleResources(params, resource_level = c(0.25, 0.5))
    level <- unclass(resource_level(p1))
    sel <- !is.nan(level[1, ])
    expect_equal(unname(level[1, sel]), rep(0.25, sum(sel)))
    sel <- !is.nan(level[2, ])
    expect_equal(unname(level[2, sel]), rep(0.5, sum(sel)))

    full <- 0 * unclass(initialNResource(params)) + 0.3
    p2 <- setMultipleResources(params, resource_level = full)
    level <- unclass(resource_level(p2))
    expect_equal(unname(level[!is.nan(level)]), rep(0.3, sum(!is.nan(level))))
})

test_that("Invalid resource levels are rejected", {
    params <- mr_test_params()
    expect_error(setMultipleResources(params, resource_level = 0,
                                      balance = FALSE),
                 "must always be greater than 0")
    expect_error(setMultipleResources(params, resource_level = 1.5,
                                      balance = FALSE),
                 "must always be greater than 0")
    expect_error(setMultipleResources(params, resource_level = NaN,
                                      balance = FALSE),
                 "must be defined everywhere")
    expect_error(setMultipleResources(params, resource_level = rep(0.5, 3),
                                      balance = FALSE),
                 "should be a single number")
    expect_error(setMultipleResources(params, resource_level = 0.5,
                                      resource_capacity =
                                          resource_capacity(params)),
                 "only either")
    # Giving both switches the balancing off, so both are simply set, but
    # asking for balancing as well is a contradiction.
    expect_error(setMultipleResources(params, resource_level = 0.5,
                                      resource_rate = resource_rate(params),
                                      balance = TRUE),
                 "only provide either")
})

test_that("The resource level needs initial abundances", {
    rp <- data.frame(resource = "one")
    expect_error(setMultipleResources(NS_params, resource_params = rp,
                                      resource_level = 0.5),
                 "already have initial abundances")
})

test_that("balanceResources() checks its arguments", {
    params <- mr_test_params()
    expect_error(balanceResources(params), "were NULL")
    expect_error(balanceResources(params,
                                  resource_rate = resource_rate(params),
                                  resource_capacity =
                                      resource_capacity(params)),
                 "only provide either")
    expect_error(balanceResources(NS_params, resource_rate = 1),
                 "does not have multiple resources")
    expect_error(balanceResources(params, resource_rate = 1),
                 "should be an array with dim")
    expect_error(balanceResources(params, resource_capacity = 1),
                 "should be an array with dim")
})

test_that("The resource level accessors fall back on mizer", {
    expect_equal(resource_level(NS_params), mizer::resource_level(NS_params))
    params <- NS_params
    resource_level(params) <- 0.5
    expected <- mizer::setResource(NS_params, resource_level = 0.5)
    expect_equal(resource_rate(params), mizer::resource_rate(expected))
    expect_equal(resource_capacity(params), mizer::resource_capacity(expected))
})

test_that("A single resource is balanced exactly as mizer balances its own", {
    rp <- as.data.frame(NS_params@resource_params)
    rp$resource <- "main"
    initial <- array(NS_params@initial_n_pp,
                     dim = c(1, length(NS_params@w_full)))
    params <- setMultipleResources(NS_params, resource_params = rp,
                                   initial_resource = initial)
    resource_level(params) <- 0.5
    expected <- mizer::setResource(NS_params, resource_level = 0.5)
    # Only within the size range of the resource: outside it mizer keeps its
    # rate at the power law while mizerMR keeps it at zero.
    sel <- unclass(resource_capacity(params))[1, ] > 0
    expect_true(sum(sel) > 10)
    expect_equal(unclass(resource_rate(params))[1, sel],
                 unclass(mizer::resource_rate(expected))[sel],
                 ignore_attr = TRUE)
    expect_equal(unclass(resource_capacity(params))[1, sel],
                 unclass(mizer::resource_capacity(expected))[sel],
                 ignore_attr = TRUE)
})

test_that("Balancing preserves the resources during a projection", {
    params <- mr_test_params()
    resource_level(params) <- 0.4
    # The consumers have not moved yet after a single time step, so the
    # mortality on the resources is still the one they were balanced against
    # and they should not have moved either.
    sim <- project(params, t_max = 0.1, dt = 0.1, t_save = 0.1)
    expect_equal(unclass(finalNResource(sim)),
                 unclass(initialNResource(params)), ignore_attr = TRUE)
})
