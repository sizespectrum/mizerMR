# Tests for valid_resources_arg() and getDiet(proportion = FALSE)

make_mr_params <- function() {
    rp <- data.frame(
        resource = c("res1", "res2"),
        kappa = c(0.1, 0.05),
        lambda = c(2.05, 2.1),
        r_pp = c(4, 2),
        stringsAsFactors = FALSE
    )
    setMultipleResources(NS_params, resource_params = rp)
}

# valid_resources_arg ----------------------------------------------------------

test_that("valid_resources_arg returns all resources when resources = NULL", {
    params <- make_mr_params()
    expect_equal(valid_resources_arg(params), c("res1", "res2"))
})

test_that("valid_resources_arg accepts a character vector of valid names", {
    params <- make_mr_params()
    expect_equal(valid_resources_arg(params, resources = "res1"), "res1")
    expect_equal(valid_resources_arg(params, resources = c("res2", "res1")),
                 c("res2", "res1"))
})

test_that("valid_resources_arg warns on unknown names and drops them", {
    params <- make_mr_params()
    expect_warning(r <- valid_resources_arg(params, resources = c("res1", "bad")),
                   "do not exist")
    expect_equal(r, "res1")
})

test_that("valid_resources_arg errors when all names are invalid", {
    params <- make_mr_params()
    expect_error(suppressWarnings(
        valid_resources_arg(params, resources = "bad")),
        "matches none")
})

test_that("valid_resources_arg accepts a numeric index vector", {
    params <- make_mr_params()
    expect_equal(valid_resources_arg(params, resources = 2), "res2")
    expect_equal(valid_resources_arg(params, resources = c(1, 2)), c("res1", "res2"))
})

test_that("valid_resources_arg warns (and errors) on out-of-range numeric indices", {
    params <- make_mr_params()
    # An index beyond the number of resources triggers a warning; when no
    # valid indices remain it also errors.
    expect_error(
        expect_warning(valid_resources_arg(params, resources = 5),
                       "integers 1 to"),
        "valid resource indices"
    )
})

test_that("valid_resources_arg accepts a logical vector", {
    params <- make_mr_params()
    expect_equal(valid_resources_arg(params, resources = c(TRUE, FALSE)), "res1")
    expect_equal(valid_resources_arg(params, resources = c(FALSE, TRUE)), "res2")
})

test_that("valid_resources_arg errors on a logical vector of wrong length", {
    params <- make_mr_params()
    expect_error(valid_resources_arg(params, resources = c(TRUE, FALSE, TRUE)),
                 "wrong length")
})

test_that("valid_resources_arg return.logical = TRUE gives a logical vector", {
    params <- make_mr_params()
    r <- valid_resources_arg(params, resources = "res1", return.logical = TRUE)
    expect_equal(r, c(TRUE, FALSE))
    expect_equal(valid_resources_arg(params, return.logical = TRUE),
                 c(TRUE, TRUE))
})

test_that("valid_resources_arg errors on a non-MizerSim/MizerParams object", {
    expect_error(valid_resources_arg(42), "MizerSim or MizerParams")
})

test_that("valid_resources_arg works with a MizerSim object", {
    params <- make_mr_params()
    sim <- project(params, t_max = 0.1)
    expect_equal(valid_resources_arg(sim), c("res1", "res2"))
})

# getDiet proportion = FALSE ---------------------------------------------------

test_that("getDiet(proportion = FALSE) returns absolute consumption rates", {
    params <- make_mr_params()
    d_abs <- getDiet(params, proportion = FALSE)
    d_prop <- getDiet(params, proportion = TRUE)

    # Proportions sum to 1 (or 0) per predator x size; rates do not
    row_sums_prop <- apply(d_prop, c(1, 2), sum)
    row_sums_abs  <- apply(d_abs,  c(1, 2), sum)
    expect_true(all(abs(row_sums_prop[row_sums_prop > 0] - 1) < 1e-10))
    # At least some absolute rate totals differ from 1
    expect_false(isTRUE(all.equal(row_sums_abs[row_sums_abs > 0],
                                  rep(1, sum(row_sums_abs > 0)))))
})

test_that("getDiet(proportion = FALSE) returns non-negative values", {
    params <- make_mr_params()
    d <- getDiet(params, proportion = FALSE)
    expect_true(all(d >= 0))
})

test_that("getDiet(proportion = FALSE) has same dim as proportion = TRUE", {
    params <- make_mr_params()
    expect_equal(dim(getDiet(params, proportion = FALSE)),
                 dim(getDiet(params, proportion = TRUE)))
})
