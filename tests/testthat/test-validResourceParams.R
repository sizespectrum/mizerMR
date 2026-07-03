# Tests for validResourceParams() and set_resource_param_default()

# validResourceParams: error conditions ----------------------------------------

test_that("validResourceParams errors when 'resource' column is missing", {
    rp <- data.frame(kappa = 0.1, stringsAsFactors = FALSE)
    expect_error(validResourceParams(rp, min_w = 1e-4),
                 "column 'resource'")
})

test_that("validResourceParams errors on duplicate resource names", {
    rp <- data.frame(resource = c("r1", "r1"), stringsAsFactors = FALSE)
    # The error may come either from R's own row.names check or from the
    # explicit stop() in validResourceParams — both are acceptable.
    expect_error(validResourceParams(rp, min_w = 1e-4))
})

test_that("validResourceParams errors when w_min >= w_max", {
    rp <- data.frame(resource = "r1", w_min = 5, w_max = 1,
                     stringsAsFactors = FALSE)
    expect_error(validResourceParams(rp, min_w = 1e-4), "w_min")

    rp2 <- data.frame(resource = "r1", w_min = 1, w_max = 1,
                      stringsAsFactors = FALSE)
    expect_error(validResourceParams(rp2, min_w = 1e-4), "w_min")
})

test_that("validResourceParams errors when w_min < min_w", {
    rp <- data.frame(resource = "r1", w_min = 1e-6, stringsAsFactors = FALSE)
    expect_error(validResourceParams(rp, min_w = 1e-4),
                 "smallest allowed size")
})

test_that("validResourceParams errors on negative kappa, lambda or r_pp", {
    # w_min and w_max negatives are caught by earlier checks; test kappa/lambda/r_pp
    for (par in c("kappa", "lambda", "r_pp")) {
        rp <- data.frame(resource = "r1", stringsAsFactors = FALSE)
        rp[[par]] <- -1
        expect_error(validResourceParams(rp, min_w = 1e-4),
                     "negative", info = paste("parameter:", par))
    }
})

# validResourceParams: defaults ------------------------------------------------

test_that("validResourceParams fills defaults for all missing columns", {
    rp <- data.frame(resource = "r1", stringsAsFactors = FALSE)
    vr <- validResourceParams(rp, min_w = 1e-4)

    expect_equal(vr$kappa,    0.1)
    expect_equal(vr$lambda,   2.05)
    expect_equal(vr$r_pp,     4)
    expect_equal(vr$w_min,    1e-4)
    expect_equal(vr$w_max,    10)
    expect_equal(vr$n,        2 / 3)
    expect_equal(vr$dynamics, "resource_semichemostat")
    expect_equal(vr$linetype, "solid")
    expect_false(is.na(vr$colour))   # colour drawn from palette
})

test_that("validResourceParams fills NAs with defaults but keeps existing values", {
    rp <- data.frame(resource = c("r1", "r2"), kappa = c(1.5, NA),
                     stringsAsFactors = FALSE)
    vr <- validResourceParams(rp, min_w = 1e-4)

    expect_equal(vr$kappa[[1]], 1.5)   # existing value kept
    expect_equal(vr$kappa[[2]], 0.1)   # NA filled with default
})

test_that("validResourceParams assigns distinct colours to multiple resources", {
    rp <- data.frame(resource = c("r1", "r2", "r3"), stringsAsFactors = FALSE)
    vr <- validResourceParams(rp, min_w = 1e-4)
    expect_equal(length(unique(vr$colour)), 3L)
})

# validResourceParams: tibble conversion ----------------------------------------

test_that("validResourceParams converts a tibble to a plain data.frame", {
    skip_if_not_installed("tibble")
    rp <- tibble::tibble(resource = "r1")
    vr <- validResourceParams(rp, min_w = 1e-4)
    expect_s3_class(vr, "data.frame")
    expect_false(inherits(vr, "tbl_df"))
})

# set_resource_param_default ---------------------------------------------------

test_that("set_resource_param_default creates a new column when absent", {
    rp <- data.frame(resource = c("r1", "r2"), stringsAsFactors = FALSE)
    out <- set_resource_param_default(rp, "kappa", 0.5)
    expect_equal(out$kappa, c(0.5, 0.5))
})

test_that("set_resource_param_default fills NA entries only", {
    rp <- data.frame(resource = c("r1", "r2"), kappa = c(2, NA),
                     stringsAsFactors = FALSE)
    out <- set_resource_param_default(rp, "kappa", 0.5)
    expect_equal(out$kappa, c(2, 0.5))
})

test_that("set_resource_param_default accepts a per-resource default vector", {
    rp <- data.frame(resource = c("r1", "r2", "r3"), stringsAsFactors = FALSE)
    out <- set_resource_param_default(rp, "r_pp", c(1, 2, 3))
    expect_equal(out$r_pp, c(1, 2, 3))
})
