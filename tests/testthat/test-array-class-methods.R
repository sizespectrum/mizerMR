# Tests for print, summary, str and plotHover on the MRArray* classes.

make_mr_array_fixture <- function() {
    rp <- data.frame(
        resource = c("res1", "res2"),
        kappa = c(0.1, 0.05),
        lambda = c(2.05, 2.1),
        r_pp = c(4, 2),
        stringsAsFactors = FALSE
    )
    params <- setMultipleResources(NS_params, resource_params = rp)
    sim <- project(params, t_max = 0.2, t_save = 0.1)
    list(params = params, sim = sim,
         mort = getResourceMort(params),
         nr   = NResource(sim))
}

fix <- make_mr_array_fixture()

# Constructors -----------------------------------------------------------------

test_that("MRArrayResourceBySize constructor errors on non-matrix input", {
    expect_error(MRArrayResourceBySize(1:4), "matrix")
    expect_error(MRArrayResourceBySize(array(1:8, dim = c(2, 2, 2))), "matrix")
})

test_that("MRArrayTimeByResourceBySize constructor errors on non-3d array", {
    expect_error(MRArrayTimeByResourceBySize(matrix(1:4, 2, 2)), "three-dimensional")
    expect_error(MRArrayTimeByResourceBySize(1:4), "three-dimensional")
})

# print methods ----------------------------------------------------------------

test_that("print.MRArrayResourceBySize outputs header with dimensions and units", {
    out <- capture.output(print(fix$mort))
    expect_match(out[[1]], "2 resources")
    expect_match(out[[1]], "1/year")
    # One line per resource with min/mean/max
    expect_true(any(grepl("res1", out)))
    expect_true(any(grepl("res2", out)))
    expect_true(any(grepl("min=", out)))
})

test_that("print.MRArrayResourceBySize is invisible", {
    expect_invisible(print(fix$mort))
})

test_that("print.MRArrayTimeByResourceBySize outputs header with all three dims", {
    out <- capture.output(print(fix$nr))
    expect_match(out[[1]], "times")
    expect_match(out[[1]], "resources")
    expect_match(out[[1]], "sizes")
    expect_true(any(grepl("min=", out)))
})

test_that("print.MRArrayTimeByResourceBySize is invisible", {
    expect_invisible(print(fix$nr))
})

test_that("print.MRArrayResourceBySize handles all-NA/Inf values", {
    m <- fix$mort
    m[] <- NA
    out <- capture.output(print(MRArrayResourceBySize(unclass(m),
                                                      value_name = "Test")))
    expect_true(any(grepl("all NA/Inf", out)))
})

# summary methods --------------------------------------------------------------

test_that("summary.MRArrayResourceBySize returns correct class", {
    s <- summary(fix$mort)
    expect_s3_class(s, "summary.MRArrayResourceBySize")
})

test_that("summary.MRArrayResourceBySize has one row per resource", {
    s <- summary(fix$mort)
    expect_equal(nrow(s$per_resource), 2L)
    expect_equal(s$per_resource$Resource, c("res1", "res2"))
})

test_that("print.summary.MRArrayResourceBySize produces output with units", {
    out <- capture.output(print(summary(fix$mort)))
    expect_true(any(grepl("1/year", out)))
    expect_true(any(grepl("res1", out)))
})

test_that("summary.MRArrayTimeByResourceBySize has one row per resource", {
    s <- summary(fix$nr)
    expect_s3_class(s, "summary.MRArrayTimeByResourceBySize")
    expect_equal(nrow(s$per_resource), 2L)
})

test_that("print.summary.MRArrayTimeByResourceBySize mentions all three dims", {
    out <- capture.output(print(summary(fix$nr)))
    expect_true(any(grepl("times", out)))
    expect_true(any(grepl("resources", out)))
    expect_true(any(grepl("sizes", out)))
})

# str methods ------------------------------------------------------------------

test_that("str.MRArrayResourceBySize mentions the class name", {
    out <- capture.output(str(fix$mort))
    expect_match(out[[1]], "MRArrayResourceBySize")
})

test_that("str.MRArrayTimeByResourceBySize mentions the class name", {
    out <- capture.output(str(fix$nr))
    expect_match(out[[1]], "MRArrayTimeByResourceBySize")
})

# plotHover methods ------------------------------------------------------------

test_that("plotHover.MRArrayResourceBySize returns a plotly object", {
    skip_if_not_installed("plotly")
    p <- plotHover(fix$mort)
    expect_s3_class(p, "plotly")
})

test_that("plotHover.MRArrayTimeByResourceBySize returns a plotly object", {
    skip_if_not_installed("plotly")
    p <- plotHover(fix$nr)
    expect_s3_class(p, "plotly")
})
