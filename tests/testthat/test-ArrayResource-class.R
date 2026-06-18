# Tests for the MRArrayResourceBySize / MRArrayTimeByResourceBySize classes.

# A small two-resource model reused across the tests in this file.
mr_params <- local({
    rp <- as.data.frame(NS_params@resource_params)[c(1, 1), ]
    rp$resource <- c("Resource A", "Resource B")
    initial <- array(dim = c(2, length(NS_params@w_full)))
    initial[1, ] <- NS_params@initial_n_pp / 2
    initial[2, ] <- NS_params@initial_n_pp / 2
    setMultipleResources(NS_params, resource_params = rp,
                         initial_resource = initial)
})
mr_sim <- project(mr_params, t_max = 2, t_save = 1)

test_that("resource accessors return the new classes with correct shapes", {
    m <- getResourceMort(mr_params)
    expect_true(is.MRArrayResourceBySize(m))
    expect_equal(dim(m), c(2L, length(mr_params@w_full)))

    inr <- initialNResource(mr_params)
    expect_true(is.MRArrayResourceBySize(inr))

    nr <- NResource(mr_sim)
    expect_true(is.MRArrayTimeByResourceBySize(nr))
    expect_equal(length(dim(nr)), 3L)
    expect_equal(dim(nr)[2], 2L)

    fnr <- finalNResource(mr_sim)
    expect_true(is.MRArrayResourceBySize(fnr))
})

test_that("subsetting a single time step re-wraps as MRArrayResourceBySize", {
    nr <- NResource(mr_sim)
    slice <- nr[idxFinalT(mr_sim), , ]
    expect_true(is.MRArrayResourceBySize(slice))
    expect_equal(unclass(slice), unclass(finalNResource(mr_sim)),
                 ignore_attr = TRUE)
})

test_that("Ops strips the class to a plain array", {
    m <- getResourceMort(mr_params)
    doubled <- m * 2
    expect_false(is.MRArrayResourceBySize(doubled))
    expect_null(attr(doubled, "value_name"))
    expect_equal(as.numeric(doubled), 2 * as.numeric(unclass(m)))

    nr <- NResource(mr_sim)
    expect_false(is.MRArrayTimeByResourceBySize(nr + 1))
})

test_that("as.data.frame returns the expected long format", {
    expect_named(as.data.frame(getResourceMort(mr_params)),
                 c("w", "value", "resource"))
    expect_named(as.data.frame(NResource(mr_sim)),
                 c("time", "resource", "w", "value"))
})

test_that("plot methods draw one coloured line per resource", {
    pd <- plot(getResourceMort(mr_params), return_data = TRUE)
    expect_setequal(unique(pd$Legend), c("Resource A", "Resource B"))

    g <- plot(getResourceMort(mr_params))
    cols <- unique(ggplot2::ggplot_build(g)$data[[1]]$colour)
    expect_equal(length(cols), 2L)
})

test_that("plot methods produce a ggplot", {
    vdiffr::expect_doppelganger("getResourceMort multi-resource",
                                plot(getResourceMort(mr_params)))
    vdiffr::expect_doppelganger("initialNResource multi-resource",
                                plot(initialNResource(mr_params)))
    vdiffr::expect_doppelganger("NResource multi-resource final time",
                                plot(NResource(mr_sim)))
})
