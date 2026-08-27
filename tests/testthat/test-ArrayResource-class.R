# Tests for the MRArrayResourceBySize / MRArrayTimeByResourceBySize classes.

# A small two-resource model reused across the tests in this file. The two
# resources are given unequal abundances, and unequal predator interactions, so
# that they show up as distinct lines in the snapshot plots (resource mortality
# depends on the interaction rather than on a resource's own abundance).
mr_params <- local({
    rp <- as.data.frame(NS_params@resource_params)[c(1, 1), ]
    rp$resource <- c("Resource A", "Resource B")
    initial <- array(dim = c(2, length(NS_params@w_full)))
    initial[1, ] <- NS_params@initial_n_pp
    initial[2, ] <- NS_params@initial_n_pp / 4
    params <- setMultipleResources(NS_params, resource_params = rp,
                                   initial_resource = initial)
    inter <- resource_interaction(params)
    inter[, "Resource B"] <- inter[, "Resource B"] / 2
    resource_interaction(params) <- inter
    params
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

# mizer 3.3 array conventions --------------------------------------------------

test_that("the arrays declare what kind of value they hold", {
    expect_identical(attr(initialNResource(mr_params), "type"), "density")
    expect_identical(attr(NResource(mr_sim), "type"), "density")
    expect_identical(attr(finalNResource(mr_sim), "type"), "density")
    expect_identical(attr(getResourceMort(mr_params), "type"), "value")
    # The type survives subsetting and slicing.
    expect_identical(attr(initialNResource(mr_params)[1, , drop = FALSE],
                          "type"), "density")
    expect_identical(attr(NResource(mr_sim)[1, , , drop = FALSE], "type"),
                     "density")
})

test_that("a density array can be drawn against a length axis", {
    n <- initialNResource(mr_params)
    df_w <- plot(n, return_data = TRUE)
    df_l <- plot(n, size_axis = "l", return_data = TRUE)
    expect_named(df_l, c("l", "value", "Spectra", "Legend"))
    ab <- get("resource_length_params", envir = asNamespace("mizer"))(mr_params)
    expect_equal(as.numeric(df_l$l), (as.numeric(df_w$w) / ab$a)^(1 / ab$b))
    expect_equal(as.numeric(df_l$value),
                 as.numeric(df_w$value) * ab$b * as.numeric(df_w$w) /
                     as.numeric(df_l$l))
})

test_that("only a density can be shown per logarithmic size", {
    n <- initialNResource(mr_params)
    per_log <- plot(n, per_log_size = TRUE, return_data = TRUE)
    plain <- plot(n, return_data = TRUE)
    expect_equal(as.numeric(per_log$value),
                 as.numeric(plain$value) * as.numeric(plain$w))
    expect_error(plot(getResourceMort(mr_params), per_log_size = TRUE),
                 "holds a value of type")
})

test_that("a rate keeps its values on a length axis", {
    mort <- getResourceMort(mr_params)
    df_w <- plot(mort, return_data = TRUE)
    df_l <- plot(mort, size_axis = "l", return_data = TRUE)
    # No Jacobian: a rate is the same number whichever size it is drawn against.
    expect_equal(as.numeric(df_l$value), as.numeric(df_w$value))
    expect_true("l" %in% names(df_l))
})
