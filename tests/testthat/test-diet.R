# Initialise ----
# Create an example oarans object with two identical resources, each at half
# abundance in the North sea model.
rp <- as.data.frame(NS_params@resource_params)
rp$kappa <- rp$kappa / 2
rp <- rbind(rp, rp)
rp$resource <- c("res1", "res2")
initial <- array(dim = c(2, length(NS_params@w_full)))
initial[1, ] <- NS_params@initial_n_pp / 2
initial[2, ] <- NS_params@initial_n_pp / 2
params <- setMultipleResources(NS_params, resource_params = rp,
                               initial_resource = initial)

# getDietMR ----
test_that("getDietMR() returns the right dimensions", {
    expect_equal(dim(getDietMR(params)), c(12, 100, 14))
})

test_that("getDietMR() agrees with getDiet()", {
    diet <- getDietMR(params)
    expect_equal(diet[, , 1:12], getDiet(NS_params)[, , 1:12])
    diet <- diet[, , 1:13]
    diet[, , 13] <- 2 * diet[, , 13]
    dimnames(diet)$prey[13] <- "Resource"
    expect_equal(diet, getDiet(NS_params))
})

# plotDietMR ----
# Need to use vdiffr conditionally
expect_doppelganger <- function(title, fig, ...) {
    testthat::skip_if_not_installed("vdiffr")
    vdiffr::expect_doppelganger(title, fig, ...)
}

test_that("plotDietMR plot has not changed", {
    p <- plotDietMR(params, species = "Cod")
    expect_doppelganger("Plot Diet", p)
})

test_that("plotDietMR returns the right dimensions", {
    expect_equal(dim(plotDietMR(params, return_data = TRUE)), c(6080, 4))
    sim <- project(params, t_max = 0.1)
    expect_equal(dim(plotDietMR(sim, return_data = TRUE)), c(6080, 4))
})
