# Initialise ----
# Create an example params object with two identical resources, each at half
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

# getDiet ----
test_that("getDiet() returns the right dimensions", {
    expect_equal(dim(getDiet(params)), c(12, 100, 14))
})

# plotDiet ----
# Need to use vdiffr conditionally
expect_doppelganger <- function(title, fig, ...) {
    testthat::skip_if_not_installed("vdiffr")
    vdiffr::expect_doppelganger(title, fig, ...)
}

test_that("plotDiet plot has not changed", {
    p <- plotDiet(params, species = "Cod")
    expect_doppelganger("Plot Diet", p)
})

test_that("plotDiet returns the right dimensions", {
    expect_equal(dim(plotDiet(params, return_data = TRUE)), c(6080, 4))
})
