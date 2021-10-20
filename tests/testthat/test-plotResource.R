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

# Need to use vdiffr conditionally
expect_doppelganger <- function(title, fig, ...) {
    testthat::skip_if_not_installed("vdiffr")
    vdiffr::expect_doppelganger(title, fig, ...)
}

# plots have not changed ----
test_that("plots have not changed", {
p <- plotResourcePred(params)
expect_doppelganger("Plot Resource Pred", p)
p <- plotResource(params)
expect_doppelganger("Plot Resource", p)
})


# plotly functions do not throw error
test_that("plotly functions do not throw error", {
    expect_error(plotlyResourcePred(params), NA)
})

# testing the plot outputs
test_that("return_data is identical",{
    expect_equal(dim(plotResourcePred(params, return_data = TRUE)), c(4296,4))
    expect_equal(dim(plotResource(params, return_data = TRUE)), c(358,3))
}
)
