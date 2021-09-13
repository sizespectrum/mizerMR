

# Need to use vdiffr conditionally
expect_doppelganger <- function(title, fig, ...) {
    testthat::skip_if_not_installed("vdiffr")
    vdiffr::expect_doppelganger(title, fig, ...)
}

test_that("plot has not changed", {
    rp <- as.data.frame(NS_params@resource_params)
    rp$kappa <- rp$kappa / 2
    rp <- rbind(rp, rp)
    rp$resource <- c("res1", "res2")
    initial <- array(dim = c(2, length(NS_params@w_full)))
    initial[1, ] <- NS_params@initial_n_pp / 2
    initial[2, ] <- NS_params@initial_n_pp / 2
    params <- setMultipleResources(NS_params, resource_params = rp,
                                   initial_resource = initial)
    sim <- project(params, t_max = 10)
    p <- plotDietMR(sim, species = "Cod")
    expect_doppelganger("Plot Diet", p)
})

test_that("Has the right dimensions",{
    rp <- as.data.frame(NS_params@resource_params)
    rp$kappa <- rp$kappa / 2
    rp <- rbind(rp, rp)
    rp$resource <- c("res1", "res2")
    initial <- array(dim = c(2, length(NS_params@w_full)))
    initial[1, ] <- NS_params@initial_n_pp / 2
    initial[2, ] <- NS_params@initial_n_pp / 2
    params <- setMultipleResources(NS_params, resource_params = rp,
                                   initial_resource = initial)
    sim <- project(params, t_max = 10)
    expect_equal(dim(plotDietMR(sim, returnData = T)),c(13215,4))
})
