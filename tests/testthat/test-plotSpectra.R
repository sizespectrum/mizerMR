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

# Need to use vdiffr conditionally
expect_doppelganger <- function(title, fig, ...) {
    testthat::skip_if_not_installed("vdiffr")
    vdiffr::expect_doppelganger(title, fig, ...)
}

# plotSpectra ----
test_that("plotSpectra plot has not changed", {
    p <- plotSpectra(params)
    expect_doppelganger("plotSpectra params", p)
})

test_that("plotSpectra works with MizerSim", {
    sim <- project(params, t_max = 0.1)
    p <- plotSpectra(sim)
    expect_doppelganger("plotSpectra sim", p)
})

test_that("plotSpectra return_data has expected columns", {
    df <- plotSpectra(params, return_data = TRUE)
    expect_true(all(c("w", "value", "Spectra", "Legend") %in% names(df)))
})

# plotlySpectra ----
test_that("plotlySpectra does not throw an error", {
    expect_error(plotlySpectra(params), NA)
})

test_that("plotlySpectra does not throw an error with MizerSim", {
    sim <- project(params, t_max = 0.1)
    expect_error(plotlySpectra(sim), NA)
})
