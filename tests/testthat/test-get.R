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
    expect_equal(dim(getDietMR(params)),c(12,100,14))
})
