test_that("Changing resource_params changes arrays", {
    rp <- as.data.frame(NS_params@resource_params)
    rp <- rbind(rp, rp)
    rp$resource <- c("res1", "res2")
    params <- setMultipleResources(NS_params, resource_params = rp)
    rr <- resource_rate(params)
    rc <- resource_capacity(params)
    r_pp <- resource_params(params)$r_pp
    kappa <- resource_params(params)$kappa

    resource_params(params)$r_pp <- resource_params(params)$r_pp * 2
    expect_identical(resource_params(params)$r_pp, r_pp * 2)
    expect_identical(resource_rate(params), rr * 2)

    resource_params(params)$kappa <- resource_params(params)$kappa * 2
    expect_identical(resource_params(params)$kappa, kappa * 2)
    expect_identical(resource_capacity(params), rc * 2)

    resource_params(params)$w_min <- 1e-3
    expect_identical(resource_capacity(params)[ , 1], c(res1 = 0, res2 = 0))
})
