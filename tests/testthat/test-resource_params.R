test_that("validResourceParams sets defaults as promised", {
    params <- NS_params
    rp_NS <- NS_params@resource_params
    rp_empty <- data.frame(resource = "res1")
    rp <- validResourceParams(rp_empty, params)
    expect_identical(rp$kappa, rp_NS$kappa)
    expect_identical(rp$lambda, rp_NS$lambda)
    expect_identical(rp$r_pp, rp_NS$r_pp)
    expect_identical(rp$n, rp_NS$n)
    expect_identical(rp$w_max, rp_NS$w_pp_cutoff)
    expect_identical(rp$w_min, params@w_full[[1]])


    rp$kappa <- 2
    resource_params(params) <- rp
    rp2 <- rbind(rp, rp)
    rp2[2, ] <- NA
    rp2[2, 1] <- "res2"
    rp <- validResourceParams(rp2, params)
    vars <- c("kappa", "lambda", "r_pp", "n", "w_min", "w_max", "dynamics",
              "linetype")
    expect_identical(rp[2, vars], rp[1, vars])
})
