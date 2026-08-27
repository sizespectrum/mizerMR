test_that("NResource and finalNResource return right dimensions", {
    params <- NS_params
    rp <- as.data.frame(params@resource_params)
    rp <- rbind(rp, rp)
    rp$resource <- c("res1", "res2")
    params <- setMultipleResources(params, rp)
    sim <- project(params, t_max = 0.2, t_save = 0.1)
    expect_identical(dim(NResource(sim)),
                     as.integer(c(3, 2, length(params@w_full))))
    expect_identical(dim(finalNResource(sim)),
                     as.integer(c(2, length(params@w_full))))
})

test_that("NResource and finalNResource work with non-MR sim", {
    expect_identical(NResource(NS_sim), mizer::NResource(NS_sim))
    expect_identical(finalNResource(NS_sim), mizer::finalNResource(NS_sim))
})

test_that("animate and animateSpectra work", {
    params <- NS_params
    rp <- as.data.frame(params@resource_params)
    rp <- rbind(rp, rp)
    rp$resource <- c("res1", "res2")
    params <- setMultipleResources(params, rp)
    sim <- project(params, t_max = 0.2, t_save = 0.1)
    expect_error(animateSpectra(sim), NA)
    expect_error(animate(sim), NA)
})

test_that("animate follows the mizer 3.3 spectrum interface", {
    params <- NS_params
    rp <- as.data.frame(params@resource_params)
    rp <- rbind(rp, rp)
    rp$resource <- c("res1", "res2")
    params <- setMultipleResources(params, rp)
    sim <- project(params, t_max = 0.2, t_save = 0.1)
    expect_s3_class(animate(sim, size_axis = "l"), "plotly")
    expect_s3_class(animate(sim, per_log_size = TRUE, total = TRUE), "plotly")
    expect_s3_class(animate(sim, biomass = FALSE), "plotly")
    expect_error(animate(sim, power = 2, biomass = FALSE),
                 "not contradictory values of both")
})
