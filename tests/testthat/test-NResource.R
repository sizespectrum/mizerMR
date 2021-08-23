test_that("NResource", {
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
