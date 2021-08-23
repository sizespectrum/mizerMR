test_that("Can reproduce single resource behaviour", {
    rp <- as.data.frame(NS_params@resource_params)
    rp$resource <- "main"
    initial <- array(NS_params@initial_n_pp,
                     dim = c(1, length(NS_params@w_full)))
    params <- setMultipleResources(NS_params, resource_params = rp,
                                   initial_resource = initial)
    expect_identical(getEncounter(params), getEncounter(NS_params))
    expect_identical(getResourceMort(params)[1, ], getResourceMort(NS_params))
    sim <- project(params, t_max = 10)
    simo <- project(NS_params, t_max = 10)
    expect_equal(sim@n_other[[1, "MR"]][1, ], simo@n_pp[1, ])
    expect_equal(sim@n_other[[11, "MR"]][1, ], simo@n_pp[11, ])
})

test_that("Works with two identical resources", {
    rp <- as.data.frame(NS_params@resource_params)
    rp$kappa <- rp$kappa / 2
    rp <- rbind(rp, rp)
    rp$resource <- c("res1", "res2")
    initial <- array(dim = c(2, length(NS_params@w_full)))
    initial[1, ] <- NS_params@initial_n_pp / 2
    initial[2, ] <- NS_params@initial_n_pp / 2
    params <- setMultipleResources(NS_params, resource_params = rp,
                                   initial_resource = initial)
    expect_identical(params@initial_n_other[["MR"]][1, ],
                     NS_params@initial_n_pp / 2)
    expect_identical(getEncounter(params), getEncounter(NS_params))
    expect_identical(getResourceMort(params)[1, ], getResourceMort(NS_params))
    sim <- project(params, t_max = 10)
    simo <- project(NS_params, t_max = 10)
    expect_equal(sim@n_other[[1, "MR"]][1, ], simo@n_pp[1, ] / 2)
    expect_equal(sim@n_other[[11, "MR"]][1, ], simo@n_pp[11, ] / 2)
})
