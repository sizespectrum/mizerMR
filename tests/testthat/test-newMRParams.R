test_that("newMRParams works", {
    params <- newMultispeciesParams(NS_species_params, NS_interaction,
                                    gear_params = data.frame())
    rp <- as.data.frame(params@resource_params)
    rp <- rbind(rp, rp)
    rp$resource <- c("res1", "res2")
    params <- setMultipleResources(params, rp)

    expect_identical(newMRParams(NS_species_params, NS_interaction,
                                 resource_params = rp),
                     params)
})
