test_that("Can reproduce single resource behaviour", {
    rp <- as.data.frame(NS_params@resource_params)
    rp$resource <- "main"
    initial <- array(NS_params@initial_n_pp,
                     dim = c(1, length(NS_params@w_full)))
    params <- setMultipleResources(NS_params, resource_params = rp,
                                   initial_resource = initial)
    expect_equal(getEncounter(params), getEncounter(NS_params),
                 ignore_attr = TRUE)
    expect_equal(getResourceMort(params)[1, ], getResourceMort(NS_params),
                 ignore_attr = TRUE)
    sim <- project(params, t_max = 10, dt = 1)
    simo <- project(NS_params, t_max = 10, dt = 1)
    expect_equal(sim@n_other[[1, "MR"]][1, ], simo@n_pp[1, ])
    expect_equal(sim@n_other[[11, "MR"]][1, ], simo@n_pp[11, ])

    # test that extension field in metadata is set, including the version stamp
    expect_equal(unname(getMetadata(params)$extensions[["mizerMR"]][["requirement"]]),
                 "sizespectrum/mizerMR")
    expect_equal(unname(getMetadata(params)$extensions[["mizerMR"]][["version"]]),
                 as.character(utils::packageVersion("mizerMR")))
    expect_equal(params@rates_funcs$Encounter, "mizerEncounter")
    expect_equal(params@rates_funcs$ResourceMort, "mizerResourceMort")
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
    expect_equal(getEncounter(params), getEncounter(NS_params),
                 ignore_attr = TRUE)
    expect_equal(getResourceMort(params)[1, ], getResourceMort(NS_params),
                 ignore_attr = TRUE)
    sim <- project(params, t_max = 10)
    simo <- project(NS_params, t_max = 10)
    expect_equal(sim@n_other[[1, "MR"]][1, ], simo@n_pp[1, ] / 2)
    expect_equal(sim@n_other[[11, "MR"]][1, ], simo@n_pp[11, ] / 2)
})

test_that("Test setting of single resource", {
    params <- NS_params
    rp <- as.data.frame(params@resource_params)
    rp$resource <- "main"
    resource_params(params) <- rp
    # The above should do the same as a call to setMultipleResources
    expect_unchanged(params,
                     setMultipleResources(params, resource_params = rp))
    # The initial resource should be at carrying capacity
    expect_identical(initialNResource(params),
                     resource_capacity(params))
    # The initial values, once set, should not be changed
    initialNResource(params) <- initialNResource(params) / 2
    expect_unchanged(params,
                     setMultipleResources(params))
    rate <- resource_rate(params) / 2
    comment(rate) <- "set manually"
    resource_rate(params) <- rate
    expect_identical(resource_rate(params), rate)
    capacity <- resource_capacity(params) / 2
    comment(capacity) <- "set manually"
    resource_capacity(params) <- capacity
    expect_identical(resource_capacity(params), capacity)
    interaction <- resource_interaction(params) / 2
    resource_interaction(params) <- interaction
    expect_identical(resource_interaction(params), interaction)
    initial <- initialNResource(params) / 2
    initialNResource(params) <- initial
    expect_identical(initialNResource(params), initial)
    expect_unchanged(setMultipleResources(NS_params,
                                          resource_params = rp,
                                          resource_interaction = interaction,
                                          resource_capacity = capacity,
                                          resource_rate = rate,
                                          initial_resource = initial),
                     params)
})

test_that("Test setting of two resources", {
    params <- NS_params
    rp <- as.data.frame(NS_params@resource_params)
    rp <- rbind(rp, rp)
    rp$resource <- c("res1", "res2")
    resource_params(params) <- rp
    expect_unchanged(params,
                     setMultipleResources(NS_params, resource_params = rp))
    rate <- resource_rate(params) / 2
    comment(rate) <- "set manually"
    resource_rate(params) <- rate
    expect_identical(resource_rate(params), rate)
    capacity <- resource_capacity(params) / 2
    comment(capacity) <- "set manually"
    resource_capacity(params) <- capacity
    expect_identical(resource_capacity(params), capacity)
    interaction <- resource_interaction(params) / 2
    resource_interaction(params) <- interaction
    expect_identical(resource_interaction(params), interaction)
    initial <- initialNResource(params) / 2
    initialNResource(params) <- initial
    expect_identical(initialNResource(params), initial)
    expect_unchanged(setMultipleResources(NS_params,
                                          resource_params = rp,
                                          resource_interaction = interaction,
                                          resource_capacity = capacity,
                                          resource_rate = rate,
                                          initial_resource = initial),
                     params)
})

test_that("We can set species_params and gear_params", {
    rp <- as.data.frame(NS_params@resource_params)
    rp$resource <- "main"
    params <- setMultipleResources(NS_params, resource_params = rp)
    species_params(params)$h <- 1
    expect_identical(species_params(params)$h[[1]], 1)
    gear_params(params)$catchability <- 2
    expect_identical(gear_params(params)$catchability[[1]], 2)
})

test_that("Work with non-MR objects", {
    expect_identical(resource_params(NS_params),
                     mizer::resource_params(NS_params))
    expect_identical(initialNResource(NS_params),
                     mizer::initialNResource(NS_params))
    expect_identical(resource_capacity(NS_params),
                     mizer::resource_capacity(NS_params))
    expect_identical(resource_rate(NS_params),
                     mizer::resource_rate(NS_params))
})

test_that("mizerMR methods fall back before MR component is installed", {
    params <- NS_params
    params@extensions <- mizer::getRegisteredExtensions()
    params <- mizer::coerceToExtensionClass(params)
    expect_identical(initialNResource(params),
                     mizer::initialNResource(NS_params))
    initial <- 0 * mizer::initialNResource(NS_params)
    initialNResource(params) <- initial
    expect_identical(mizer::initialNResource(params), initial)
})

test_that("Multiple resources compose with outer encounter methods", {
    projectEncounter.doubleSearch <- function(params, ...) {
        params@search_vol <- 2 * params@search_vol
        NextMethod()
    }
    assign("projectEncounter.doubleSearch", projectEncounter.doubleSearch,
           envir = .GlobalEnv)
    on.exit(rm(projectEncounter.doubleSearch, envir = .GlobalEnv),
            add = TRUE)

    rp <- as.data.frame(NS_params@resource_params)
    rp$resource <- "main"
    initial <- array(NS_params@initial_n_pp,
                     dim = c(1, length(NS_params@w_full)))
    params <- setMultipleResources(NS_params, resource_params = rp,
                                   initial_resource = initial)
    encounter <- getEncounter(params)

    params@extensions <- c(doubleSearch = NA_character_, params@extensions)
    mizer::registerExtensions(params@extensions)
    params <- mizer::coerceToExtensionClass(params)

    expect_equal(getEncounter(params), 2 * encounter, ignore_attr = TRUE)

    old_extensions <- params@extensions
    resource_rate(params) <- resource_rate(params)
    expect_identical(params@extensions, old_extensions)
})
