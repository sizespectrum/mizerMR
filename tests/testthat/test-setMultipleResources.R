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
    expect_equal(initialNResource(params),
                 resource_capacity(params), ignore_attr = TRUE)
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
    expect_equal(initialNResource(params), initial, ignore_attr = TRUE)
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
    expect_equal(initialNResource(params), initial, ignore_attr = TRUE)
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
    expect_equal(initialNResource(params),
                 mizer::initialNResource(NS_params), ignore_attr = TRUE)
    initial <- 0 * mizer::initialNResource(NS_params)
    initialNResource(params) <- initial
    expect_equal(mizer::initialNResource(params), initial, ignore_attr = TRUE)
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

test_that("power_law_bin_average handles a two-sided cutoff", {
    w <- c(1, 2, 4, 8)
    dw <- c(1, 2, 4, 8)
    avg <- power_law_bin_average(w, dw, -2.05, w_min = 2, w_max = 8)
    # Bin [1, 2) is entirely below w_min and bin [8, 16) is at/above w_max.
    expect_equal(avg[[1]], 0)
    expect_equal(avg[[4]], 0)
    expect_true(all(avg[2:3] > 0))
    # An interior in-range bin equals the exact integral over the full bin
    # width.
    num <- integrate(function(x) x^(-2.05), 2, 4)$value / dw[[2]]
    expect_equal(avg[[2]], num, tolerance = 1e-6)
    # The straddling bin gets the partial average over the in-range part only.
    avg_lo <- power_law_bin_average(c(1), c(2), -2.05, w_min = 2, w_max = 8)
    part <- integrate(function(x) x^(-2.05), 2, 3)$value / 2
    expect_equal(avg_lo[[1]], part, tolerance = 1e-6)
    # The exponent == -1 branch.
    expect_equal(power_law_bin_average(w, dw, -1), log((w + dw) / w) / dw)
    # No cutoff reproduces the plain bin average (lower edge at w).
    expect_equal(power_law_bin_average(w, dw, -2),
                 ((w + dw)^(-1) - w^(-1)) / (-1 * dw))
})

test_that("second_order_w bin-averages the multiple resources", {
    skip_if_not("second_order_w" %in% getNamespaceExports("mizer"),
                "installed mizer has no second_order_w support")
    rp <- data.frame(resource = c("res1", "res2"),
                     kappa = c(0.1, 0.05), lambda = c(2.05, 2.1),
                     r_pp = c(4, 2), w_min = c(1e-4, 1e-2),
                     w_max = c(1, 10), n = c(2 / 3, 0.7),
                     stringsAsFactors = FALSE)
    p1 <- setMultipleResources(NS_params, resource_params = rp)
    p2 <- NS_params
    second_order_w(p2) <- c(bin_average = TRUE)
    p2 <- setMultipleResources(p2, resource_params = rp)

    wf <- w_full(NS_params)
    dwf <- dw_full(NS_params)
    cap1 <- getComponent(p1, "MR")$component_params$capacity
    cap2 <- getComponent(p2, "MR")$component_params$capacity
    rate1 <- getComponent(p1, "MR")$component_params$rate
    rate2 <- getComponent(p2, "MR")$component_params$rate

    for (i in 1:2) {
        sel <- wf >= rp$w_min[i] & wf <= rp$w_max[i]
        # First order: point-sampled within the size range, zero outside.
        cap_pt <- numeric(length(wf))
        cap_pt[sel] <- rp$kappa[i] * wf[sel] ^ (-rp$lambda[i])
        expect_equal(unname(cap1[i, ]), cap_pt)
        # Second order: exact two-sided bin average.
        cap_ba <- rp$kappa[i] * power_law_bin_average(
            wf, dwf, -rp$lambda[i], w_min = rp$w_min[i], w_max = rp$w_max[i])
        expect_equal(unname(cap2[i, ]), cap_ba)
        rate_ba <- rp$r_pp[i] * power_law_bin_average(
            wf, dwf, rp$n[i] - 1, w_min = rp$w_min[i], w_max = rp$w_max[i])
        expect_equal(unname(rate2[i, ]), rate_ba)
    }
    # Bin-averaging actually changes the resource (steep power laws).
    expect_false(isTRUE(all.equal(unname(cap1), unname(cap2))))
    expect_false(isTRUE(all.equal(unname(rate1), unname(rate2))))
})

test_that("newMRParams accepts second_order_w", {
    skip_if_not("second_order_w" %in% getNamespaceExports("mizer"),
                "installed mizer has no second_order_w support")
    sp <- data.frame(species = c("A", "B"), w_max = c(100, 1000),
                     stringsAsFactors = FALSE)
    rp <- data.frame(resource = c("res1", "res2"), kappa = c(0.1, 0.05),
                     lambda = c(2.05, 2.1), r_pp = c(4, 2),
                     w_min = c(1e-4, 1e-2), w_max = c(1, 10),
                     stringsAsFactors = FALSE)
    p <- suppressWarnings(suppressMessages(
        newMRParams(sp, resource_params = rp, no_w = 50,
                    second_order_w = TRUE)))
    expect_true(second_order_w(p)[["bin_average"]])
    cap <- getComponent(p, "MR")$component_params$capacity
    expect_true(all(is.finite(cap)))
    expect_true(any(cap > 0))
    # The initial resource inherits the bin-averaged capacity.
    expect_equal(unname(getComponent(p, "MR")$initial_value), unname(cap))
    expect_error(suppressWarnings(suppressMessages(project(p, t_max = 1))), NA)
})
