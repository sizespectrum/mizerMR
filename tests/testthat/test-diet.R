# Initialise ----
# Create an example oarans object with two identical resources, each at half
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

# getDiet ----
test_that("getDiet() returns the right dimensions", {
    expect_equal(dim(getDiet(params)), c(12, 100, 14))
})

# plotDiet ----
# Need to use vdiffr conditionally
expect_doppelganger <- function(title, fig, ...) {
    testthat::skip_if_not_installed("vdiffr")
    vdiffr::expect_doppelganger(title, fig, ...)
}

test_that("plotDiet plot has not changed", {
    p <- plotDiet(params, species = "Cod")
    expect_doppelganger("Plot Diet", p)
})

test_that("plotDiet returns the right dimensions", {
    expect_equal(dim(plotDiet(params, return_data = TRUE)), c(6080, 4))
    expect_equal(names(plotDiet(params, return_data = TRUE)),
                 c("w", "Proportion", "Prey", "Predator"))
    sim <- project(params, t_max = 0.1)
    expect_equal(dim(plotDiet(sim, return_data = TRUE)), c(6080, 4))
    expect_equal(names(plotDiet(sim, return_data = TRUE)),
                 c("w", "Proportion", "Prey", "Predator"))
})

test_that("old plotDietMR() alias still works", {
    expect_equal(plotDietMR(params, return_data = TRUE),
                 plotDiet(params, return_data = TRUE))
})

test_that("the diet is consistent with the encounter rate", {
    # Summed over prey, the diet must reproduce
    # `getEncounter() * (1 - getFeedingLevel())` under both quadrature schemes,
    # which is the identity mizer restored in version 3.3.
    for (second_order in c(FALSE, TRUE)) {
        p <- newMRParams(NS_species_params,
                         resource_params = rp[, c("resource", "kappa",
                                                  "lambda", "r_pp")],
                         second_order_w = second_order, info_level = 0)
        consumed <- rowSums(getDiet(p, proportion = FALSE), dims = 2)
        expected <- getEncounter(p) * (1 - getFeedingLevel(p)) *
            (initialN(p) > 0)
        expect_equal(consumed, expected, ignore_attr = TRUE)
    }
})

test_that("plotDiet can be drawn against a length axis", {
    df_w <- plotDiet(params, species = "Cod", return_data = TRUE)
    df_l <- plotDiet(params, species = "Cod", size_axis = "l",
                     return_data = TRUE)
    expect_named(df_l, c("l", "Proportion", "Prey", "Predator"))
    sp <- params@species_params
    cod <- sp[sp$species == "Cod", ]
    expect_equal(as.numeric(df_l$l),
                 (as.numeric(df_w$w) / cod$a)^(1 / cod$b))
    # A proportion is not a density, so its values are unchanged.
    expect_equal(df_l$Proportion, df_w$Proportion)
    expect_s3_class(plotDiet(params, species = "Cod", size_axis = "l"), "gg")
})

test_that("plotDiet respects the size limits", {
    df <- plotDiet(params, species = "Cod", wlim = c(10, 1000),
                   return_data = TRUE)
    expect_true(all(df$w >= 10 & df$w <= 1000))
    df <- plotDiet(params, species = "Cod", size_axis = "l", llim = c(10, 50),
                   return_data = TRUE)
    expect_true(all(df$l >= 10 & df$l <= 50))
})
