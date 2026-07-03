# Tests for plotSpectra.mizerMR and plotSpectra.mizerMRSim

make_mr_sim <- function() {
    rp <- data.frame(
        resource = c("res1", "res2"),
        kappa = c(0.1, 0.05),
        lambda = c(2.05, 2.1),
        r_pp = c(4, 2),
        stringsAsFactors = FALSE
    )
    params <- setMultipleResources(NS_params, resource_params = rp)
    sim <- project(params, t_max = 0.2, t_save = 0.1)
    list(params = params, sim = sim)
}

# plotSpectra.mizerMR ----------------------------------------------------------

test_that("plotSpectra.mizerMR return_data has the right column names", {
    params <- make_mr_sim()$params
    df <- plotSpectra(params, return_data = TRUE)
    expect_named(df, c("w", "value", "Spectra", "Legend"))
})

test_that("plotSpectra.mizerMR includes all resources in Spectra column", {
    params <- make_mr_sim()$params
    df <- plotSpectra(params, return_data = TRUE)
    expect_true(all(c("res1", "res2") %in% unique(df$Spectra)))
})

test_that("plotSpectra.mizerMR includes species rows in addition to resources", {
    params <- make_mr_sim()$params
    df <- plotSpectra(params, return_data = TRUE)
    expect_true(any(df$Spectra %in% params@species_params$species))
})

test_that("plotSpectra.mizerMR returns a ggplot when return_data = FALSE", {
    params <- make_mr_sim()$params
    g <- plotSpectra(params)
    expect_s3_class(g, "gg")
})

test_that("plotSpectra.mizerMR respects the resources argument", {
    params <- make_mr_sim()$params
    df <- plotSpectra(params, resources = "res1", return_data = TRUE)
    expect_false("res2" %in% unique(df$Spectra))
    expect_true("res1" %in% unique(df$Spectra))
})

test_that("plotSpectra.mizerMR applies power correctly", {
    params <- make_mr_sim()$params
    df0 <- plotSpectra(params, power = 0, return_data = TRUE)
    df1 <- plotSpectra(params, power = 1, return_data = TRUE)
    # power = 1 values should differ from power = 0 values
    res1_0 <- df0$value[df0$Spectra == "res1"]
    res1_1 <- df1$value[df1$Spectra == "res1"]
    expect_false(isTRUE(all.equal(sort(res1_0), sort(res1_1))))
})

# plotSpectra.mizerMRSim -------------------------------------------------------

test_that("plotSpectra.mizerMRSim return_data has the right column names", {
    sim <- make_mr_sim()$sim
    df <- plotSpectra(sim, return_data = TRUE)
    expect_named(df, c("w", "value", "Spectra", "Legend"))
})

test_that("plotSpectra.mizerMRSim includes both resources", {
    sim <- make_mr_sim()$sim
    df <- plotSpectra(sim, return_data = TRUE)
    expect_true(all(c("res1", "res2") %in% unique(df$Spectra)))
})

test_that("plotSpectra.mizerMRSim returns a ggplot when return_data = FALSE", {
    sim <- make_mr_sim()$sim
    g <- plotSpectra(sim)
    expect_s3_class(g, "gg")
})

test_that("plotSpectra.mizerMRSim respects the resources argument", {
    sim <- make_mr_sim()$sim
    df <- plotSpectra(sim, resources = "res2", return_data = TRUE)
    expect_false("res1" %in% unique(df$Spectra))
    expect_true("res2" %in% unique(df$Spectra))
})
