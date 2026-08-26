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

# mizer 3.3 plotting interface -------------------------------------------------

test_that("biomass and per_log_size choose the plotted quantity", {
    params <- make_mr_sim()$params
    number <- plotSpectra(params, biomass = FALSE, return_data = TRUE)
    biomass <- plotSpectra(params, return_data = TRUE)
    per_log <- plotSpectra(params, per_log_size = TRUE, return_data = TRUE)
    res <- function(df) df[df$Spectra == "res1", ]
    # The default is the biomass density, i.e. the number density times w.
    expect_equal(res(biomass)$value, res(number)$value * res(number)$w)
    # `per_log_size` multiplies by one more power of w.
    expect_equal(res(per_log)$value, res(biomass)$value * res(biomass)$w)
})

test_that("a power contradicting the flags is an error", {
    params <- make_mr_sim()$params
    expect_error(plotSpectra(params, power = 1, biomass = FALSE,
                             per_log_size = FALSE),
                 "not contradictory values of both")
    # Agreeing values are honoured.
    expect_no_error(plotSpectra(params, power = 2, biomass = TRUE,
                                per_log_size = TRUE))
})

test_that("plotSpectra can be drawn against a length axis", {
    params <- make_mr_sim()$params
    df_w <- plotSpectra(params, return_data = TRUE)
    df_l <- plotSpectra(params, size_axis = "l", return_data = TRUE)
    expect_named(df_l, c("l", "value", "Spectra", "Legend"))
    expect_true(all(c("res1", "res2") %in% df_l$Spectra))
    # The resources are converted with mizer's resource weight-length
    # relationship and pick up the density Jacobian dw/dl = b w / l.
    ab <- get("resource_length_params", envir = asNamespace("mizer"))(params)
    res_w <- df_w[df_w$Spectra == "res1", ]
    res_l <- df_l[df_l$Spectra == "res1", ]
    expect_equal(as.numeric(res_l$l), (as.numeric(res_w$w) / ab$a)^(1 / ab$b))
    expect_equal(as.numeric(res_l$value),
                 as.numeric(res_w$value) * ab$b * as.numeric(res_w$w) /
                     as.numeric(res_l$l))
    expect_s3_class(plotSpectra(params, size_axis = "l"), "gg")
})

test_that("llim selects the length range", {
    params <- make_mr_sim()$params
    df <- plotSpectra(params, size_axis = "l", llim = c(1, 10),
                      return_data = TRUE)
    expect_true(all(df$l >= 1 & df$l <= 10))
})

test_that("resource = FALSE drops the resources", {
    params <- make_mr_sim()$params
    df <- plotSpectra(params, resource = FALSE, return_data = TRUE)
    expect_false(any(c("res1", "res2") %in% df$Spectra))
    expect_true(any(df$Spectra %in% params@species_params$species))
})

test_that("the total includes all resources", {
    params <- make_mr_sim()$params
    df <- plotSpectra(params, total = TRUE, return_data = TRUE)
    expect_true("Total" %in% df$Legend)
    total <- df[df$Legend == "Total", ]
    # At the smallest sizes only the resources contribute, so the total there
    # is the sum over all resources.
    w <- params@w_full
    combined <- colSums(unclass(initialNResource(params))) * w
    smallest <- which.min(total$w)
    expect_equal(total$value[smallest],
                 unname(combined[which.min(abs(w - total$w[smallest]))]))
})

test_that("plotSpectra.mizerMRSim follows the same interface", {
    sim <- make_mr_sim()$sim
    df_l <- plotSpectra(sim, size_axis = "l", return_data = TRUE)
    expect_named(df_l, c("l", "value", "Spectra", "Legend"))
    expect_true(all(c("res1", "res2") %in% df_l$Spectra))
    expect_error(plotSpectra(sim, power = 0, biomass = TRUE),
                 "not contradictory values of both")
    df <- plotSpectra(sim, resource = FALSE, return_data = TRUE)
    expect_false(any(c("res1", "res2") %in% df$Spectra))
})

test_that("linear axes are respected", {
    params <- make_mr_sim()$params
    g <- plotSpectra(params, log_x = FALSE, log_y = FALSE)
    expect_equal(g$scales$scales[[1]]$trans$name, "identity")
    expect_equal(g$scales$scales[[2]]$trans$name, "identity")
})
