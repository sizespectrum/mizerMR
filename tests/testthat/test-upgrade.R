make_mr <- function() {
    rp <- as.data.frame(NS_params@resource_params)
    rp <- rbind(rp, rp)
    rp$resource <- c("res1", "res2")
    setMultipleResources(NS_params, resource_params = rp)
}

# Move resource_params back to the old location and clear the mizerMR version
# stamp, mimicking an object saved by a version of mizerMR that stored
# resource_params under other_params$other$MR.
make_old_layout <- function(params) {
    params@other_params$other$MR$resource_params <-
        params@other_params$MR$resource_params
    params@other_params$MR$resource_params <- NULL
    params@extensions <- mizer:::makeExtensions(
        c(mizerMR = "sizespectrum/mizerMR"), character())
    params
}

test_that("setMultipleResources stamps the installed mizerMR version", {
    params <- make_mr()
    expect_equal(mizer:::extensionVersion(params, "mizerMR"),
                 as.character(utils::packageVersion("mizerMR")))
})

test_that("an old-layout object is detected as needing an upgrade", {
    old <- make_old_layout(make_mr())
    expect_true(mizer:::needs_upgrading(old))
    expect_true(is.na(mizer:::extensionVersion(old, "mizerMR")))
    expect_null(old@other_params$MR$resource_params)
})

test_that("validParams upgrades an old-layout object", {
    old <- make_old_layout(make_mr())
    up <- validParams(old)

    # resource_params moved to the new location and recovered by the accessor
    expect_false(is.null(up@other_params$MR$resource_params))
    expect_null(up@other_params$other$MR$resource_params)
    expect_true(is.data.frame(resource_params(up)))
    expect_identical(nrow(resource_params(up)), 2L)

    # version stamp brought up to date and class preserved
    expect_equal(mizer:::extensionVersion(up, "mizerMR"),
                 as.character(utils::packageVersion("mizerMR")))
    expect_s3_class(up, "mizerMR")
})

test_that("upgrade.mizerMR is idempotent and pure", {
    params <- make_mr()
    # Already in the new layout: migration must be a no-op.
    expect_identical(mizerMR:::upgrade.mizerMR(params), params)

    old <- make_old_layout(make_mr())
    once <- mizerMR:::upgrade.mizerMR(old)
    twice <- mizerMR:::upgrade.mizerMR(once)
    expect_identical(once, twice)
    # The method itself does not touch the version stamp (the orchestrator does).
    expect_true(is.na(mizer:::extensionVersion(once, "mizerMR")))
})

test_that("an already-current object is not re-upgraded", {
    params <- make_mr()
    expect_false(mizer:::needs_upgrading(params))
    expect_identical(validParams(params)@extensions, params@extensions)
})


test_that("legacy MR capabilities upgrade through validation and reading", {
    original <- small_mr_model()
    old <- original
    old$other_params$MR$capability_version <- NULL
    old$capabilities$components$MR <- NULL
    old$extensions <- mizer::recordExtension(
        old, "mizerMR", version = "0.3.3.0")$extensions
    original_old <- old
    upgraded <- validParams(old)
    expect_equal(upgraded$initial_n_other$MR, original$initial_n_other$MR)
    expect_equal(upgraded$other_params$MR$rate, original$other_params$MR$rate)
    expect_identical(upgraded$other_params$MR$capability_version, 1L)
    expect_identical(getComponent(upgraded, "MR")$definition$rate_fun,
                     "mizerMR::mrEquation")
    expect_equal(mizer:::extensionVersion(upgraded, "mizerMR"),
                 as.character(utils::packageVersion("mizerMR")))
    expect_identical(old, original_old)
    file <- tempfile(fileext = ".rds")
    on.exit(unlink(file))
    saveRDS(old, file)
    read <- mizer::readParams(file)
    expect_identical(read$other_params$MR$capability_version, 1L)
    expect_equal(read$initial_n_other$MR, original$initial_n_other$MR)
    expect_length(mizer::getSteadyAssessment(read)$missing, 0)
})

test_that("legacy MR upgrades preserve unknown component steps", {
    rlang::local_bindings(review_mr_step = function(n_other, ...) n_other$MR,
                          .env = globalenv())
    old <- small_mr_model()
    old <- setComponent(old, "MR", dynamics_fun = "review_mr_step")
    old$other_params$MR$capability_version <- NULL
    old$capabilities$components$MR <- NULL
    old <- recordExtension(old, "mizerMR", version = "0.3.3.0")
    upgraded <- validParams(old)
    expect_identical(upgraded$other_dynamics$MR, "review_mr_step")
    expect_null(getComponent(upgraded, "MR")$definition$rate_fun)
    sim <- project(upgraded, t_max = 0.1, dt = 0.1, t_save = 0.1,
                   progress_bar = FALSE, info_level = 0)
    expect_equal(finalParams(sim)$initial_n_other$MR, old$initial_n_other$MR)
    expect_error(tuneSteadyState(upgraded, info_level = 0), "support|capabilit")
})


test_that("MR legacy adoption preserves author-owned rate contributions", {
    rlang::local_bindings(review_mr_contribution = function(params, ...) {
        params$initial_n * 0 + 0.1
    }, .env = globalenv())
    for (field in c("encounter_fun", "mort_fun")) {
        p <- small_mr_model()
        args <- list(p, "MR", "review_mr_contribution")
        names(args) <- c("params", "component", field)
        p <- do.call(setComponent, args)
        before <- getRates(p)
        p$other_params$MR$capability_version <- NULL
        p$capabilities$components$MR <- NULL
        old <- recordExtension(p, "mizerMR", version = "0.3.3.0")
        upgraded <- validParams(old)
        component <- getComponent(upgraded, "MR")
        expect_identical(component[[field]], "review_mr_contribution")
        expect_null(component$definition$transformations)
        expect_null(component$definition$tuning)
        expect_equal(getRates(upgraded), before, ignore_attr = TRUE)
        expect_error(mizer::scaleModel(upgraded, 2), "support|transformation")
    }
})


test_that("MR preview upgrades preserve authored definitions and removals", {
    p <- small_mr_model()
    p <- setComponent(p, "MR", convergence_fun = NULL, biomass_fun = NULL,
                      autonomous = FALSE, tuning = NULL, transformations = NULL)
    p <- recordExtension(p, "mizerMR", version = "0.3.3.9001")
    specifications <- p$capabilities
    up <- validParams(p)
    expect_identical(up$capabilities, specifications)
    expect_identical(up$other_params$MR$capability_version, 1L)
    expect_identical(mizer:::extensionVersion(up, "mizerMR"), "0.3.3.9002")
})

# Simulate a preview template whose bindings were adopted around providers that
# the earlier automatic adopter did not inspect. Retaining the stored template
# must not make the official implementation claim support for those providers.
review_bind_mr_template <- function(params) {
    definition <- getComponent(params, "MR")$definition
    params <- setComponent(params, "MR", definition = definition)
    for (owner in c("consumers", "resource", "reproduction")) {
        definition <- getProcess(params, owner)$definition
        params <- setProcess(params, owner, definition = definition)
    }
    recordExtension(params, "mizerMR", version = "0.3.3.9001")
}

test_that("old MR templates cannot certify unknown component contributions", {
    rlang::local_bindings(review_mr_mort = function(params, ...) {
        params$initial_n * 0 + 0.01
    }, .env = globalenv())
    p <- small_mr_model()
    definition <- getComponent(p, "MR")$definition
    definition$mort_fun <- "review_mr_mort"
    p <- setComponent(p, "MR", definition = definition)
    old <- review_bind_mr_template(p)
    up <- validParams(old)
    expect_identical(up$capabilities, old$capabilities)
    expect_identical(up$other_mort$MR, "review_mr_mort")
    expect_equal(getRates(up), getRates(old), ignore_attr = TRUE)
    expect_error(getSteadyAssessment(up), "built-in capability callbacks")
    expect_error(scaleModel(up, 2), "built-in capability callbacks")
    expect_error(balanceResources(up, resource_rate = resource_rate(up)),
                 "built-in capability callbacks")
    expect_identical(up$initial_n_other, old$initial_n_other)
})

test_that("old MR templates reject unknown construction providers", {
    p <- small_mr_model()
    rlang::local_bindings(
        setPredKernel.mizerMR = function(params, ...) NextMethod(),
        .env = globalenv()
    )
    old <- review_bind_mr_template(p)
    up <- validParams(old)
    expect_identical(up$capabilities, old$capabilities)
    expect_error(scaleModel(up, 2), "built-in capability callbacks")
    expect_identical(up$initial_n_other, old$initial_n_other)
})
