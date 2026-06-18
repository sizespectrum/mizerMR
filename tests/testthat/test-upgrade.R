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
    expect_s4_class(up, "mizerMR")
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
