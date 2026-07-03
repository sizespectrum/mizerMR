# Tests for addSpecies, removeSpecies, renameSpecies and expandSizeGrid methods

make_mr_params <- function() {
    rp <- data.frame(
        resource = c("res1", "res2"),
        kappa = c(0.1, 0.05),
        lambda = c(2.05, 2.1),
        r_pp = c(4, 2),
        stringsAsFactors = FALSE
    )
    setMultipleResources(NS_params, resource_params = rp)
}

# addSpecies -------------------------------------------------------------------

test_that("addSpecies adds species with default resource_interaction of 1", {
    params <- make_mr_params()
    sp <- data.frame(species = "NewSp", w_max = 500, stringsAsFactors = FALSE)
    p2 <- suppressMessages(suppressWarnings(addSpecies(params, sp)))

    expect_s4_class(p2, "mizerMR")
    expect_true("NewSp" %in% p2@species_params$species)
    inter <- resource_interaction(p2)
    expect_equal(dim(inter), c(13L, 2L))
    # New species gets 1 for all resources by default
    expect_equal(unname(inter["NewSp", ]), c(1, 1))
    # Existing species interactions are preserved
    expect_equal(inter[rownames(resource_interaction(params)), ],
                 resource_interaction(params), ignore_attr = TRUE)
})

test_that("addSpecies respects a user-supplied resource_interaction vector", {
    params <- make_mr_params()
    sp <- data.frame(species = "NewSp", w_max = 500, stringsAsFactors = FALSE)
    p2 <- suppressMessages(suppressWarnings(
        addSpecies(params, sp, resource_interaction = c(0.3, 0.7))
    ))
    expect_equal(unname(resource_interaction(p2)["NewSp", ]), c(0.3, 0.7))
})

test_that("addSpecies errors when resource_interaction has wrong dimensions", {
    params <- make_mr_params()
    sp <- data.frame(species = "NewSp", w_max = 500, stringsAsFactors = FALSE)
    bad_inter <- matrix(c(0.5, 0.5, 0.5), nrow = 1, ncol = 3)
    expect_error(
        suppressMessages(suppressWarnings(
            addSpecies(params, sp, resource_interaction = bad_inter)
        )),
        "1 x 2"
    )
})

test_that("addSpecies preserves resource capacity, rate and initial abundance", {
    params <- make_mr_params()
    sp <- data.frame(species = "NewSp", w_max = 500, stringsAsFactors = FALSE)
    p2 <- suppressMessages(suppressWarnings(addSpecies(params, sp)))

    expect_equal(resource_capacity(p2), resource_capacity(params),
                 ignore_attr = TRUE)
    expect_equal(resource_rate(p2), resource_rate(params),
                 ignore_attr = TRUE)
    expect_equal(unclass(initialNResource(p2)),
                 unclass(initialNResource(params)), ignore_attr = TRUE)
})

# removeSpecies ----------------------------------------------------------------

test_that("removeSpecies trims the resource interaction matrix", {
    params <- make_mr_params()
    p2 <- suppressMessages(suppressWarnings(removeSpecies(params, "Cod")))

    expect_s4_class(p2, "mizerMR")
    expect_false("Cod" %in% p2@species_params$species)
    inter <- resource_interaction(p2)
    expect_equal(nrow(inter), nrow(params@species_params) - 1L)
    expect_false("Cod" %in% rownames(inter))
    # Remaining species interactions are preserved
    remaining <- setdiff(params@species_params$species, "Cod")
    expect_equal(inter[remaining, ], resource_interaction(params)[remaining, ],
                 ignore_attr = TRUE)
})

test_that("removeSpecies preserves resource capacity, rate and params", {
    params <- make_mr_params()
    p2 <- suppressMessages(suppressWarnings(removeSpecies(params, "Cod")))

    expect_equal(resource_capacity(p2), resource_capacity(params),
                 ignore_attr = TRUE)
    expect_equal(resource_rate(p2), resource_rate(params),
                 ignore_attr = TRUE)
    expect_equal(resource_params(p2), resource_params(params),
                 ignore_attr = TRUE)
})

# renameSpecies ----------------------------------------------------------------

test_that("renameSpecies updates interaction matrix row names", {
    params <- make_mr_params()
    p2 <- suppressMessages(renameSpecies(params, c(Cod = "Bigfish")))

    expect_s4_class(p2, "mizerMR")
    expect_true("Bigfish" %in% p2@species_params$species)
    expect_false("Cod" %in% p2@species_params$species)
    inter <- resource_interaction(p2)
    expect_true("Bigfish" %in% rownames(inter))
    expect_false("Cod" %in% rownames(inter))
    # Interaction values are unchanged
    expect_equal(unname(inter["Bigfish", ]),
                 unname(resource_interaction(params)["Cod", ]))
})

test_that("renameSpecies preserves resource capacity and rate", {
    params <- make_mr_params()
    p2 <- suppressMessages(renameSpecies(params, c(Cod = "Bigfish")))

    expect_equal(resource_capacity(p2), resource_capacity(params),
                 ignore_attr = TRUE)
    expect_equal(resource_rate(p2), resource_rate(params),
                 ignore_attr = TRUE)
})

# expandSizeGrid ---------------------------------------------------------------

test_that("expandSizeGrid returns a mizerMR with recalculated arrays", {
    params <- make_mr_params()
    p2 <- suppressWarnings(
        expandSizeGrid(params, new_max_w = max(params@w) * 2)
    )

    expect_s4_class(p2, "mizerMR")
    # Expanded grid must be larger
    expect_gt(length(w_full(p2)), length(w_full(params)))
    # Resource names are preserved
    expect_equal(rownames(resource_capacity(p2)),
                 rownames(resource_capacity(params)))
    expect_equal(rownames(resource_rate(p2)),
                 rownames(resource_rate(params)))
    # Arrays are sized to the new grid
    expect_equal(ncol(resource_capacity(p2)), length(w_full(p2)))
    expect_equal(ncol(resource_rate(p2)), length(w_full(p2)))
})

test_that("expandSizeGrid preserves resource_interaction values", {
    params <- make_mr_params()
    inter_before <- resource_interaction(params)
    p2 <- suppressWarnings(
        expandSizeGrid(params, new_max_w = max(params@w) * 2)
    )
    expect_equal(resource_interaction(p2), inter_before, ignore_attr = TRUE)
})
