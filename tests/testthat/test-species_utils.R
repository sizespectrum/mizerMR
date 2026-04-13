## Setup: a two-resource NS model used by all tests in this file
local_params <- function() {
    rp <- data.frame(resource = c("Phyto", "Zoo"))
    setMultipleResources(NS_params, resource_params = rp)
}

# ── addSpecies ────────────────────────────────────────────────────────────────

test_that("addSpecies returns MRMizerParams", {
    params <- local_params()
    new_sp <- data.frame(species = "NewSp", w_max = 100, w_min = 0.001)
    result <- addSpecies(params, new_sp)
    expect_s4_class(result, "MRMizerParams")
})

test_that("addSpecies adds species to species_params", {
    params <- local_params()
    new_sp <- data.frame(species = "NewSp", w_max = 100, w_min = 0.001)
    result <- addSpecies(params, new_sp)
    expect_true("NewSp" %in% result@species_params$species)
    expect_equal(nrow(result@species_params), nrow(params@species_params) + 1)
})

test_that("addSpecies extends resource_interaction with ones by default", {
    params <- local_params()
    no_res <- nrow(resource_params(params))
    new_sp <- data.frame(species = "NewSp", w_max = 100, w_min = 0.001)
    result <- addSpecies(params, new_sp)
    ri <- resource_interaction(result)
    expect_equal(dim(ri), c(nrow(result@species_params), no_res))
    expect_equal(ri["NewSp", ], c(Phyto = 1, Zoo = 1))
})

test_that("addSpecies preserves existing resource interaction values", {
    params <- local_params()
    # Set a non-default interaction for one species
    inter <- resource_interaction(params)
    inter["Sprat", "Phyto"] <- 0.3
    inter["Sprat", "Zoo"] <- 0.7
    resource_interaction(params) <- inter
    new_sp <- data.frame(species = "NewSp", w_max = 100, w_min = 0.001)
    result <- addSpecies(params, new_sp)
    ri <- resource_interaction(result)
    expect_equal(ri["Sprat", "Phyto"], 0.3)
    expect_equal(ri["Sprat", "Zoo"], 0.7)
})

test_that("addSpecies accepts custom resource_interaction for new species", {
    params <- local_params()
    new_sp <- data.frame(species = "NewSp", w_max = 100, w_min = 0.001)
    custom_ri <- matrix(c(0.2, 1.5), nrow = 1,
                        dimnames = list(sp = "NewSp",
                                        resource = c("Phyto", "Zoo")))
    result <- addSpecies(params, new_sp, resource_interaction = custom_ri)
    ri <- resource_interaction(result)
    expect_equal(ri["NewSp", "Phyto"], 0.2)
    expect_equal(ri["NewSp", "Zoo"], 1.5)
})

test_that("addSpecies with a scalar resource_interaction broadcasts to all resources", {
    params <- local_params()
    new_sp <- data.frame(species = "NewSp", w_max = 100, w_min = 0.001)
    result <- addSpecies(params, new_sp, resource_interaction = 0.5)
    ri <- resource_interaction(result)
    expect_equal(unname(ri["NewSp", ]), c(0.5, 0.5))
})

test_that("addSpecies resource_interaction dimension error is informative", {
    params <- local_params()
    new_sp <- data.frame(species = "NewSp", w_max = 100, w_min = 0.001)
    bad_ri <- matrix(0.5, nrow = 2, ncol = 2)  # wrong number of rows
    expect_error(addSpecies(params, new_sp, resource_interaction = bad_ri),
                 "resource_interaction")
})

test_that("addSpecies produces a model that can be projected", {
    params <- local_params()
    new_sp <- data.frame(species = "NewSp", w_max = 100, w_min = 0.001)
    result <- addSpecies(params, new_sp)
    expect_no_error(project(result, t_max = 1, dt = 1))
})

test_that("addSpecies with multiple new species works", {
    params <- local_params()
    new_sp <- data.frame(species = c("Sp1", "Sp2"),
                         w_max  = c(100, 1000),
                         w_min  = c(0.001, 0.001))
    result <- addSpecies(params, new_sp)
    expect_equal(nrow(result@species_params), nrow(params@species_params) + 2)
    ri <- resource_interaction(result)
    expect_equal(unname(ri["Sp1", ]), c(1, 1))
    expect_equal(unname(ri["Sp2", ]), c(1, 1))
})

# ── removeSpecies ─────────────────────────────────────────────────────────────

test_that("removeSpecies returns MRMizerParams", {
    params <- local_params()
    result <- removeSpecies(params, "Sprat")
    expect_s4_class(result, "MRMizerParams")
})

test_that("removeSpecies removes the species from species_params", {
    params <- local_params()
    result <- removeSpecies(params, "Sprat")
    expect_false("Sprat" %in% result@species_params$species)
    expect_equal(nrow(result@species_params), nrow(params@species_params) - 1)
})

test_that("removeSpecies trims resource_interaction rows", {
    params <- local_params()
    result <- removeSpecies(params, "Sprat")
    ri <- resource_interaction(result)
    expect_equal(dim(ri), c(nrow(result@species_params),
                             nrow(resource_params(result))))
    expect_false("Sprat" %in% rownames(ri))
})

test_that("removeSpecies preserves interaction values for remaining species", {
    params <- local_params()
    inter <- resource_interaction(params)
    inter["Cod", "Phyto"] <- 0.4
    inter["Cod", "Zoo"] <- 1.8
    resource_interaction(params) <- inter
    result <- removeSpecies(params, "Sprat")
    ri <- resource_interaction(result)
    expect_equal(ri["Cod", "Phyto"], 0.4)
    expect_equal(ri["Cod", "Zoo"], 1.8)
})

test_that("removeSpecies produces a model that can be projected", {
    params <- local_params()
    result <- removeSpecies(params, "Sprat")
    expect_no_error(project(result, t_max = 1, dt = 1))
})

test_that("removeSpecies works when removing multiple species", {
    params <- local_params()
    result <- removeSpecies(params, c("Sprat", "Sandeel"))
    expect_false("Sprat" %in% result@species_params$species)
    expect_false("Sandeel" %in% result@species_params$species)
    expect_equal(nrow(result@species_params), nrow(params@species_params) - 2)
    expect_equal(nrow(resource_interaction(result)), nrow(result@species_params))
})

# ── renameSpecies ─────────────────────────────────────────────────────────────

test_that("renameSpecies returns MRMizerParams", {
    params <- local_params()
    result <- renameSpecies(params, c(Sprat = "Sprattus sprattus"))
    expect_s4_class(result, "MRMizerParams")
})

test_that("renameSpecies renames the species in species_params", {
    params <- local_params()
    result <- renameSpecies(params, c(Sprat = "Sprattus sprattus"))
    expect_true("Sprattus sprattus" %in% result@species_params$species)
    expect_false("Sprat" %in% result@species_params$species)
})

test_that("renameSpecies updates rownames of resource_interaction", {
    params <- local_params()
    result <- renameSpecies(params, c(Sprat = "Sprattus sprattus"))
    ri <- resource_interaction(result)
    expect_true("Sprattus sprattus" %in% rownames(ri))
    expect_false("Sprat" %in% rownames(ri))
})

test_that("renameSpecies preserves interaction values", {
    params <- local_params()
    inter <- resource_interaction(params)
    inter["Sprat", "Phyto"] <- 0.6
    inter["Sprat", "Zoo"] <- 1.4
    resource_interaction(params) <- inter
    result <- renameSpecies(params, c(Sprat = "Sprattus sprattus"))
    ri <- resource_interaction(result)
    expect_equal(ri["Sprattus sprattus", "Phyto"], 0.6)
    expect_equal(ri["Sprattus sprattus", "Zoo"], 1.4)
})

test_that("renameSpecies works with multiple renames", {
    params <- local_params()
    result <- renameSpecies(params,
                            c(Sprat = "S. sprattus", Cod = "G. morhua"))
    sp <- result@species_params$species
    expect_true("S. sprattus" %in% sp)
    expect_true("G. morhua" %in% sp)
    expect_false("Sprat" %in% sp)
    expect_false("Cod" %in% sp)
    ri_names <- rownames(resource_interaction(result))
    expect_true("S. sprattus" %in% ri_names)
    expect_true("G. morhua" %in% ri_names)
})

test_that("renameSpecies produces a model that can be projected", {
    params <- local_params()
    result <- renameSpecies(params, c(Sprat = "Sprattus sprattus"))
    expect_no_error(project(result, t_max = 1, dt = 1))
})

# ── expandSizeGrid ────────────────────────────────────────────────────────────

test_that("expandSizeGrid on MRMizerParams returns MRMizerParams", {
    params <- local_params()
    result <- expandSizeGrid(params, new_max_w = 1e6)
    expect_s4_class(result, "MRMizerParams")
})

test_that("expandSizeGrid on MizerParams returns MizerParams (not MRMizerParams)", {
    result <- expandSizeGrid(NS_params, new_max_w = 1e6)
    expect_s4_class(result, "MizerParams")
    expect_false(is(result, "MRMizerParams"))
})

test_that("expandSizeGrid expands the species size grid", {
    params <- local_params()
    old_max <- max(w(params))
    result <- expandSizeGrid(params, new_max_w = 1e6)
    expect_gt(max(w(result)), old_max)
})

test_that("expandSizeGrid preserves species count and interaction shape", {
    params <- local_params()
    no_sp  <- nrow(params@species_params)
    no_res <- nrow(resource_params(params))
    result <- expandSizeGrid(params, new_max_w = 1e6)
    expect_equal(nrow(result@species_params), no_sp)
    expect_equal(dim(resource_interaction(result)), c(no_sp, no_res))
})

test_that("expandSizeGrid preserves resource interaction values", {
    params <- local_params()
    inter <- resource_interaction(params)
    inter["Cod", "Phyto"] <- 0.25
    resource_interaction(params) <- inter
    result <- expandSizeGrid(params, new_max_w = 1e6)
    expect_equal(resource_interaction(result)["Cod", "Phyto"], 0.25)
})

test_that("expandSizeGrid updates resource array dimensions", {
    params <- local_params()
    old_no_w_full <- ncol(resource_rate(params))
    no_res <- nrow(resource_params(params))
    result <- expandSizeGrid(params, new_max_w = 1e6)
    new_no_w_full <- ncol(resource_rate(result))
    expect_gt(new_no_w_full, old_no_w_full)
    expect_equal(dim(resource_rate(result)),     c(no_res, new_no_w_full))
    expect_equal(dim(resource_capacity(result)), c(no_res, new_no_w_full))
})

test_that("expandSizeGrid also works when expanding the minimum size", {
    params <- local_params()
    old_min <- min(w(params))
    result  <- expandSizeGrid(params, new_min_w = 1e-5)
    expect_lt(min(w(result)), old_min)
    expect_s4_class(result, "MRMizerParams")
})

test_that("expandSizeGrid produces a model that can be projected", {
    params <- local_params()
    result <- expandSizeGrid(params, new_max_w = 1e6)
    expect_no_error(project(result, t_max = 1, dt = 1))
})
