test_that("Early MR hooks update species interaction ownership", {
    p <- small_mr_model()
    request <- matrix(c(0.7, 0.3), 1,
                      dimnames = list("NewSp", c("large", "small")))
    added <- addSpecies(p, data.frame(species = "NewSp", w_max = 500),
                        resource_interaction = request, steady = FALSE)
    expect_equal(unname(resource_interaction(added)["NewSp", ]), c(0.3, 0.7))
    expect_null(added$other_params$MR$pending_interaction)
    expect_null(p$other_params$MR$pending_interaction)
    expect_equal(resource_interaction(added)[p$species_params$species, ],
                 resource_interaction(p))
    expect_no_error(getRates(added))
    renamed <- mizer::renameSpecies(added, c(NewSp = "Renamed"))
    expect_equal(resource_interaction(renamed)["Renamed", ],
                 resource_interaction(added)["NewSp", ], ignore_attr = TRUE)
    new_order <- rev(renamed$species_params$species)
    reordered <- mizer::reorderSpecies(renamed, new_order)
    expect_identical(rownames(resource_interaction(reordered)),
                     reordered$species_params$species)
    removed <- mizer::removeSpecies(reordered, "Renamed")
    expect_setequal(rownames(resource_interaction(removed)),
                    p$species_params$species)
    expect_true(all(removed$initial_n_pp == 0))
})

test_that("MR hook failures leave the caller and request unchanged", {
    p <- small_mr_model()
    before <- p
    rlang::local_bindings(mr_failing_transform = function(...) {
        stop("deliberate hook failure")
    }, .env = globalenv())
    transforms <- mrTransformations()
    transforms$add_species$transform_fun <- "mr_failing_transform"
    failing <- mizer::setComponent(p, "MR", transformations = transforms)
    unchanged <- failing
    expect_error(
        addSpecies(failing, data.frame(species = "NewSp", w_max = 500),
                   steady = FALSE, resource_interaction = c(0.4, 0.6)),
        "deliberate hook failure"
    )
    expect_identical(failing, unchanged)
    expect_identical(p, before)
    expect_null(failing$other_params$MR$pending_interaction)
})

test_that("MR grid maps preserve frozen arrays and the zero core input", {
    for (scheme in c(FALSE, TRUE)) {
        p <- small_mr_model(scheme)
        old <- p$initial_n_other$MR
        capacity <- p$other_params$MR$capacity
        capacity[1, ] <- capacity[1, ] * 1.1
        p <- setMultipleResources(p, resource_capacity = capacity,
                                  balance = FALSE)
        q <- mizer::adjustSizeGrid(p, new_max_w = max(p$w) * 10)
        overlap <- match(round(log(p$w_full), 10), round(log(q$w_full), 10))
        expect_false(anyNA(overlap))
        expect_equal(q$initial_n_other$MR[, overlap], old,
                     ignore_attr = TRUE)
        expect_equal(q$other_params$MR$capacity[, overlap], capacity,
                     ignore_attr = TRUE)
        expect_identical(comment(q$other_params$MR$capacity), "set manually")
        expect_true(all(q$initial_n_pp == 0))
        expect_no_error(getRates(q))
    }
})
