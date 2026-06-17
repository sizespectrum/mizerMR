# Tests for the single-FFT resource encounter in projectEncounter.mizerMR

# Build a model with three genuinely different resources (distinct size ranges,
# slopes and per-species interactions) on the shared w_full grid.
make_three_resource_params <- function() {
    rp <- data.frame(
        resource = c("small", "mid", "large"),
        kappa = c(1e11, 5e10, 2e11),
        lambda = c(2.13, 2.05, 1.95),
        r_pp = c(4, 10, 6),
        w_min = c(NA, 1e-4, 1e-3),
        w_max = c(1e-2, 1, 10),
        stringsAsFactors = FALSE
    )
    no_sp <- nrow(NS_params@species_params)
    # Distinct, non-trivial species x resource interactions
    interaction <- matrix(
        seq(0.2, 1.5, length.out = no_sp * 3),
        nrow = no_sp, ncol = 3,
        dimnames = list(sp = dimnames(NS_params@initial_n)[[1]],
                        resource = rp$resource)
    )
    setMultipleResources(NS_params, resource_params = rp,
                         resource_interaction = interaction)
}

# Independent oracle for the encounter rate of an MR model, computed purely with
# base mizer's mizerEncounter() applied to each resource in turn. This does not
# use mizerMRResourceEncounter(), so it genuinely checks the fast path.
reference_encounter <- function(params, n = initialN(params),
                                n_other = initialNOther(params)) {
    pbase <- methods::as(params, "MizerParams")
    n_mr <- n_other[["MR"]]
    interaction <- params@other_params[["MR"]]$interaction
    zero_n <- n
    zero_n[] <- 0
    zero_pp <- mizerMRBaseResource(params)
    zero_pp[] <- 0

    # Baseline contribution (ext_encounter etc.) with everything else silenced.
    baseline <- mizerEncounter(pbase, n = zero_n, n_pp = zero_pp,
                               n_other = list())
    # Pure fish contribution.
    fish <- mizerEncounter(pbase, n = n, n_pp = zero_pp,
                           n_other = list()) - baseline

    encounter <- baseline + fish
    for (r in seq_len(nrow(n_mr))) {
        pbase@species_params$interaction_resource <- interaction[, r]
        enc_r <- mizerEncounter(pbase, n = zero_n, n_pp = n_mr[r, ],
                                n_other = list()) - baseline
        encounter <- encounter + enc_r
    }
    encounter
}

test_that("Single-FFT encounter matches per-resource reference", {
    params <- make_three_resource_params()
    expect_equal(getEncounter(params), reference_encounter(params),
                 ignore_attr = TRUE)
})

test_that("Single-FFT encounter is correct away from the initial state", {
    params <- make_three_resource_params()
    n <- initialN(params) * 1.3
    n_other <- initialNOther(params)
    n_other[["MR"]] <- n_other[["MR"]] * c(0.5, 2, 1.5)  # scale each resource
    enc <- getEncounter(params, n = n, n_other = n_other)
    ref <- reference_encounter(params, n = n, n_other = n_other)
    expect_equal(enc, ref, ignore_attr = TRUE)
})

test_that("Fallback loop (custom pred_kernel) agrees with the fast path", {
    params <- make_three_resource_params()
    fast <- getEncounter(params)
    # Force the non-Fourier fallback by giving pred_kernel a comment.
    params_pk <- params
    pk <- getPredKernel(params_pk)
    comment(pk) <- "custom"
    params_pk@pred_kernel <- pk
    slow <- getEncounter(params_pk)
    # The fallback uses the explicit (direct-sum) predation kernel while the
    # fast path uses the Fourier kernel, so allow a looser tolerance.
    expect_equal(fast, slow, ignore_attr = TRUE, tolerance = 1e-4)
})

test_that("Encounter cost is roughly flat in the number of resources", {
    skip_on_cran()
    skip_on_ci()

    one <- make_three_resource_params()  # reuse helper but with one resource
    rp1 <- data.frame(resource = "only", kappa = 1e11, lambda = 2.05,
                      r_pp = 4, stringsAsFactors = FALSE)
    one <- setMultipleResources(NS_params, resource_params = rp1)

    rp_many <- data.frame(
        resource = paste0("r", 1:12),
        kappa = 1e11, lambda = 2.05, r_pp = 4,
        stringsAsFactors = FALSE
    )
    many <- setMultipleResources(NS_params, resource_params = rp_many)

    reps <- 100
    t_one <- system.time(for (i in seq_len(reps)) getEncounter(one))[["elapsed"]]
    t_many <- system.time(for (i in seq_len(reps)) getEncounter(many))[["elapsed"]]

    # With the old per-resource loop this ratio would be ~12. The single-FFT
    # path keeps it close to 1; allow generous slack for timing noise.
    expect_lt(t_many / max(t_one, 1e-6), 4)
})
