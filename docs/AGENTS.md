# NA

## What This Package Does

mizerMR is an R extension package for
[mizer](https://sizespectrum.org/mizer/) that adds support for multiple
size-structured background resource spectra. It enables species to feed
selectively across distinct resource types (e.g., plankton vs. benthic
invertebrates), supporting ontogenetic dietary shifts.

## Common Commands

These are run from within an R session or via `Rscript`:

``` r
# Load package for interactive development
devtools::load_all()

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-newMRParams.R")

# Rebuild documentation (after editing roxygen comments)
devtools::document()

# Full package check
devtools::check()

# Install dependencies
devtools::install_deps()
```

Visual regression tests use [vdiffr](https://vdiffr.r-lib.org/). To
update snapshots after intentional plot changes:

``` r
vdiffr::manage_cases()
```

## Architecture

### State: Where Data Lives

All MR-specific state is stored in `params@other_params[["MR"]]` — a
list attached to mizer’s `MizerParams` object. Key sub-slots:

- `$resource_params`: data frame with one row per resource (columns:
  resource, kappa, lambda, r_pp, w_min, w_max)
- `$resource_capacity`: array `[resource × size]` — carrying capacity
- `$resource_rate`: array `[resource × size]` — growth/renewal rate
- `$resource_interaction`: matrix `[species × resource]` — feeding
  preferences
- `$n_pp`: initial resource abundances

Simulation output is stored in `sim@other[["MR"]]` — an array
`[time × resource × size]`, accessed via `NResource(sim)`.

### Core Flow

1.  **Setup** (`setMultipleResources.R`, `newMRParams.R`): User calls
    `setMultipleResources(params, resource_params)` or
    `newMRParams(...)`. These functions validate inputs, compute size
    grids per resource, populate `other_params[["MR"]]`, and register
    custom dynamics/encounter functions with mizer’s hook system.

2.  **Projection** (`resource_dynamics.R`, `project_methods.R`): During
    `project()`, mizer calls
    [`mizerMR_dynamics()`](https://sizespectrum.org/mizerMR/reference/mizerMR_dynamics.md)
    (registered as `other_dynamics[["MR"]]`), which updates each
    resource using semi-chemostat dynamics. Encounter rates are computed
    by
    [`mizerMREncounter()`](https://sizespectrum.org/mizerMR/reference/project_methods.md),
    which replaces mizer’s default encounter function.

3.  **Results access** (`NResource.R`, `plotResource.R`,
    `plotSpectra.R`, `diet.R`): Users retrieve results via
    `NResource(sim)` and visualise with
    [`plotSpectra()`](https://sizespectrum.org/mizerMR/reference/plotSpectra.md),
    [`animateSpectra()`](https://sizespectrum.org/mizerMR/reference/animateSpectra.md),
    [`plotResourceLevel()`](https://sizespectrum.org/mizerMR/reference/plotResourceLevel.md),
    [`plotDiet()`](https://sizespectrum.org/mizerMR/reference/plotDiet.md).

### Key Design Constraints

- Requires mizer ≥ 2.4.0 and mizerExperimental ≥ 2.4.0 as dependencies.
- The package replaces mizer’s single background resource — once
  [`setMultipleResources()`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
  is called, the original `n_pp` slot is no longer used for dynamics.
- Resource size grids are independent per resource (each has its own
  `w_min`/`w_max`), unlike mizer’s single shared grid.
- Parameter validation is handled by
  [`validResourceParams()`](https://sizespectrum.org/mizerMR/reference/validResourceParams.md)
  and
  [`valid_resources_arg()`](https://sizespectrum.org/mizerMR/reference/valid_resources_arg.md)
  in `resource_params.R`.

### File Map

| File                     | Role                                                                                                                                                             |
|--------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `setMultipleResources.R` | Main setup; registers hooks, builds arrays                                                                                                                       |
| `newMRParams.R`          | Convenience wrapper around `setMultipleResources`                                                                                                                |
| `resource_params.R`      | Getters/setters for all MR parameters                                                                                                                            |
| `resource_dynamics.R`    | [`mizerMR_dynamics()`](https://sizespectrum.org/mizerMR/reference/mizerMR_dynamics.md) and mortality/encounter internals                                         |
| `project_methods.R`      | Hooks into mizer’s projection output recording                                                                                                                   |
| `NResource.R`            | Accessor for simulation output array                                                                                                                             |
| `plotSpectra.R`          | Multi-resource spectral plots (replaces mizer’s version)                                                                                                         |
| `plotResource.R`         | Resource-specific plots (level, predation)                                                                                                                       |
| `animateSpectra.R`       | Animated spectral plots via plotly                                                                                                                               |
| `diet.R`                 | [`getDiet()`](https://sizespectrum.org/mizerMR/reference/getDiet.md) and [`plotDiet()`](https://sizespectrum.org/mizerMR/reference/plotDiet.md) / `plotlyDiet()` |
