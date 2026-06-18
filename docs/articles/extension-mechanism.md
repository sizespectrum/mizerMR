# How mizerMR uses mizer's extension mechanism

## Overview

This vignette is aimed at developers who want to understand how mizerMR
is built on top of mizer’s extension machinery. It assumes you have read
`vignette("extensions", package = "mizer")`.

mizerMR uses
[`setComponent()`](https://sizespectrum.org/mizer/reference/setComponent.html)
for the multiple-resource state,
[`other_params()`](https://sizespectrum.org/mizer/reference/setRateFunction.html)
for resource metadata, and mizer’s extension-chain S3 methods for mizer
generics that need to know about the multiple resources. Its methods
call [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) so that
mizerMR can compose with other extension packages, such as therMizer,
instead of replacing entire mizer rate functions or plotting helpers.

## How mizerMR currently works

### The resource state lives in a `setComponent()` component

The multiple resources are stored as a single dynamical component named
`"MR"`. The component state is an array of dimensions
`(resource × size)` holding the number density of each resource at each
size on mizer’s full size grid.
[`setMultipleResources()`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
registers this component:

``` r

params <- setComponent(
    params      = params,
    component   = "MR",
    initial_value = initial_resource,   # array (resource × size)
    dynamics_fun  = "mizerMR_dynamics",
    component_params = list(
        rate        = resource_rate,        # array (resource × size)
        capacity    = resource_capacity,    # array (resource × size)
        interaction = resource_interaction  # matrix (species × resource)
    )
)
```

During a call to
[`project()`](https://sizespectrum.org/mizer/reference/project.html),
mizer automatically calls
[`mizerMR_dynamics()`](https://sizespectrum.org/mizerMR/reference/mizerMR_dynamics.md)
at each time step. That function iterates over resources and applies
semi-chemostat dynamics to each row of the state array, using the
per-resource mortality computed earlier in the rate pipeline.

The component state at every saved time step ends up in
`sim@n_other[, "MR"]` and is exposed to users via `NResource(sim)` and
`finalNResource(sim)`.

### mizerMR registers as a dispatching extension

mizerMR defines the S4 marker classes `mizerMR` and `mizerMRSim`. The
package registers itself with mizer when it is loaded:

``` r

.onLoad <- function(libname, pkgname) {
    mizer::registerExtensions(
        stats::setNames("sizespectrum/mizerMR", pkgname))
}
```

When
[`setMultipleResources()`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
has finished setting up the component state, it records the current
session extension chain in the object and coerces the object to the
appropriate dispatch class:

``` r

params@extensions <- mizer::getRegisteredExtensions()
mizer::coerceToExtensionClass(params)
```

This is the same two-step constructor pattern described in mizer’s
extension package vignette. The stored value for mizerMR is the package
requirement string `"sizespectrum/mizerMR"`, which mizer can use when
loading saved objects.

### Projection methods extend the defaults

The built-in mizer encounter and resource-mortality rate functions work
with a single `n_pp` vector. mizerMR adds methods for the `mizerMR`
extension class so that extension-aware projections understand the
multi-resource array.

#### `projectEncounter.mizerMR`

The method first calls
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) to get the
encounter rate from the rest of the extension chain (the fish community
plus the silenced built-in resource, together with any other extensions’
contributions). It then adds the encounter on the multiple resources.

The encounter convolution is *linear* in the prey spectra, and all
resources share mizer’s full size grid. So the contributions of the
individual resources do not need a separate Fourier transform each: they
can be summed into a single combined prey spectrum for each predator and
transformed once. With the species × resource interaction matrix
$`\Theta`$ and the resource state $`N`$ (an array with one row per
resource),
``` math
\sum_r \mathrm{FFT}\bigl(\Theta_{\cdot r}\,N_r\bigr)
   = \mathrm{FFT}\bigl(\Theta\,N\bigr),
```
so a single transform of `interaction %*% n_other[["MR"]]` gives the
whole resource encounter. This is what
[`mizerMRResourceEncounter()`](https://sizespectrum.org/mizerMR/reference/project_methods.md)
computes, and it makes the encounter cost flat in the number of
resources rather than growing with it. `projectEncounter.mizerMR` uses
this fast path whenever the model uses mizer’s default Fourier predation
kernel:

``` r

projectEncounter.mizerMR <- function(params, n, n_pp, n_other, t = 0, ...) {
    n_pp <- mizerMRValidBaseResource(params, n_pp)
    encounter <- NextMethod(n = n, n_pp = n_pp, n_other = n_other, t = t)
    n_mr <- n_other[["MR"]]
    if (is.null(n_mr)) return(encounter)

    # Fast path: one FFT for all resources combined.
    if (is.null(comment(params@pred_kernel))) {
        return(encounter + mizerMRResourceEncounter(params, n_other))
    }

    # Fallback for a user-supplied (non-Fourier) predation kernel: add each
    # resource's contribution separately by exposing it through the standard
    # `n_pp` and `interaction_resource` arguments and calling `NextMethod()`.
    # ... (per-resource loop, see source) ...
}
```

The per-resource fallback loop is kept only for models with a custom,
non-Fourier predation kernel, which
[`mizerMRResourceEncounter()`](https://sizespectrum.org/mizerMR/reference/project_methods.md)
cannot handle. In that loop each resource is exposed in turn through the
standard `n_pp` and `interaction_resource` arguments and
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) is called again,
so that other extension methods — for example therMizer’s
temperature-scaling of search volume — still affect the
multiple-resource encounter contribution. (The `other_encounter` and
`ext_encounter` contributions are already in `encounter` from the first
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) call, so they
are silenced inside the loop to avoid counting them once per resource.)

The exported
[`mizerMREncounter()`](https://sizespectrum.org/mizerMR/reference/project_methods.md)
function is retained as a compatibility wrapper around
[`mizerMRResourceEncounter()`](https://sizespectrum.org/mizerMR/reference/project_methods.md),
but
[`setMultipleResources()`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
no longer installs it in `params@rates_funcs`.

#### `projectResourceMort.mizerMR`

mizer’s default `ResourceMort` returns a single vector (one value per
size bin) representing mortality on `n_pp`. The mizerMR method returns a
matrix `(resource × size)` by multiplying the predation rate matrix by
the species–resource interaction:

``` r

projectResourceMort.mizerMR <- function(params, n, n_pp, n_other, t,
                                        pred_rate, ...) {
    NextMethod()
    t(params@other_params[["MR"]]$interaction) %*% pred_rate
}
```

The [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) call keeps
the extension chain live. mizerMR does not use the single-resource
mortality vector returned by mizer’s base method; instead it returns a
matrix that is later passed row-by-row to each per-resource dynamics
function inside
[`mizerMR_dynamics()`](https://sizespectrum.org/mizerMR/reference/mizerMR_dynamics.md).

### The built-in single resource is silenced

Because all resource dynamics are now handled by the “MR” component, the
built-in mizer resource must not interfere.
[`setMultipleResources()`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
does two things to achieve this:

``` r

mizer::initialNResource(params) <- 0         # abundance set to zero
resource_dynamics(params) <- "resource_constant"  # kept at zero forever
```

### Resource parameters live in two places

Resource-level parameters (kappa, lambda, r_pp, w_min, w_max) are stored
as a data frame in `other_params(params)[["MR"]]$resource_params`. The
per-size arrays derived from those parameters (carrying capacity and
replenishment rate) live inside the component’s own `component_params`.
This split reflects the two-level structure recommended by mizer:
[`other_params()`](https://sizespectrum.org/mizer/reference/setRateFunction.html)
for model-wide state, `component_params` for data belonging to a single
component.

### Accessors, plots and species utilities are methods

Functions like
[`NResource()`](https://sizespectrum.org/mizerMR/reference/NResource.md),
[`finalNResource()`](https://sizespectrum.org/mizer/reference/finalN.html),
[`plotSpectra()`](https://sizespectrum.org/mizerMR/reference/plotSpectra.md),
[`getDiet()`](https://sizespectrum.org/mizerMR/reference/getDiet.md),
and
[`plotDiet()`](https://sizespectrum.org/mizerMR/reference/plotDiet.md)
need to behave differently depending on whether the model has multiple
resources or not. They are mizer S3 generics, and mizerMR registers
methods for `mizerMR` or `mizerMRSim` objects that use the
multiple-resource component. Those methods still call
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) so that other
extension packages can participate in the same generic.

``` r

NResource.mizerMRSim <- function(sim) {
    NextMethod()
    n_res <- aperm(simplify2array(NOther(sim)[, "MR"]), c(3, 1, 2))
    dimnames(n_res)[[1]] <- dimnames(NOther(sim))[[1]]
    names(dimnames(n_res))[[1]] <- names(dimnames(NOther(sim)))[[1]]
    n_res
}
```

When a user calls `NResource(sim)` and `sim` is a `mizerMRSim`, R
dispatches to `NResource.mizerMRSim`. When `sim` is a plain `MizerSim`
(no multiple resources), R dispatches to mizer’s own
`NResource.MizerSim`.

The same pattern applies to `finalNResource`, `plotSpectra`, `getDiet`,
`plotDiet`, and `animateSpectra`.

Species-manipulation helpers follow the same rule. Methods such as
`addSpecies.mizerMR`, `removeSpecies.mizerMR`, `renameSpecies.mizerMR`
and `expandSizeGrid.mizerMR` strip the MR component temporarily when
mizer’s base operation needs to run on an ordinary single-resource
object, call [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html),
and then rebuild the MR component on the returned object.

### Model rescaling and reporting are methods

A few mizer functions that transform or report on a whole model also
need to be aware of the resources, because those functions only know
about the single built-in resource that mizerMR has silenced. mizerMR
therefore provides methods for them too:

- `scaleModel.mizerMR` and `scaleRates.mizerMR` rescale the resource
  capacities, abundances and rates stored in the MR component, which the
  base methods do not see (the base
  [`scaleModel()`](https://sizespectrum.org/mizer/reference/scaleModel.html)
  would even error on a `mizerMR` object). They delegate the consumer
  scaling to the base method.
- `setResource.mizerMR` warns that it only affects the silenced built-in
  resource and points the user to
  [`setMultipleResources()`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
  and the `resource_rate<-` / `resource_capacity<-` setters.
- `summary.mizerMR` reports the combined size range of all resources
  instead of the (empty) built-in resource.

`scaleModel.mizerMR` and `setResource.mizerMR` reach the base method by
coercing to the plain class with the extension chain temporarily
cleared, so that the base method’s internal
[`validParams()`](https://sizespectrum.org/mizer/reference/validParams.html)
does not re-promote the object to the `mizerMR` class and dispatch back
into the MR-aware setters.

[`project()`](https://sizespectrum.org/mizer/reference/project.html)
returns a `mizerMRSim` object automatically because the params object
inside the simulation carries the `mizerMR` extension chain, so the
correct methods fire without the user having to do anything special.

### Summary of benefits

| Aspect | Method approach |
|----|----|
| Dispatch logic | R method dispatch, with no repeated component guards |
| Fallback to mizer | Methods call [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html) to keep the chain live |
| Extensibility | Rate hooks, accessors, plots and species utilities follow standard R generic behaviour |
| Correctness risk | MR-specific code is tied to the `mizerMR` class membership |

The extension mechanism in mizer is designed with this kind of layering
in mind. Using methods for projection hooks, accessors and plots keeps
mizerMR composable with other extension packages and makes the codebase
easier to maintain.
