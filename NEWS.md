# mizerMR 0.3.0

* Compatible with mizer version 3.0.0
* `setMultipleResources()` now uses mizer's extension-chain methods for
  encounter and resource mortality instead of replacing entries in
  `params@rates_funcs`, allowing composition with other extension packages.
* Accessors and plots that need multiple-resource behaviour are now registered
  as methods for mizer's generics. `plotlySpectra()` has been removed.
* The resource encounter rate is now computed with a single Fourier transform
  for all resources combined instead of one per resource, so the encounter cost
  no longer grows with the number of resources. A per-resource fallback is
  retained for models with a custom (non-Fourier) predation kernel.
* `scaleModel()`, `scaleRates()`, `setResource()` and `summary()` now have
  multiple-resource methods. `scaleModel()` and `scaleRates()` rescale all
  resource capacities, abundances and rates consistently (previously
  `scaleModel()` errored); `setResource()` warns that it only affects the
  silenced built-in resource; and `summary()` reports the combined resource
  size range instead of the empty built-in resource.

# mizerMR 0.0.3

* `plotSpectra()` and `plotlySpectra()` work with multiple resources.
* New plotting functions `plotResourceLevel()` and `plotResourcePred()`.
* `setMultipleResources()` no longer changes the initial resource abundances 
  unless supplied via `initial_resource`.
* Fix bug preventing `resource_params()` from changing the resource arrays.
* `plotDietMR()` renamed to `plotDiet()`.

# mizerMR 0.0.2

* `setMultipleResources()` now adds to the `extensions` field in params metadata.
* `plotDietMR()` is a replacement for `plotDiet()` that works with multiple
  resources.
* `setMultipleResources()`now sets the `inital_n_pp`slot to zero so that there
  are no spurious contributions to `getDiet()` for example.
* The functions extracting information from MizerParams or MizerSim objects now
all fall back onto core mizer functions when called with objects for which no
multiple resources have been set up.
* The resource params are now saved in the `@other_params` slot instead of the
  `resource_params` slot to avoid breaking core mizer code.

# mizerMR 0.0.1

* First functional version of package, ready for testing.
