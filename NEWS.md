# mizerMR (development version)

* mizerMR now requires mizer (>= 3.3.0) and follows the plotting interface that
  mizer introduced in that release. `plotSpectra()` and `animate()` take the
  `biomass` and `per_log_size` flags, whose sum is the power of the weight
  multiplying the number density; `power` is still accepted and is the only way
  to ask for a power that is not such a sum, but a `power` that contradicts the
  flags is now an error rather than being silently resolved.

* `plotSpectra()`, `animate()` and the `plot()` methods of the resource array
  classes now honour `size_axis = "l"`, `llim`, `log_x`, `log_y` and `log`,
  which used to be ignored. On a length axis the resources are converted with
  the weight-length relationship mizer uses for its own resource (`a` and `b`
  from `resource_params()`, or mizer's defaults), which is the same one with
  which the combined resource enters the total, and a density picks up the
  `dw/dl` Jacobian. The total is summed on the axis it is plotted against.
  `plotSpectra()` also honours `resource = FALSE` and now labels its axes and
  places its legend exactly as mizer does.

* The resource arrays returned by `initialNResource()`, `NResource()`,
  `finalNResource()` and `getResourceMort()` carry mizer's new `type` attribute
  saying what kind of quantity they hold, and their `plot()` method uses it: a
  density can be converted to a length axis or shown per logarithmic size, a
  rate cannot, and a proportion would be drawn on a linear axis from 0 to 1.
  `MRArrayResourceBySize()` and `MRArrayTimeByResourceBySize()` gained a `type`
  argument.

* `plotDiet()` honours `size_axis`, `wlim`, `llim`, `log_x` and `log_y`, which
  used to be ignored. Its `wlim` default is now `c(NA, NA)` as in mizer, rather
  than the `c(1, NA)` that never took effect.

* `plotResourceLevel()` shows the whole of the interval from 0 to 1 on its y
  axis, as mizer now does for a proportion.

* `newMRParams()` and `setMultipleResources()` gained an `info_level` argument
  and report through mizer's reporting mechanism, so that everything mizerMR
  says while building or changing a model obeys `info_level` and arrives
  together with mizer's own reports at the end of the call.

* mizerMR now respects mizer's `second_order_w` flag. When second-order
  bin-averaging is switched on, each resource's carrying capacity and
  replenishment rate are built from the exact bin averages of their power laws
  over the resource's size range (with the bins straddling `w_min`/`w_max`
  getting the partial average) rather than point-sampled at the left bin edge,
  and the initial resource inherits the bin-averaged capacity. `newMRParams()`
  gains a `second_order_w` argument that is passed through to mizer's
  constructor. The default (first-order) behaviour is unchanged and the package
  still works against mizer versions without the `second_order_w` slot.

* The resource accessors (`getResourceMort()`, `initialNResource()`,
  `finalNResource()` and `NResource()`) now return classed objects
  (`MRArrayResourceBySize` and `MRArrayTimeByResourceBySize`) that support
  `print()`, `summary()`, `plot()` and `as.data.frame()` methods, so you can
  do e.g. `plot(getResourceMort(params))` or `plot(NResource(sim))` with one
  coloured line per resource. This mirrors the corresponding classes added for
  the single resource in mizer. Requires mizer (>= 3.0.0.9002).

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
