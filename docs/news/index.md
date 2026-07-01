# Changelog

## mizerMR (development version)

- mizerMR now respects mizer’s `second_order_w` flag. When second-order
  bin-averaging is switched on, each resource’s carrying capacity and
  replenishment rate are built from the exact bin averages of their
  power laws over the resource’s size range (with the bins straddling
  `w_min`/`w_max` getting the partial average) rather than point-sampled
  at the left bin edge, and the initial resource inherits the
  bin-averaged capacity.
  [`newMRParams()`](https://sizespectrum.org/mizerMR/reference/newMRParams.md)
  gains a `second_order_w` argument that is passed through to mizer’s
  constructor. The default (first-order) behaviour is unchanged and the
  package still works against mizer versions without the
  `second_order_w` slot.

- The resource accessors
  ([`getResourceMort()`](https://sizespectrum.org/mizer/reference/getResourceMort.html),
  [`initialNResource()`](https://sizespectrum.org/mizer/reference/initialNResource-set.html),
  [`finalNResource()`](https://sizespectrum.org/mizer/reference/finalN.html)
  and
  [`NResource()`](https://sizespectrum.org/mizerMR/reference/NResource.md))
  now return classed objects (`MRArrayResourceBySize` and
  `MRArrayTimeByResourceBySize`) that support
  [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  methods, so you can do e.g. `plot(getResourceMort(params))` or
  `plot(NResource(sim))` with one coloured line per resource. This
  mirrors the corresponding classes added for the single resource in
  mizer. Requires mizer (\>= 3.0.0.9002).

## mizerMR 0.3.0

- Compatible with mizer version 3.0.0
- [`setMultipleResources()`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
  now uses mizer’s extension-chain methods for encounter and resource
  mortality instead of replacing entries in `params@rates_funcs`,
  allowing composition with other extension packages.
- Accessors and plots that need multiple-resource behaviour are now
  registered as methods for mizer’s generics.
  [`plotlySpectra()`](https://sizespectrum.org/mizer/reference/plotSpectra.html)
  has been removed.
- The resource encounter rate is now computed with a single Fourier
  transform for all resources combined instead of one per resource, so
  the encounter cost no longer grows with the number of resources. A
  per-resource fallback is retained for models with a custom
  (non-Fourier) predation kernel.
- [`scaleModel()`](https://sizespectrum.org/mizer/reference/scaleModel.html),
  [`scaleRates()`](https://sizespectrum.org/mizer/reference/scaleRates.html),
  [`setResource()`](https://sizespectrum.org/mizer/reference/setResource.html)
  and [`summary()`](https://rdrr.io/r/base/summary.html) now have
  multiple-resource methods.
  [`scaleModel()`](https://sizespectrum.org/mizer/reference/scaleModel.html)
  and
  [`scaleRates()`](https://sizespectrum.org/mizer/reference/scaleRates.html)
  rescale all resource capacities, abundances and rates consistently
  (previously
  [`scaleModel()`](https://sizespectrum.org/mizer/reference/scaleModel.html)
  errored);
  [`setResource()`](https://sizespectrum.org/mizer/reference/setResource.html)
  warns that it only affects the silenced built-in resource; and
  [`summary()`](https://rdrr.io/r/base/summary.html) reports the
  combined resource size range instead of the empty built-in resource.

## mizerMR 0.0.3

- [`plotSpectra()`](https://sizespectrum.org/mizerMR/reference/plotSpectra.md)
  and
  [`plotlySpectra()`](https://sizespectrum.org/mizer/reference/plotSpectra.html)
  work with multiple resources.
- New plotting functions
  [`plotResourceLevel()`](https://sizespectrum.org/mizerMR/reference/plotResourceLevel.md)
  and
  [`plotResourcePred()`](https://sizespectrum.org/mizerMR/reference/plotResourcePred.md).
- [`setMultipleResources()`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
  no longer changes the initial resource abundances unless supplied via
  `initial_resource`.
- Fix bug preventing
  [`resource_params()`](https://sizespectrum.org/mizerMR/reference/resource_params.md)
  from changing the resource arrays.
- [`plotDietMR()`](https://sizespectrum.org/mizerMR/reference/plotDiet.md)
  renamed to
  [`plotDiet()`](https://sizespectrum.org/mizerMR/reference/plotDiet.md).

## mizerMR 0.0.2

- [`setMultipleResources()`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
  now adds to the `extensions` field in params metadata.
- [`plotDietMR()`](https://sizespectrum.org/mizerMR/reference/plotDiet.md)
  is a replacement for
  [`plotDiet()`](https://sizespectrum.org/mizerMR/reference/plotDiet.md)
  that works with multiple resources.
- [`setMultipleResources()`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)now
  sets the `inital_n_pp`slot to zero so that there are no spurious
  contributions to
  [`getDiet()`](https://sizespectrum.org/mizerMR/reference/getDiet.md)
  for example.
- The functions extracting information from MizerParams or MizerSim
  objects now all fall back onto core mizer functions when called with
  objects for which no multiple resources have been set up.
- The resource params are now saved in the `@other_params` slot instead
  of the `resource_params` slot to avoid breaking core mizer code.

## mizerMR 0.0.1

- First functional version of package, ready for testing.
