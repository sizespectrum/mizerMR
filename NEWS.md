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
