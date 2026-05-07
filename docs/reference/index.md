# Package index

## Setting multiple resources

- [`resource_params()`](https://sizespectrum.org/mizerMR/reference/resource_params.md)
  [`` `resource_params<-`() ``](https://sizespectrum.org/mizerMR/reference/resource_params.md)
  : Resource parameters
- [`setMultipleResources()`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
  [`resource_capacity()`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
  [`` `resource_capacity<-`() ``](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
  [`resource_rate()`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
  [`` `resource_rate<-`() ``](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
  [`resource_interaction()`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
  [`` `resource_interaction<-`() ``](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
  [`initialNResource(`*`<mizerMR>`*`)`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
  [`` `initialNResource<-`( ``*`<mizerMR>`*`)`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
  : Set up multiple resources
- [`newMRParams()`](https://sizespectrum.org/mizerMR/reference/newMRParams.md)
  : Create a new multi-resource, multi-species model

## Accessing results of simulations

- [`NResource(`*`<mizerMRSim>`*`)`](https://sizespectrum.org/mizerMR/reference/NResource.md)
  [`finalNResource(`*`<mizerMRSim>`*`)`](https://sizespectrum.org/mizerMR/reference/NResource.md)
  : Access resource abundances in simulation result
- [`plotSpectra(`*`<mizerMRSim>`*`)`](https://sizespectrum.org/mizerMR/reference/plotSpectra.md)
  [`plotSpectra(`*`<mizerMR>`*`)`](https://sizespectrum.org/mizerMR/reference/plotSpectra.md)
  : Plot the abundance spectra
- [`getDiet(`*`<mizerMR>`*`)`](https://sizespectrum.org/mizerMR/reference/getDiet.md)
  : Get diet of predator at size, resolved by prey species
- [`plotDiet(`*`<mizerMR>`*`)`](https://sizespectrum.org/mizerMR/reference/plotDiet.md)
  [`plotDiet(`*`<mizerMRSim>`*`)`](https://sizespectrum.org/mizerMR/reference/plotDiet.md)
  [`plotDietMR()`](https://sizespectrum.org/mizerMR/reference/plotDiet.md)
  [`plotlyDiet()`](https://sizespectrum.org/mizerMR/reference/plotDiet.md)
  **\[experimental\]** : Plot diet, resolved by prey species, as
  function of predator at size.
- [`plotResourceLevel()`](https://sizespectrum.org/mizerMR/reference/plotResourceLevel.md)
  : Plot the proportion of the resource spectrum(s) compared to their
  carrying capacity
- [`plotResourcePred()`](https://sizespectrum.org/mizerMR/reference/plotResourcePred.md)
  [`plotlyResourcePred()`](https://sizespectrum.org/mizerMR/reference/plotResourcePred.md)
  : Plot functions regarding the resources. Compatible with non mizerMR
  objects Plot the mortality applied on the resource spectrum(s)
- [`animateSpectra(`*`<mizerMRSim>`*`)`](https://sizespectrum.org/mizerMR/reference/animateSpectra.md)
  **\[experimental\]** : Animation of the abundance spectra

## Species utilities

- [`addSpecies(`*`<mizerMR>`*`)`](https://sizespectrum.org/mizerMR/reference/addSpecies.md)
  : Add species to a mizerMR model
- [`removeSpecies(`*`<mizerMR>`*`)`](https://sizespectrum.org/mizerMR/reference/removeSpecies.md)
  : Remove species from a mizerMR model
- [`renameSpecies(`*`<mizerMR>`*`)`](https://sizespectrum.org/mizerMR/reference/renameSpecies.md)
  : Rename species in a mizerMR model
- [`expandSizeGrid(`*`<mizerMR>`*`)`](https://sizespectrum.org/mizerMR/reference/expandSizeGrid.md)
  : Expand the size grid of a mizerMR model

## Internal helper functions

- [`validResourceParams()`](https://sizespectrum.org/mizerMR/reference/validResourceParams.md)
  : Validate resource parameter data frame
- [`valid_resources_arg()`](https://sizespectrum.org/mizerMR/reference/valid_resources_arg.md)
  : Helper function to assure validity of resources argument
- [`set_resource_param_default()`](https://sizespectrum.org/mizerMR/reference/set_resource_param_default.md)
  : Set a resource parameter to a default value
- [`mizerMR-class`](https://sizespectrum.org/mizerMR/reference/mizerMR-class.md)
  [`mizerMRSim-class`](https://sizespectrum.org/mizerMR/reference/mizerMR-class.md)
  : mizerMR marker classes
- [`mizerMRBaseResource()`](https://sizespectrum.org/mizerMR/reference/project_methods.md)
  [`mizerMRValidBaseResource()`](https://sizespectrum.org/mizerMR/reference/project_methods.md)
  [`getEncounter(`*`<mizerMR>`*`)`](https://sizespectrum.org/mizerMR/reference/project_methods.md)
  [`getPredRate(`*`<mizerMR>`*`)`](https://sizespectrum.org/mizerMR/reference/project_methods.md)
  [`getResourceMort(`*`<mizerMR>`*`)`](https://sizespectrum.org/mizerMR/reference/project_methods.md)
  [`getRates(`*`<mizerMR>`*`)`](https://sizespectrum.org/mizerMR/reference/project_methods.md)
  [`projectEncounter(`*`<mizerMR>`*`)`](https://sizespectrum.org/mizerMR/reference/project_methods.md)
  [`mizerMREncounter()`](https://sizespectrum.org/mizerMR/reference/project_methods.md)
  [`mizerMRResourceEncounter()`](https://sizespectrum.org/mizerMR/reference/project_methods.md)
  [`projectResourceMort(`*`<mizerMR>`*`)`](https://sizespectrum.org/mizerMR/reference/project_methods.md)
  : Multiple-resource project methods
- [`mizerMR_dynamics()`](https://sizespectrum.org/mizerMR/reference/mizerMR_dynamics.md)
  : Multiple-resource dynamics
- [`plotDietData()`](https://sizespectrum.org/mizerMR/reference/plotDietData.md)
  : Format and plot MR diet data
- [`expect_unchanged()`](https://sizespectrum.org/mizerMR/reference/expect_unchanged.md)
  : Expect a mizer object to be unchanged
- [`.onLoad()`](https://sizespectrum.org/mizerMR/reference/dot-onLoad.md)
  : Register mizerMR with mizer
- [`.strip_mr()`](https://sizespectrum.org/mizerMR/reference/dot-strip_mr.md)
  : Strip the MR extension from params
