# Plot the abundance spectra

Plots the number density multiplied by a power of the weight, with the
power specified by the `power` argument.

## Usage

``` r
# S3 method for class 'mizerMRSim'
plotSpectra(
  object,
  species = NULL,
  resources = NULL,
  time_range,
  geometric_mean = FALSE,
  wlim = c(NA, NA),
  ylim = c(NA, NA),
  power = 1,
  biomass = TRUE,
  total = FALSE,
  background = TRUE,
  highlight = NULL,
  return_data = FALSE,
  ...
)

# S3 method for class 'mizerMR'
plotSpectra(
  object,
  species = NULL,
  resources = NULL,
  wlim = c(NA, NA),
  ylim = c(NA, NA),
  power = 1,
  total = FALSE,
  background = TRUE,
  highlight = NULL,
  return_data = FALSE,
  ...
)
```

## Arguments

- object:

  An object of class
  [MizerSim](https://sizespectrum.org/mizer/reference/MizerSim-class.html)
  or
  [MizerParams](https://sizespectrum.org/mizer/reference/MizerParams-class.html).

- species:

  The species to be selected. Optional. By default all target species
  are selected. A vector of species names, or a numeric vector with the
  species indices, or a logical vector indicating for each species
  whether it is to be selected (TRUE) or not.

- resources:

  The resources to be selected. Optional. By default all resources are
  selected. A vector of resource names, or a numeric vector with the
  resource indices, or a logical vector indicating for each resource
  whether it is to be selected (TRUE) or not.

- time_range:

  The time range to average over when called with a
  [MizerSim](https://sizespectrum.org/mizer/reference/MizerSim-class.html)
  object.

- geometric_mean:

  Whether to average abundances using a geometric mean when called with
  a
  [MizerSim](https://sizespectrum.org/mizer/reference/MizerSim-class.html)
  object.

- wlim:

  A numeric vector of length two providing lower and upper limits for
  the w axis. Use NA to refer to the existing minimum or maximum.

- ylim:

  A numeric vector of length two providing lower and upper limits for
  the y axis. Use NA to refer to the existing minimum or maximum. Any
  values below 1e-20 are always cut off.

- power:

  The abundance is plotted as the number density times the weight raised
  to `power`. The default `power = 1` gives the biomass density, whereas
  `power = 2` gives the biomass density with respect to logarithmic size
  bins.

- biomass:

  Deprecated compatibility argument. If `power` is missing, `power` is
  set to `as.numeric(biomass)`.

- total:

  A boolean value that determines whether the total over all species and
  resources in the system is plotted as well. Note that even if the plot
  only shows a selection of species, the total is including all species.
  Default is FALSE.

- background:

  A boolean value that determines whether background species are
  included. Ignored if the model does not contain background species.
  Default is TRUE.

- highlight:

  Name or vector of names of the species to be highlighted.

- return_data:

  A boolean value that determines whether the formatted data used for
  the plot is returned instead of the plot itself. Default value is
  FALSE

- ...:

  Other arguments (currently unused)

## Value

A ggplot2 object, unless `return_data = TRUE`, in which case a data
frame with the four variables 'w', 'value', 'Species', 'Legend' is
returned.

## Details

When called with a
[MizerSim](https://sizespectrum.org/mizer/reference/MizerSim-class.html)
object, the abundance is averaged over the specified time range (a
single value for the time range can be used to plot a single time step).
When called with a
[MizerParams](https://sizespectrum.org/mizer/reference/MizerParams-class.html)
object the initial abundance is plotted.

## See also

[plotting_functions](https://sizespectrum.org/mizer/reference/plotting_functions.html)

Other plotting functions:
[`animateSpectra`](https://sizespectrum.org/mizerMR/reference/animateSpectra.md),
[`plotDiet`](https://sizespectrum.org/mizerMR/reference/plotDiet.md),
[`plotResourceLevel()`](https://sizespectrum.org/mizerMR/reference/plotResourceLevel.md),
[`plotResourcePred()`](https://sizespectrum.org/mizerMR/reference/plotResourcePred.md)
