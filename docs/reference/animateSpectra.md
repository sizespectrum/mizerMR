# Animation of the abundance spectra

**\[experimental\]**

## Usage

``` r
# S3 method for class 'mizerMRSim'
animateSpectra(
  sim,
  species = NULL,
  time_range,
  wlim = c(NA, NA),
  ylim = c(NA, NA),
  power = 1,
  total = FALSE,
  resource = TRUE,
  background = TRUE,
  ...,
  resources = NULL
)
```

## Arguments

- sim:

  A MizerSim object

- species:

  Name or vector of names of the species to be plotted. By default all
  species are plotted.

- time_range:

  The time range to animate over. Either a vector of values or a vector
  of min and max time. Default is the entire time range of the
  simulation.

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

- total:

  A boolean value that determines whether the total over all species in
  the system is plotted as well. Default is FALSE.

- resource:

  Deprecated compatibility argument from mizer's generic.

- background:

  Deprecated compatibility argument from mizer's generic.

- ...:

  Other arguments (currently unused).

- resources:

  Name or vector of names of the resources to be plotted. By default all
  resources are plotted.

## Value

A plotly object

## See also

Other plotting functions:
[`plotDiet`](https://sizespectrum.org/mizerMR/reference/plotDiet.md),
[`plotResourceLevel()`](https://sizespectrum.org/mizerMR/reference/plotResourceLevel.md),
[`plotResourcePred()`](https://sizespectrum.org/mizerMR/reference/plotResourcePred.md),
[`plotSpectra`](https://sizespectrum.org/mizerMR/reference/plotSpectra.md)
