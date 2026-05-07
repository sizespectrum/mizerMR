# Plot diet, resolved by prey species, as function of predator at size.

**\[experimental\]** Plots the proportions with which each prey species
contributes to the total biomass consumed by the specified predator
species, as a function of the predator's size. These proportions are
obtained with
[`getDiet()`](https://sizespectrum.org/mizerMR/reference/getDiet.md).

## Usage

``` r
# S3 method for class 'mizerMR'
plotDiet(
  object,
  species = NULL,
  time_range,
  wlim = c(1, NA),
  return_data = FALSE,
  ...
)

# S3 method for class 'mizerMRSim'
plotDiet(
  object,
  species = NULL,
  time_range,
  wlim = c(1, NA),
  return_data = FALSE,
  ...
)

plotDietMR(object, ...)

plotlyDiet(object, species = NULL, time_range, wlim = c(1, NA), ...)
```

## Arguments

- object:

  An object of class
  [MizerSim](https://sizespectrum.org/mizer/reference/MizerSim-class.html)
  or
  [MizerParams](https://sizespectrum.org/mizer/reference/MizerParams-class.html).

- species:

  The name of the predator species for which to plot the diet.

- time_range:

  The time range (either a vector of values, a vector of min and max
  time, or a single value) to average the abundances over. Default is
  the final time step. Ignored when called with a
  [MizerParams](https://sizespectrum.org/mizer/reference/MizerParams-class.html)
  object.

- wlim:

  A numeric vector of length two providing lower and upper limits for
  the w axis. Use NA to refer to the existing minimum or maximum.

- return_data:

  A boolean value that determines whether the formatted data used for
  the plot is returned instead of the plot itself. Default value is
  FALSE

- ...:

  Other arguments (currently unused)

## Value

A ggplot2 object, unless `return_data = TRUE`, in which case a data
frame with the three variables 'w', 'Proportion', 'Prey' is returned.

## Details

Prey species that contribute less than 1 permille to the diet are
suppressed in the plot.

## See also

[`getDiet()`](https://sizespectrum.org/mizerMR/reference/getDiet.md)

Other plotting functions:
[`animateSpectra`](https://sizespectrum.org/mizerMR/reference/animateSpectra.md),
[`plotResourceLevel()`](https://sizespectrum.org/mizerMR/reference/plotResourceLevel.md),
[`plotResourcePred()`](https://sizespectrum.org/mizerMR/reference/plotResourcePred.md),
[`plotSpectra`](https://sizespectrum.org/mizerMR/reference/plotSpectra.md)
