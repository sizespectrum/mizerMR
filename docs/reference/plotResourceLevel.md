# Plot the proportion of the resource spectrum(s) compared to their carrying capacity

Plot the proportion of the resource spectrum(s) compared to their
carrying capacity

## Usage

``` r
plotResourceLevel(object, return_data = FALSE)
```

## Arguments

- object:

  An object of class
  [MizerSim](https://sizespectrum.org/mizer/reference/MizerSim-class.html)
  or
  [MizerParams](https://sizespectrum.org/mizer/reference/MizerParams-class.html).

- return_data:

  A boolean value that determines whether the formatted data used for
  the plot is returned instead of the plot itself. Default value is
  FALSE

## Value

A ggplot2 object, unless `return_data = TRUE`, in which case a data
frame with the three variables 'w', 'value', 'Resource' is returned.

## See also

[plotting_functions](https://sizespectrum.org/mizer/reference/plotting_functions.html)

Other plotting functions:
[`animateSpectra`](https://sizespectrum.org/mizerMR/reference/animateSpectra.md),
[`plotDiet`](https://sizespectrum.org/mizerMR/reference/plotDiet.md),
[`plotResourcePred()`](https://sizespectrum.org/mizerMR/reference/plotResourcePred.md),
[`plotSpectra`](https://sizespectrum.org/mizerMR/reference/plotSpectra.md)

## Examples

``` r
# \donttest{
plotResourceLevel(NS_params)


# Returning the data frame
fr <- plotResourceLevel(NS_params, return_data = TRUE)
str(fr)
#> 'data.frame':    179 obs. of  3 variables:
#>  $ w       : num  2.12e-13 2.53e-13 3.02e-13 3.61e-13 4.30e-13 ...
#>  $ value   : num  1 1 1 1 1 ...
#>  $ Resource: chr  "Resource" "Resource" "Resource" "Resource" ...
# }
```
