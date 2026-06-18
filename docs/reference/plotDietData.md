# Format and plot MR diet data

Converts an MR diet array to the data frame expected by
[`plotDataFrame()`](https://sizespectrum.org/mizer/reference/plotDataFrame.html),
or returns that data frame directly.

## Usage

``` r
plotDietData(params, diet, species = NULL, return_data = FALSE)
```

## Arguments

- params:

  A
  [mizerMR](https://sizespectrum.org/mizerMR/reference/mizerMR-class.md)
  object.

- diet:

  A diet array as returned by
  [`getDiet()`](https://sizespectrum.org/mizerMR/reference/getDiet.md).

- species:

  Optional predator species selection.

- return_data:

  Whether to return the plotting data instead of a plot.

## Value

A ggplot2 object or, if `return_data = TRUE`, a data frame.
