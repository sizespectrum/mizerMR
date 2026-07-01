# Plot mizerMR resource arrays

[`plot()`](https://rdrr.io/r/graphics/plot.default.html) creates a
ggplot2 figure with one line for each resource, showing the values
against size. It is the multiple-resource analogue of the
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) method for
mizer's `ArrayResourceBySize` objects and reuses the same internal mizer
plotting machinery, so resources are drawn in the colours registered in
the `params` object.

## Usage

``` r
# S3 method for class 'MRArrayResourceBySize'
plot(
  x,
  resources = NULL,
  return_data = FALSE,
  log_x = TRUE,
  log_y = TRUE,
  log = NULL,
  wlim = c(NA, NA),
  ylim = c(NA, NA),
  y_ticks = 6,
  ...
)

# S3 method for class 'MRArrayResourceBySize'
plotHover(x, ...)

# S3 method for class 'MRArrayTimeByResourceBySize'
plot(x, time = NULL, ...)

# S3 method for class 'MRArrayTimeByResourceBySize'
plotHover(x, ...)
```

## Arguments

- x:

  An `MRArrayResourceBySize` or `MRArrayTimeByResourceBySize` object.

- resources:

  Optional vector of resource names (or indices) to restrict the plot
  to.

- return_data:

  Logical. If TRUE, return the plotting data frame instead of the plot.

- log_x, log_y:

  Logical flags for logarithmic axes.

- log:

  Alternative way to specify the log axes, see
  [`mizer::parsePlotLog()`](https://sizespectrum.org/mizer/reference/parsePlotLog.html).

- wlim, ylim:

  Length-2 numeric vectors giving the weight and value limits.

- y_ticks:

  Approximate number of ticks on the y-axis.

- ...:

  Unused.

- time:

  For `MRArrayTimeByResourceBySize`, the time at which to plot the
  spectrum. Defaults to the final time step.

## Value

A ggplot object, or a data frame if `return_data = TRUE`.
