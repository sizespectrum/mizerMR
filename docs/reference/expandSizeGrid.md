# Expand the size grid of a mizerMR model

Extends
[`mizer::expandSizeGrid()`](https://sizespectrum.org/mizer/reference/expandSizeGrid.html)
to handle
[mizerMR](https://sizespectrum.org/mizerMR/reference/mizerMR-class.md)
objects. The resource rate, capacity, and initial abundance arrays are
recalculated on the new grid from the resource parameters.

## Usage

``` r
# S3 method for class 'mizerMR'
expandSizeGrid(
  params,
  new_min_w = min(params@w),
  new_max_w = max(params@w),
  preserve_species = params@species_params$species,
  ...
)
```

## Arguments

- params:

  A
  [mizerMR](https://sizespectrum.org/mizerMR/reference/mizerMR-class.md)
  object.

- new_min_w:

  The new minimum size in the grid. Defaults to the current minimum.

- new_max_w:

  The new maximum size in the grid. Defaults to the current maximum.

- preserve_species:

  A vector of species names for which all rate arrays should be copied
  over to the new params object rather than being re-calculated from the
  species parameters. If missing, all species are preserved.

- ...:

  Additional arguments (currently unused).

## Value

A [mizerMR](https://sizespectrum.org/mizerMR/reference/mizerMR-class.md)
object with an expanded size grid.
