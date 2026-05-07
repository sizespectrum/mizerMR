# Remove species from a mizerMR model

Extends
[`mizer::removeSpecies()`](https://sizespectrum.org/mizer/reference/removeSpecies.html)
to also trim the resource interaction matrix.

## Usage

``` r
# S3 method for class 'mizerMR'
removeSpecies(params, species, ...)
```

## Arguments

- params:

  A
  [mizerMR](https://sizespectrum.org/mizerMR/reference/mizerMR-class.md)
  object.

- species:

  The species to be removed. A vector of species names, or a numeric
  vector of species indices, or a logical vector indicating for each
  species whether it is to be removed (TRUE) or not.

- ...:

  Additional arguments

## Value

A [mizerMR](https://sizespectrum.org/mizerMR/reference/mizerMR-class.md)
object with the specified species removed.
