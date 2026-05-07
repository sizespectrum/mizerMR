# Rename species in a mizerMR model

Extends
[`mizer::renameSpecies()`](https://sizespectrum.org/mizer/reference/renameSpecies.html)
to also update the row names of the resource interaction matrix.

## Usage

``` r
# S3 method for class 'mizerMR'
renameSpecies(params, replace, ...)
```

## Arguments

- params:

  A
  [mizerMR](https://sizespectrum.org/mizerMR/reference/mizerMR-class.md)
  object.

- replace:

  A named character vector, with new names as values, and old names as
  names.

- ...:

  Additional arguments

## Value

A [mizerMR](https://sizespectrum.org/mizerMR/reference/mizerMR-class.md)
object with the species renamed.
