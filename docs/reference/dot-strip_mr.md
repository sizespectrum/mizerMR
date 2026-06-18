# Strip the MR extension from params

Removes the MR extension state so that mizer's own methods can run
without triggering the MR encounter method. Also restores the built-in
resource from the stored mizer resource parameters so that steady-state
calculations have a valid single-resource spectrum.

## Usage

``` r
.strip_mr(params)
```

## Arguments

- params:

  A
  [mizerMR](https://sizespectrum.org/mizerMR/reference/mizerMR-class.md)
  object.

## Value

A
[MizerParams](https://sizespectrum.org/mizer/reference/MizerParams-class.html)
object without the MR extension.
