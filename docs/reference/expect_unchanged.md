# Expect a mizer object to be unchanged

Compares two mizer objects after normalising fields that are expected to
change during setup or projection.

## Usage

``` r
expect_unchanged(object, expected)
```

## Arguments

- object:

  A
  [MizerParams](https://sizespectrum.org/mizer/reference/MizerParams-class.html)
  or
  [MizerSim](https://sizespectrum.org/mizer/reference/MizerSim-class.html)
  object.

- expected:

  The expected object.

## Value

The expectation result from
[`testthat::expect_equal()`](https://testthat.r-lib.org/reference/equality-expectations.html).
