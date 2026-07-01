# Upgrade a mizerMR object from an earlier version of mizerMR

This is the `mizerMR` method of the
[`utils::upgrade()`](https://rdrr.io/r/utils/upgrade.html) generic (on
which mizer registers its own methods). It performs only the
mizerMR-specific migration and is invoked by mizer's upgrade
orchestrator (from
[`validParams()`](https://sizespectrum.org/mizer/reference/validParams.html)
/
[`readParams()`](https://sizespectrum.org/mizer/reference/saveParams.html))
when the version stamp recorded in `params@extensions[["mizerMR"]]` is
older than the installed mizerMR, or is missing. The orchestrator
records the new stamp afterwards, so this method must **not** stamp the
version itself and must **not** call
[`NextMethod()`](https://rdrr.io/r/base/UseMethod.html). It is written
to be idempotent.

## Usage

``` r
# S3 method for class 'mizerMR'
upgrade(object, ...)
```

## Arguments

- object:

  A
  [mizerMR](https://sizespectrum.org/mizerMR/reference/mizerMR-class.md)
  object to be upgraded.

- ...:

  Unused.

## Value

The upgraded object.

## Details

Migrations performed:

- The `resource_params` data frame used to be stored under
  `other_params(params)[["MR"]]$resource_params`, i.e. in
  `params@other_params$other$MR$resource_params`. It now lives alongside
  the other MR component parameters in
  `params@other_params$MR$resource_params`. This method moves it to the
  new location.
