# S3 class for time x resource x size arrays

[`NResource()`](https://sizespectrum.org/mizerMR/reference/NResource.md)
for a mizerMR simulation returns a three-dimensional array (time x
resource x size) holding the resource number densities through time. The
`MRArrayTimeByResourceBySize` class wraps this array to provide
convenient [`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html), and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) methods.
It is the multiple-resource analogue of mizer's
`ArrayTimeByResourceBySize` class.

## Usage

``` r
MRArrayTimeByResourceBySize(x, value_name = NULL, units = NULL, params = NULL)
```

## Arguments

- x:

  A three-dimensional array (time x resource x size).

- value_name:

  A string giving the human-readable name for the value.

- units:

  A string giving the units (e.g. "1/g").

- params:

  A MizerParams object. Used for the resource colours and the size grid
  in the [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  method.

## Value

An `MRArrayTimeByResourceBySize` object (inherits from `array`).

## See also

[`mizer::ArrayTimeByResourceBySize()`](https://sizespectrum.org/mizer/reference/ArrayTimeByResourceBySize.html)
