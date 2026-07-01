# S3 class for multiple-resource size spectra

Several mizerMR functions return a `resource x size` matrix holding a
resource-related quantity such as the resource number density or the
resource mortality. The `MRArrayResourceBySize` class wraps such a
matrix to provide convenient
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html), and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) methods.
It is the multiple-resource analogue of mizer's `ArrayResourceBySize`
class.

## Usage

``` r
MRArrayResourceBySize(x, value_name = NULL, units = NULL, params = NULL)
```

## Arguments

- x:

  A matrix (resource x size), with resource names as row names.

- value_name:

  A string giving the human-readable name for the value.

- units:

  A string giving the units (e.g. "1/year").

- params:

  A MizerParams object. Used for the resource colours and the size grid
  in the [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  method.

## Value

An `MRArrayResourceBySize` object (inherits from `matrix`/`array`).

## Details

An `MRArrayResourceBySize` object behaves just like a regular matrix for
arithmetic and subsetting. It carries `value_name`, `units` and `params`
attributes, mirroring the mizer array classes.

## See also

[`mizer::ArrayResourceBySize()`](https://sizespectrum.org/mizer/reference/ArrayResourceBySize.html)
