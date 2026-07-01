# Whether second-order bin-averaging is switched on

Internal helper reading the `bin_average` entry of the model's
`second_order_w` slot via mizer's accessor. Returns `FALSE` for mizer
versions that predate the slot, so that mizerMR keeps its first-order
behaviour against older mizer.

## Usage

``` r
mr_bin_average(params)
```

## Arguments

- params:

  A
  [MizerParams](https://sizespectrum.org/mizer/reference/MizerParams-class.html)
  object.

## Value

`TRUE` or `FALSE`.
