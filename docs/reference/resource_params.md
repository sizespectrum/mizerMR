# Resource parameters

The resource parameter data frame has one row for each resource. The
variables are:

- `resource` Name you want to use for the resource

- `kappa` Coefficient in the carrying capacity power law

- `lambda` Exponent of the carrying capacity power law

- `r_pp` Coefficient in the allometric replenishment rate

- `w_min` Smallest size of the resource

- `w_max` Largest size of the resource

- `n` Exponent for allometric scaling of replenishment rate

- `dynamics` Name of the resource dynamics function

- `colour` Colour with which to plot the resource

- `linetype` Linetype with which to plot the resource

## Usage

``` r
resource_params(params)

resource_params(params) <- value
```

## Arguments

- params:

  A MizerParams object

- value:

  A data frame with the resource parameters

## Details

Except for `resource`, all of these get default value if you do not
supply them, see
[`validResourceParams()`](https://sizespectrum.org/mizerMR/reference/validResourceParams.md)

## See also

[`validResourceParams()`](https://sizespectrum.org/mizerMR/reference/validResourceParams.md)
