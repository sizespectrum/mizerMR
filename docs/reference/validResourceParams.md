# Validate resource parameter data frame

Check validity of resource parameters and set defaults for missing but
required parameters

## Usage

``` r
validResourceParams(resource_params, min_w)
```

## Arguments

- resource_params:

  The user-supplied resource parameter data frame

- min_w:

  The smallest allowed resource size

## Value

A valid resource parameter data frame

This function throws an error if

- the `resource` column does not exist or contains duplicates

- `w_min` is not smaller than `w_max`

- `w_min` is smaller than `min_w`

- any parameter is negative

It sets default values if any of the following are missing or NA

- `kappa` is set to `0.1`

- `lambda` is set to `2.05`

- `r_pp` is set to `4`

- `w_min` is set to `min_w`

- `w_max` is set to `10`

- `n` is set to 2/3

- `dynamics` is set to "resource_semichemostat"

- `colour` is drawn from a colour-blind-friendly palette

- `linetype` is set to "solid"

If `resource_params` was provided as a tibble it is converted back to an
ordinary data frame.
