# Return valid initial resource array

If `initial_resource` is given it is checked for validity and returned.
Otherwise the value stored in `params` is returned.

## Usage

``` r
valid_initial_resource(params, initial_resource = NULL)
```

## Arguments

- params:

  A MizerParams object

- initial_resource:

  Array (resource x size) of initial values

## Value

An array (resource x size)
