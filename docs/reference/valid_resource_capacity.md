# Return valid resource capacity array

If `resource capacity` is given it is checked for validity. If it does
not have a comment, then it is given the comment "set manually". This is
then returned. If `resource capacity` is missing or NULL, but one was
set by the user and stored in `params` with a comment, then this is
returned. Otherwise a resource capacity is calculated from
`resource_params`. If this is NULL to it is taken from `params`.

## Usage

``` r
valid_resource_capacity(
  params,
  resource_params = NULL,
  resource_capacity = NULL
)
```

## Arguments

- params:

  A MizerParams object

- resource_params:

  A data frame with the resource parameters

- resource_capacity:

  Array (resource x size) of the intrinsic resource carrying capacities

## Value

An array (resource x size) with the resource capacities
