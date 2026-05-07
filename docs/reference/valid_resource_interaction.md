# Return valid resource interaction array

If `resource interaction` is given it is checked for validity and
returned. Otherwise the value stored in `params` is returned.

## Usage

``` r
valid_resource_interaction(params, resource_interaction = NULL)
```

## Arguments

- params:

  A MizerParams object

- resource_interaction:

  Interaction matrix between species and resources (predator species x
  prey resource). By default all entries are 1.

## Value

An array (resource x size)
