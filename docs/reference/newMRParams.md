# Create a new multi-resource, multi-species model

Create a new multi-resource, multi-species model

## Usage

``` r
newMRParams(
  species_params,
  interaction = NULL,
  resource_params,
  resource_interaction = NULL,
  gear_params = data.frame(),
  no_w = 100,
  min_w = 0.001,
  max_w = NA
)
```

## Arguments

- species_params:

  A data frame with the species parameters, with one row for each
  species.

- interaction:

  Optional interaction matrix between species (predator species x prey
  species). Entries should be numbers between 0 and 1. By default all
  entries are 1.

- resource_params:

  A data frame with the resource parameters

- resource_interaction:

  Optional interaction matrix between species and resources (predator
  species x prey resource). By default all entries are 1.

- gear_params:

  A data frame with the gear parameters, with one row for each
  gear-species pair.

- no_w:

  The number of size bins in the consumer spectrum.

- min_w:

  Sets the size of the eggs of all species for which this is not given
  in the `w_min` column of the `species_params` dataframe.

- max_w:

  The largest size of the consumer spectrum. By default this is set to
  the largest `w_max specified in the `species_params\` data frame.

## Value

An object of class MizerMRParams
