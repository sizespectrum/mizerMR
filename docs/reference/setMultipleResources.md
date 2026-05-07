# Set up multiple resources

Set up multiple resources

## Usage

``` r
setMultipleResources(
  params,
  resource_params = NULL,
  resource_interaction = NULL,
  resource_capacity = NULL,
  resource_rate = NULL,
  initial_resource = NULL
)

resource_capacity(params)

resource_capacity(params) <- value

resource_rate(params)

resource_rate(params) <- value

resource_interaction(params)

resource_interaction(params) <- value

# S3 method for class 'mizerMR'
initialNResource(object)

# S3 method for class 'mizerMR'
initialNResource(params) <- value
```

## Arguments

- params:

  A MizerParams object

- resource_params:

  A data frame with the resource parameters

- resource_interaction:

  Optional interaction matrix between species and resources (predator
  species x prey resource). By default all entries are 1.

- resource_capacity:

  Optional. Array (resource x size) of the intrinsic resource carrying
  capacities

- resource_rate:

  Optional. Array (resource x size) of intrinsic resource growth rates

- initial_resource:

  Optional. Array (resource x size) of initial values

- value:

  Value to assign

- object:

  A mizerMR object
