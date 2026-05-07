# Set a resource parameter to a default value

If the resource parameter does not yet exist in the resource parameter
data frame, then create it and fill it with the default. Otherwise use
the default only to fill in any NAs. Optionally gives a message if the
parameter did not already exist.

## Usage

``` r
set_resource_param_default(resource_params, parname, default, message = NULL)
```

## Arguments

- resource_params:

  A resource parameter data frame

- parname:

  A string with the name of the resource parameter to set

- default:

  A single default value or a vector with one default value for each
  resource

- message:

  A string with a message to be issued when the parameter did not
  already exist

## Value

The resource params data frame with an updated column.
