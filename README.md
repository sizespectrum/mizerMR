
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mizerMR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

Here we are starting to develop a mizer add-on package that implements
multiple resources. So far this is just a skeleton and not yet
functional.

# Setting up a model with multiple resources

You can add multiple resources to any existing MizerParams object
`params` with

``` r
params <- setMultipleResources(params, resource_params, resource_interaction)
```

where `resource_params` is a data frame with one row for each resource
and columns `kappa`, `lambda`, `r_pp`, `w_min`, `w_max`, and
`resource_interaction` is a matrix with one row for each species and one
column for each resource giving the strength of interaction between the
species and the resource.

Alternatively and equivalently this could be achieved with

``` r
resource_params(params) <- resource_params
resource_interaction(params) <- resource_interaction
```

By default the initial values for the resources will be equal to the
carrying capacities. But if a user has their own array (resource x size)
of resource number densities `my_n_resource` they can set these as the
intial values with

``` r
initialNResource(params) <- `my_n_resource`
```

Users who are not happy with the allometric expressions for the resource
carrying capacities or the replenishment rates but have created their
own matrices `my_capacity` and `my_rate` with one row for each resource
and one column for each size can set these with

``` r
resource_capacity(params) <- my_capacity
resource_rate(params) <- my_rate
```

Rather than adding the multiple resources to an existing model, the user
can also create a new model with multiple resources with

``` r
params <- newMRParams(species_params, interaction, resource_params,
                      resource_interaction, gear_params)
```

# Projecting the model

The MizerParams object with multiple resources will be simulated as
usual with `project()` and its siblings. The mizer extension mechanism
ensures that the resource dynamics are reflected correctly.

Each resource will follow its semichemostat dynamics unless the user
specifies a different resource dynamics function via the
`dynamics` slot in the `resource_params` data frame.

# Analysing the model

A MizerSim `sim` object obtained via `project()` from a MizerParams
object with multiple resources, will also contain the abundances of the
resources over time. These can be assessed with `NResource(sim)`, which
will return an array (time x resource x size), similar to how `N(sim)`
returns the fish abundances as an array (time x species x size). The
only difference is that the size for the resource runs over the larger
set of values `w_full`.

`plotSpectra()` behaves like the one in mizer but it shows one line for
each resource. Most other functions can be used just as in mizer.
