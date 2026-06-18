# Add species to a mizerMR model

Extends
[`mizer::addSpecies()`](https://sizespectrum.org/mizer/reference/addSpecies.html)
to handle the multiple-resource interaction matrix. New species are
added with a resource interaction of 1 for all resources by default, or
as specified by `resource_interaction`.

## Usage

``` r
# S3 method for class 'mizerMR'
addSpecies(
  params,
  species_params,
  gear_params = data.frame(),
  initial_effort,
  interaction,
  steady = TRUE,
  info_level = 0,
  ...,
  resource_interaction = NULL
)
```

## Arguments

- params:

  A
  [mizerMR](https://sizespectrum.org/mizerMR/reference/mizerMR-class.md)
  object.

- species_params:

  A data frame with the species parameters of the new species.

- gear_params:

  Data frame with the gear parameters for the new species. If not
  provided then the new species will not be fished.

- initial_effort:

  A named vector with the effort for any new fishing gear introduced in
  `gear_params`. Not needed if the added species are only fished by
  already existing gear. Should not include effort values for existing
  gear. New gear for which no effort is set via this vector will have an
  initial effort of 0.

- interaction:

  Interaction matrix. A square matrix giving either the interaction
  coefficients between all species or only those between the new
  species. In the latter case all interaction between an old and a new
  species are set to 1. If this argument is missing, all interactions
  involving a new species are set to 1.

- steady:

  If `TRUE` (default), runs
  [`steadySingleSpecies()`](https://sizespectrum.org/mizer/reference/steadySingleSpecies.html)
  to initialise the new species at their single-species steady state and
  retuning their reproductive efficiencies. Set to `FALSE` when the
  caller (e.g. an extension package using
  [`NextMethod()`](https://rdrr.io/r/base/UseMethod.html)) needs to make
  further changes to the params object before that steady-state
  calculation can be run successfully.

- info_level:

  Controls the amount of information messages that are shown when the
  function sets default values for parameters. Higher levels lead to
  more messages. Set to 0 to suppress all such messages.

- ...:

  Currently unused.

- resource_interaction:

  Optional matrix (new species x resources) of interaction values
  between new species and each resource. Defaults to 1 for all new
  species and resources.

## Value

A [mizerMR](https://sizespectrum.org/mizerMR/reference/mizerMR-class.md)
object with the new species added.
