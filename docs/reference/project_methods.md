# Multiple-resource project methods

Internal helpers and S3 methods used to make mizer's rate and projection
generics work with the multiple-resource component.

Internal projection hook for predation mortality on multiple resources.

## Usage

``` r
mizerMRBaseResource(params)

mizerMRValidBaseResource(params, n_pp)

# S3 method for class 'mizerMR'
getEncounter(
  params,
  n = initialN(params),
  n_pp = mizerMRBaseResource(params),
  n_other = initialNOther(params),
  t = 0,
  ...
)

# S3 method for class 'mizerMR'
getPredRate(
  params,
  n = initialN(params),
  n_pp = mizerMRBaseResource(params),
  n_other = initialNOther(params),
  t = 0,
  ...
)

# S3 method for class 'mizerMR'
getResourceMort(
  params,
  n = initialN(params),
  n_pp = mizerMRBaseResource(params),
  n_other = initialNOther(params),
  t = 0,
  ...
)

# S3 method for class 'mizerMR'
getRates(
  params,
  n = initialN(params),
  n_pp = mizerMRBaseResource(params),
  n_other = initialNOther(params),
  effort,
  t = 0,
  ...
)

# S3 method for class 'mizerMR'
projectEncounter(params, n, n_pp, n_other, t = 0, ...)

mizerMREncounter(params, n, n_pp, n_other, t = 0, ...)

mizerMRResourceEncounter(params, n_other)

# S3 method for class 'mizerMR'
projectResourceMort(params, n, n_pp, n_other, t = 0, pred_rate, ...)
```

## Arguments

- params:

  A
  [mizerMR](https://sizespectrum.org/mizerMR/reference/mizerMR-class.md)
  object.

- n_pp:

  A resource abundance vector, or an MR resource array that will be
  replaced by the silenced built-in resource vector before calling
  mizer's base method.

- n:

  A matrix of species abundances (species x size).

- n_other:

  A list of abundances for other dynamical components.

- t:

  Current projection time.

- ...:

  Further arguments passed along the mizer method chain.

- effort:

  Fishing effort by gear.

## Value

The return value depends on the method called: a rate array, a rate
list, or a resource abundance vector.
