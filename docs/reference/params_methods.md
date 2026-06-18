# Multiple-resource methods for model rescaling and reporting

S3 methods that make mizer's
[`scaleModel()`](https://sizespectrum.org/mizer/reference/scaleModel.html),
[`scaleRates()`](https://sizespectrum.org/mizer/reference/scaleRates.html),
[`setResource()`](https://sizespectrum.org/mizer/reference/setResource.html)
and [`summary()`](https://rdrr.io/r/base/summary.html) aware of the
multiple-resource component. The base `MizerParams` methods only know
about the single built-in resource, which
[`setMultipleResources()`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
silences, so they would otherwise ignore (or, for
[`scaleModel()`](https://sizespectrum.org/mizer/reference/scaleModel.html)
and
[`setResource()`](https://sizespectrum.org/mizer/reference/setResource.html),
error on) the resources stored in the `MR` component.

Extends
[`mizer::scaleModel()`](https://sizespectrum.org/mizer/reference/scaleModel.html)
so that the resource carrying capacities and abundances of all resources
are rescaled consistently. The base method scales the fish spectra and
the resource *abundances* (held in `initial_n_other`), but not the
resource *capacities* and `kappa` coefficients, which live in the `MR`
component's parameters. The resource replenishment *rate* is left
unchanged, exactly as the built-in resource rate is in the base method,
so that the steady state is preserved.

Extends
[`mizer::scaleRates()`](https://sizespectrum.org/mizer/reference/scaleRates.html)
so that the resource replenishment rate of all resources is rescaled by
`factor` along with the consumer rates. Without this the base method
would scale only the silenced built-in resource rate, leaving the active
resource rates untouched and breaking the rescaling invariant (resource
replenishment keeping pace with the rescaled search volume).

Extends
[`mizer::setResource()`](https://sizespectrum.org/mizer/reference/setResource.html).
In a multiple-resource model the built-in single resource is silenced
and plays no part in the dynamics, so changing it with
[`setResource()`](https://sizespectrum.org/mizer/reference/setResource.html)
has no effect on the simulation. This method warns when the user tries
to change the resource rate, capacity or level, and directs them to
[`setMultipleResources()`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md)
and the related setters. Calls that only change, for example, the
built-in resource dynamics (as mizer does internally) pass through
silently. The work is delegated to the base method on the plain class so
that the base accessors return the (vector-valued) built-in resource
instead of the resource-by-size arrays of the MR component.

Extends the [`summary()`](https://rdrr.io/r/base/summary.html) method
for `MizerParams` objects. The base method reports the resource size
spectrum from the silenced built-in resource, which is empty for a
multiple-resource model. This method instead reports the overall size
range spanned by all resources combined.

## Usage

``` r
# S3 method for class 'mizerMR'
scaleModel(params, factor, ...)

# S3 method for class 'mizerMR'
scaleRates(params, factor, ...)

# S3 method for class 'mizerMR'
setResource(params, ...)

# S3 method for class 'mizerMR'
summary(object, ...)
```

## Arguments

- params:

  A
  [mizerMR](https://sizespectrum.org/mizerMR/reference/mizerMR-class.md)
  object.

- factor:

  The factor by which to rescale.

- ...:

  Further arguments passed along the mizer method chain.

- object:

  A
  [mizerMR](https://sizespectrum.org/mizerMR/reference/mizerMR-class.md)
  object.

## Value

For
[`scaleModel()`](https://sizespectrum.org/mizer/reference/scaleModel.html),
[`scaleRates()`](https://sizespectrum.org/mizer/reference/scaleRates.html)
and
[`setResource()`](https://sizespectrum.org/mizer/reference/setResource.html):
the updated `mizerMR` object. For
[`summary()`](https://rdrr.io/r/base/summary.html): the object,
invisibly.
