# Get diet of predator at size, resolved by prey species

Calculates the rate at which a predator of a particular species and size
consumes biomass of each prey species and resource. The diet has units
of grams/year.

## Usage

``` r
# S3 method for class 'mizerMR'
getDiet(
  params,
  n = initialN(params),
  n_pp = NULL,
  n_other = initialNOther(params),
  proportion = TRUE
)
```

## Arguments

- params:

  A
  [MizerParams](https://sizespectrum.org/mizer/reference/MizerParams-class.html)
  object

- n:

  A matrix of species abundances (species x size).

- n_pp:

  A vector of the resource abundance by size

- n_other:

  A list of abundances for other dynamical components of the

- proportion:

  If TRUE (default) the function returns the diet as a proportion of the
  total consumption rate. If FALSE it returns the consumption rate in
  grams per year.

## Value

An array (predator species x predator size x (prey species + resource +
other components) )

## Details

Returns the rates \\D\_{ij}(w)\\ at which a predator of species \\i\\
and size \\w\\ consumes biomass from prey species \\j\\. This is
calculated from the predation kernel \\\phi_i(w, w_p)\\, the search
volume \\\gamma_i(w)\\, the feeding level \\f_i(w)\\, the species
interaction matrix \\\theta\_{ij}\\ and the prey abundance density
\\N_j(w_p)\\: \$\$ D\_{ij}(w, w_p) = (1-f_i(w)) \gamma_i(w) \theta\_{ij}
\int N_j(w_p) \phi_i(w, w_p) w_p dw_p. \$\$ The prey index \\j\\ runs
over all species and the resource. It also runs over any extra ecosystem
components in your model for which you have defined an encounter rate
function. This encounter rate is multiplied by \\1-f_i(w)\\ to give the
rate of consumption of biomass from these extra components.

This function performs the same integration as
[`getEncounter()`](https://sizespectrum.org/mizer/reference/getEncounter.html)
but does not aggregate over prey species, and multiplies by \\1-f_i(w)\\
to get the consumed biomass rather than the available biomass. Outside
the range of sizes for a predator species the returned rate is zero.

## See also

[`plotDiet()`](https://sizespectrum.org/mizerMR/reference/plotDiet.md)

## Examples

``` r
diet <- getDiet(NS_params)
str(diet)
#>  num [1:12, 1:100, 1:14] 8.94e-18 6.86e-19 3.46e-18 1.75e-09 1.12e-17 ...
#>  - attr(*, "dimnames")=List of 3
#>   ..$ predator: chr [1:12] "Sprat" "Sandeel" "N.pout" "Herring" ...
#>   ..$ w       : chr [1:100] "0.001" "0.00119" "0.00142" "0.0017" ...
#>   ..$ prey    : chr [1:14] "Sprat" "Sandeel" "N.pout" "Herring" ...
```
