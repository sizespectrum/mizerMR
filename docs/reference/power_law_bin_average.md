# Bin-average of a power law over a restricted size range

Internal helper. Returns the exact average of \\w^d\\ over each grid bin
\\\[w_j, w\_{j+1}\]\\, restricted to the size range \\\[w\_{min},
w\_{max}\]\\ (the power law is taken to be zero outside it) and divided
by the full bin width \\\Delta w_j\\. A bin straddling either knife edge
gets the partial average over the in-range part of the bin; bins
entirely outside the range get zero. This is the same finite-volume
convention mizer uses internally, extended with a lower cutoff for
mizerMR's per-resource size ranges.

## Usage

``` r
power_law_bin_average(w, dw, d, w_min = -Inf, w_max = Inf)
```

## Arguments

- w:

  Numeric vector of left bin edges \\w_j\\.

- dw:

  Numeric vector of bin widths \\\Delta w_j\\ (same length as `w`).

- d:

  Single numeric exponent of the power law.

- w_min, w_max:

  Lower and upper cutoffs of the size range. Default to no cutoff
  (`-Inf` and `Inf`).

## Value

A numeric vector (same length as `w`) of the restricted bin averages.
