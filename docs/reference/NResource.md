# Access resource abundances in simulation result

Access resource abundances in simulation result

## Usage

``` r
# S3 method for class 'mizerMRSim'
NResource(sim)

# S3 method for class 'mizerMRSim'
finalNResource(sim)
```

## Arguments

- sim:

  A MizerSim object

## Value

For `NResource()`: An array (time x resource x size) holding the
resource number densities at all saved timesteps of the simulation.

For
[`finalNResource()`](https://sizespectrum.org/mizer/reference/finalN.html):
An array (resource x size) holding the resource number densities at the
end of the simulation
