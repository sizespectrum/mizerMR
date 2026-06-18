# mizerMR marker classes

S4 marker subclasses of
[MizerParams](https://sizespectrum.org/mizer/reference/MizerParams.html)
and [MizerSim](https://sizespectrum.org/mizer/reference/MizerSim.html)
that enable S3 dispatch for MR-specific methods.

## Details

Objects of class `mizerMR` are created by
[`setMultipleResources()`](https://sizespectrum.org/mizerMR/reference/setMultipleResources.md).
Objects of class `mizerMRSim` are returned automatically by
[`project()`](https://sizespectrum.org/mizer/reference/project.html)
when called on a `mizerMR` params object.
