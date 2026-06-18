#' Access resource abundances in simulation result
#'
#' @param sim A MizerSim object
#' @return For `NResource()`: An array (time x resource x size) holding the
#'   resource number densities at all saved timesteps of the simulation.
#' @export
#' @name NResource
NResource.mizerMRSim <- function(sim) {
    NextMethod()
    n_res <- aperm(simplify2array(NOther(sim)[, "MR"]), c(3, 1, 2))
    dimnames(n_res)[[1]] <- dimnames(NOther(sim))[[1]]
    names(dimnames(n_res))[[1]] <- names(dimnames(NOther(sim)))[[1]]
    MRArrayTimeByResourceBySize(n_res, value_name = "Number density",
                                units = "1/g", params = sim@params)
}

#' @rdname NResource
#' @return For `finalNResource()`: An array (resource x size) holding the
#'   resource number densities at the end of the simulation
#' @export
finalNResource.mizerMRSim <- function(sim) {
    NextMethod()
    MRArrayResourceBySize(NOther(sim)[[idxFinalT(sim), "MR"]],
                          value_name = "Number density", units = "1/g",
                          params = sim@params)
}
