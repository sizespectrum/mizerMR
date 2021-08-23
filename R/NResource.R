#' Access resource abundances in simulation result
#' @export
NResource <- function(sim) {
    if (!"MR" %in% names(sim@params@initial_n_other)) {
        mizer::NResource(sim)
    }
    aperm(simplify2array(sim@n_other[, "MR"]), c(3, 1, 2))
}

#' @rdname NResource
#' @return For `finalNResource()`: An array (resource x size) holding the
#'   resource number densities at the end of the simulation
#' @export
finalNResource <- function(sim) {
    if (!"MR" %in% names(sim@params@initial_n_other)) {
        mizer::FinalNResource(sim)
    }
    sim@n_other[[dim(sim@n_other)[[1]], "MR"]]
}
