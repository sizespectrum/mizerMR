#' Access resource abundances in simulation result
#' @export
NResource <- function(sim) {
    if (!"MR" %in% names(sim@params@initial_n_other)) {
        mizer::NResource(sim)
    }
    n_res <- aperm(simplify2array(sim@n_other[, "MR"]), c(3, 1, 2))
    dimnames(n_res)[[1]] <- dimnames(sim@n_other)[[1]]
    names(dimnames(n_res))[[1]] <- names(dimnames(sim@n_other))[[1]]
    n_res
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
