#' Expect a mizer object to be unchanged
#'
#' Compares two mizer objects after normalising fields that are expected to
#' change during setup or projection.
#'
#' @param object A \linkS4class{MizerParams} or \linkS4class{MizerSim} object.
#' @param expected The expected object.
#' @return The expectation result from [testthat::expect_equal()].
#' @keywords internal
expect_unchanged <- function(object, expected) {
    if (is(object, "MizerParams")) {
        object@time_modified <- expected@time_modified
        object@initial_n_pp <- expected@initial_n_pp
    }
    if (is(object, "MizerSim")) {
        object@params@time_modified <- expected@params@time_modified
        object@params@initial_n_pp <- expected@params@initial_n_pp
        object@n_pp <- expected@n_pp
    }

    expect_equal(object, expected)
}
