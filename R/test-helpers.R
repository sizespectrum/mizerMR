# Often I need to test that a MizerParams or MizerSim object has not
# changed except for the time_modified and n_pp slots
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
