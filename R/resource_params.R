#' Resource parameters
#'
#' The resource parameter data frame has one row for each resource. The
#' variables are:
#' * `resource` Name you want to use for the resource
#' * `kappa`    Coefficient in the carrying capacity power law
#' * `lambda`   Exponent of the carrying capacity power law
#' * `r_pp`     Coefficient in the allometric replenishment rate
#' * `w_min`    Smallest size of the resource
#' * `w_max`    Largest size of the resource
#' * `resource_dynamics` Name of the resource dynamics function
#'
#' @param params A MizerParams object
#' @return Resource parameter data frame
#' @export
resource_params <- function(params) {

}

#' @rdname resource_params
#' @param params A MizerParams object
#' @param value Resource parameter data frame
#' @export
`resource_params<-` <- function(params, value) {

}
