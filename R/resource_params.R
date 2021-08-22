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
#' @export
#' @seealso [validResourceParams()]
#' @family functions for setting parameters
resource_params <- function(params) {
    params@resource_params
}

#' @rdname resource_params
#' @param value A data frame with the resource parameters
#' @export
`resource_params<-` <- function(params, value) {
    value <- validResourceParams(value)
    params@resource_params <- value
    setMultipleResources(params)
}

#' Validate resource parameter data frame
#'
#' Check validity of resource parameters and set defaults for missing but
#' required parameters
#'
#' @param resource_params The user-supplied resource parameter data frame
#' @param min_w The smallest allowed resource size
#' @return A valid resource parameter data frame
#'
#' This function throws an error if
#' * the `resource` column does not exist or contains duplicates
#' * `w_min` is not smaller than `w_max`
#' * `w_min` is smaller than `min_w`
#' * any parameter is negative
# TODO: Implement:
# #' * `resource_dynamics` is not a valid resource dynamics function
#'
#' It sets default values if any of the following are missing or NA
#' * `kappa` is set to `0.1`
#' * `lambda` is set to `2.05`
#' * `r_pp` is set to `4`
#' * `w_min` is set to `min_w`
#' * `w_max` is set to `10`
#' * `resource_dynamics` is set to `semichemostat`
#'
#' If `resource_params` was provided as a tibble it is converted back to an
#' ordinary data frame.
#'
#' @concept helper
#' @export
validResourceParams <- function(resource_params, min_w) {
    assert_that(is.data.frame(resource_params))
    # Convert a tibble back to an ordinary data frame
    rp <- as.data.frame(resource_params,
                        stringsAsFactors = FALSE) # for old versions of R

    # check resource names ----
    if (!("resource" %in% colnames(rp))) {
        stop("The resource params dataframe needs a column 'resource' with the resource names")
    }
    resource_names <- as.character(rp$resource)
    rp$resource <- resource_names
    row.names(rp) <- resource_names
    no_r <- nrow(rp)
    if (length(unique(resource_names)) != no_r) {
        stop("The resource parameter data frame has multiple rows for the same resource")
    }

    # Set defaults ----
    rp <- set_resource_param_default("kappa", 0.1)
    rp <- set_resource_param_default("lambda", 2.05)
    rp <- set_resource_param_default("r_pp", 4)
    rp <- set_resource_param_default("w_min", min_w)
    rp <- set_resource_param_default("w_max", 10)
    rp <- set_resource_param_default("resource_dynamics", "semichemostat")

    # Check values ----
    wrong <- rp$w_min >= rp$w_max
    if (any(wrong)) {
        stop("w_min is not smaller than w_max for ",
             paste(rp$resource[wrong], collapse = ", "))
    }
    wrong <- rp$w_min < min_w
    if (any(wrong)) {
        stop("w_min is smaller than the smallest allowed size for ",
             paste(rp$resource[wrong], collapse = ", "))
    }
    wrong <- rp$kappa < 0 | rp$lambda < 0 | rp$r_pp < 0 | rp$w_min < 0 |
        rp$w_max < 0
    if (any(wrong)) {
        stop("The following resources have some negative parameters: ",
             paste(rp$resource[wrong], collapse = ", "))
    }

}


#' Set a resource parameter to a default value
#'
#' If the resource parameter does not yet exist in the resource parameter data
#' frame, then create it and fill it with the default. Otherwise use the default
#' only to fill in any NAs. Optionally gives a message if the parameter
#' did not already exist.
#' @param object Either a MizerParams object or a resource parameter data frame
#' @param parname A string with the name of the resource parameter to set
#' @param default A single default value or a vector with one default value for
#'   each resource
#' @param message A string with a message to be issued when the parameter did
#'   not already exist
#' @return The `object` with an updated column in the resource params data frame.
#' @export
#' @concept helper
set_resource_param_default <- function(object, parname, default,
                                      message = NULL) {
    # This is a copy of `set_species_params_default()` in core mizer, just with
    # species replaced by resource.
    if (is(object, "MizerParams")) {
        resource_params <- object@resource_params
    } else {
        resource_params <- object
    }
    assert_that(is.data.frame(resource_params))
    assert_that(is.string(parname))
    no_r <- nrow(resource_params)
    if (length(default) == 1) {
        default <- rep(default, no_r)
    }
    assert_that(length(default) == no_r)
    if (!(parname %in% colnames(resource_params))) {
        if (!missing(message)) {
            signal(message,
                   class = "info_about_default", var = parname, level = 3)
        }
        resource_params <- data.frame(resource_params, default,
                                     stringsAsFactors = FALSE)
        colnames(resource_params)[[ncol(resource_params)]] <- parname
    } else {
        # We do not like factors
        if (is.factor(resource_params[[parname]])) {
            resource_params[[parname]] <- as.character(resource_params[[parname]])
        }
        missing <- is.na(resource_params[[parname]])
        if (any(missing)) {
            resource_params[missing, parname] <- default[missing]
        }
    }
    if  (is(object, "MizerParams")) {
        object@resource_params <- resource_params
        return(object)
    } else {
        return(resource_params)
    }
}
