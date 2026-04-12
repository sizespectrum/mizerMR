#' Plot the mortality applied on the resource spectrum(s)
#'
#' @param object An object of class \linkS4class{MizerSim} or
#'   \linkS4class{MizerParams}.
#' @param proportion A boolean value that determines whether values should be
#' displayed as proportions from 0 to 1 or with their actual values. Default is TRUE.
#' @param return_data A boolean value that determines whether the formatted data
#' used for the plot is returned instead of the plot itself. Default value is FALSE
#' @param ... Other arguments (currently unused)
#' @return A ggplot2 object, unless `return_data = TRUE`, in which case a data
#'   frame with the four variables 'w', 'value', 'Predator', 'Resource' is returned.
#' @export
#' @family plotting functions
#' @seealso [plotting_functions]
#' @examples
#' \donttest{
#' plotResourcePred(NS_params)
#'
#' # Returning the data frame
#' fr <- plotResourcePred(NS_params, return_data = TRUE)
#' str(fr)
#' }
plotResourcePred <- function(object, proportion = TRUE, return_data = FALSE) {
    if (is(object, "MizerSim")) {
        params <- setInitialValues(object@params, object)
        if (is(object, "MRMizerSim")) params <- new("MRMizerParams", params)
    } else if (is(object, "MizerParams")) {
        params <- validParams(object)
    }
    UseMethod("plotResourcePred", params)
}

#' @rdname plotResourcePred
#' @export
plotResourcePred.MRMizerParams <- function(object,
                                            proportion = TRUE,
                                            return_data = FALSE, ...) {
    params <- object
    SpIdx <- factor(params@species_params$species,
                    levels = params@species_params$species)
    rp <- resource_params(params)
    cap <- resource_capacity(params)
    inter <- resource_interaction(params)

    plot_dat <- NULL
    for (iRes in seq_len(nrow(rp))) {
        select <- cap[iRes, ] > 0
        pred_rate <- inter[, iRes] * getPredRate(params)[, select]
        total <- colSums(pred_rate)

        ylab <- "Death rate [1/year]"
        if (proportion) {
            pred_rate <- pred_rate / rep(total, each = nrow(pred_rate))
            ylab <- "Proportion of predation"
        }
        plot_dat <- rbind(plot_dat,
                          data.frame(
                              w        = rep(params@w_full[select],
                                             each = nrow(pred_rate)),
                              value    = c(pred_rate),
                              Predator = SpIdx,
                              Resource = rp$resource[iRes]
                          ))
    }
    if (return_data) return(plot_dat)
    plotDataFrame(plot_dat, params, style = "area", xtrans = "log10",
                  xlab = "Resource size [g]", ylab = ylab,
                  wrap_var = "Resource", wrap_scale = "free")
}

#' @rdname plotResourcePred
#' @export
plotResourcePred.MizerParams <- function(object,
                                          proportion = TRUE,
                                          return_data = FALSE, ...) {
    params <- object
    SpIdx <- factor(params@species_params$species,
                    levels = params@species_params$species)
    select <- params@cc_pp > 0
    pred_rate <- params@species_params$interaction_resource *
        getPredRate(params)[, select]
    total <- colSums(pred_rate)
    ylab <- "Death rate [1/year]"
    if (proportion) {
        pred_rate <- pred_rate / rep(total, each = nrow(pred_rate))
        ylab <- "Proportion of predation"
    }
    plot_dat <- data.frame(
        w        = rep(params@w_full[select], each = nrow(pred_rate)),
        value    = c(pred_rate),
        Predator = SpIdx
    )
    if (return_data) return(plot_dat)
    plotDataFrame(plot_dat, params, style = "area", xtrans = "log10",
                  xlab = "Resource size [g]", ylab = ylab)
}

#' @rdname plotResourcePred
#' @export
plotlyResourcePred <- function(object, proportion = TRUE, ...) {
    argg <- c(as.list(environment()), list(...))
    ggplotly(do.call("plotResourcePred", argg),
             tooltip = c("value", "Predator", "w"))
}


#' Plot the proportion of the resource spectrum(s) compared to
#' their carrying capacity
#'
#' @inheritParams plotResourcePred
#' @return A ggplot2 object, unless `return_data = TRUE`, in which case a data
#'   frame with the three variables 'w', 'value', 'Resource' is returned.
#' @export
#' @family plotting functions
#' @seealso [plotting_functions]
#' @examples
#' \donttest{
#' plotResourceLevel(NS_params)
#'
#' # Returning the data frame
#' fr <- plotResourceLevel(NS_params, return_data = TRUE)
#' str(fr)
#' }
plotResourceLevel <- function(object, return_data = FALSE) {
    if (is(object, "MizerSim")) {
        params <- setInitialValues(object@params, object)
        if (is(object, "MRMizerSim")) params <- new("MRMizerParams", params)
    } else if (is(object, "MizerParams")) {
        params <- validParams(object)
    }
    UseMethod("plotResourceLevel", params)
}

#' @rdname plotResourceLevel
#' @export
plotResourceLevel.MRMizerParams <- function(object, return_data = FALSE, ...) {
    params <- object
    rp  <- resource_params(params)
    cap <- resource_capacity(params)
    n   <- initialNResource(params)

    plot_dat <- NULL
    for (iRes in seq_len(nrow(rp))) {
        select <- cap[iRes, ] > 0
        plot_dat <- rbind(plot_dat,
                          data.frame(
                              w        = params@w_full[select],
                              value    = n[iRes, select] / cap[iRes, select],
                              Resource = rp$resource[iRes]
                          ))
    }
    if (return_data) return(plot_dat)
    plotDataFrame(plot_dat, params, xtrans = "log10",
                  xlab = "Resource size [g]",
                  ylab = "Proportion of carrying capacity")
}

#' @rdname plotResourceLevel
#' @export
plotResourceLevel.MizerParams <- function(object, return_data = FALSE, ...) {
    params <- object
    select <- params@cc_pp > 0
    plot_dat <- data.frame(
        w        = params@w_full[select],
        value    = params@initial_n_pp[select] / params@cc_pp[select],
        Resource = "Resource"
    )
    if (return_data) return(plot_dat)
    plotDataFrame(plot_dat, params, xtrans = "log10",
                  xlab = "Resource size [g]",
                  ylab = "Proportion of carrying capacity")
}
