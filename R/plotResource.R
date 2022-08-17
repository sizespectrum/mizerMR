#' Plot functions regarding the resources. Compatible with non mizerMR objects

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
plotResourcePred <- function(object, proportion = TRUE, return_data = FALSE)
{
    if (is(object, "MizerSim")) {
        params <- object@params
        params <- setInitialValues(params, object)
    } else if (is(object, "MizerParams")) {
        params <- validParams(object)
    }
    SpIdx <- factor(params@species_params$species,
                    levels = params@species_params$species)

    #How many backgrounds are being used? Assuming using only mizerMR for now for additional backgrounds
    if(!is.null(getComponent(params, "MR")))
    {
        plot_dat <- NULL
        for(iRes in 1:dim(params@other_params$other$MR$resource_params)[1])
        {
            select <- (params@other_params$MR$capacity[iRes,] > 0)
            pred_rate <- params@other_params$MR$interaction[,iRes] *
                getPredRate(params)[, select]
            total <- colSums(pred_rate)

            ylab <- "Death rate [1/year]"
            if (proportion) {
                pred_rate <- pred_rate / rep(total, each = dim(pred_rate)[[1]])
                ylab = "Proportion of predation"
            }
            # Make data.frame for plot
            plot_dat <- rbind(plot_dat,
                              data.frame(
                                  w = rep(params@w_full[select], each = dim(pred_rate)[[1]]),
                                  value = c(pred_rate),
                                  Predator = SpIdx,
                                  Resource = params@other_params$other$MR$resource_params$resource[iRes]
                              ))
        }
        pl <- plotDataFrame(plot_dat, params, style = "area", xtrans = "log10",
                            xlab = "Resource size [g]", ylab = ylab,
                            wrap_var = "Resource", wrap_scale = "free")
    } else {
        select <- (params@cc_pp > 0)
        pred_rate <- params@species_params$interaction_resource *
            getPredRate(params)[, select]
        total <- colSums(pred_rate)
        ylab <- "Death rate [1/year]"
        if (proportion) {
            pred_rate <- pred_rate / rep(total, each = dim(pred_rate)[[1]])
            ylab = "Proportion of predation"
        }
        # Make data.frame for plot
        plot_dat <- data.frame(
            w = rep(params@w_full[select], each = dim(pred_rate)[[1]]),
            value = c(pred_rate),
            Predator = SpIdx
        )
        pl <- plotDataFrame(plot_dat, params, style = "area", xtrans = "log10",
                            xlab = "Resource size [g]", ylab = ylab)
    }

    if(return_data) return(plot_dat) else return(pl)
}

#' @rdname plotResourcePred
#' @export
plotlyResourcePred <- function(object,
                               proportion = TRUE,
                               ...) {
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
plotResourceLevel <- function(object, return_data = FALSE)
{
    if (is(object, "MizerSim")) {
        params <- object@params
        params <- setInitialValues(params, object)
    } else if (is(object, "MizerParams")) {
        params <- validParams(object)
    }
    #How many backgrounds are being used? Assuming using only mizerMR for now for additional backgrounds
    if(!is.null(getComponent(params, "MR")))
    {
        plot_dat <- NULL
        for(iRes in 1:dim(params@other_params$other$MR$resource_params)[1])
        {
            select <- (params@other_params$MR$capacity[iRes,] > 0)
            plot_dat <- rbind(plot_dat,
                              data.frame(
                                  w = params@w_full[select],
                                  value = params@initial_n_other$MR[iRes,select] / params@other_params$MR$capacity[iRes,select],
                                  Resource = params@other_params$other$MR$resource_params$resource[iRes])
            )
        }
    } else {
        select <- (params@cc_pp > 0)
        plot_dat <- data.frame(
            w = params@w_full[select],
            value = params@initial_n_pp[select] / params@cc_pp[select],
            Resource = "Resource" # 3rd var for plotDataFrame()
        )
    }

    if(return_data) return(plot_dat)

    plotDataFrame(plot_dat, params, xtrans = "log10",
                  xlab = "Resource size [g]", ylab = "Proportion of carrying capacity")
}
