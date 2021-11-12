#' Get diet of predator at size, resolved by prey species
#'
#' Calculates the rate at which a predator of a particular species and size
#' consumes biomass of each prey species and resource.
#' The diet has units of grams/year.
#'
#' Returns the rates \eqn{D_{ij}(w)} at which a predator of species \eqn{i}
#' and size \eqn{w} consumes biomass from prey species \eqn{j}. This is
#' calculated from the predation kernel \eqn{\phi_i(w, w_p)},
#' the search volume \eqn{\gamma_i(w)}, the feeding level \eqn{f_i(w)}, the
#' species interaction matrix \eqn{\theta_{ij}} and the prey abundance density
#' \eqn{N_j(w_p)}:
#' \deqn{
#' D_{ij}(w, w_p) = (1-f_i(w)) \gamma_i(w) \theta_{ij}
#' \int N_j(w_p) \phi_i(w, w_p) w_p dw_p.
#' }
#' The prey index \eqn{j} runs over all species and the resource. It also runs
#' over any extra ecosystem components in your model for which you have
#' defined an encounter rate function. This encounter rate is multiplied by
#' \eqn{1-f_i(w)} to give the rate of consumption of biomass from these extra
#' components.
#'
#' This function performs the same integration as
#' [getEncounter()] but does not aggregate over prey species, and
#' multiplies by \eqn{1-f_i(w)} to get the consumed biomass rather than the
#' available biomass. Outside the range of sizes for a predator species the
#' returned rate is zero.
#'
#' @param params A \linkS4class{MizerParams} object
#' @param n A matrix of species abundances (species x size).
#' @param n_pp A vector of the resource abundance by size
#' @param n_other A list of abundances for other dynamical components of the
#' @param proportion If TRUE (default) the function returns the diet as a
#'   proportion of the total consumption rate. If FALSE it returns the
#'   consumption rate in grams per year.
#'
#' @return An array (predator species  x predator size x
#'   (prey species + resource + other components) )
#' @export
#' @family summary functions
#' @concept summary_function
#' @seealso [plotDiet()]
#' @examples
#' diet <- getDiet(NS_params)
#' str(diet)
getDiet <- function (params, n = initialN(params), n_pp = initialNResource(params),
                     n_other = initialNOther(params), proportion = TRUE)
{
    params <- validParams(params)
    species <- params@species_params$species
    no_sp <- length(species)
    no_w <- length(params@w)
    no_w_full <- length(params@w_full)

    if(is.null(getComponent(params, "MR"))) # in this case there are no additional background so should have npp only
    {# using n_other as place holder for n_pp
        no_other = 1
        other_names = "Resource"
        no_w_other <- length(n_pp)
        n_other_inter <- matrix(params@species_params$interaction_resource, ncol = 1,
                                dimnames = list("sp" = params@species_params$species, "resource" = other_names))
        names(n_other_inter) <- other_names
    } else {
        no_other <- dim(n_other$MR)[1]
        other_names <- rownames(n_other$MR)
        no_w_other <- dim(n_other$MR)[2]
        n_other_inter <- resource_interaction(params)
    }

    assert_that(identical(dim(n), c(no_sp, no_w)), no_w_other == no_w_full)

    diet <- array(0, dim = c(no_sp, no_w, no_sp + no_other),
                  dimnames = list(predator = species, w = dimnames(params@initial_n)$w,
                                  prey = c(as.character(species), other_names)))
    idx_sp <- (no_w_full - no_w + 1):no_w_full

    if(!is.null(comment(params@pred_kernel))) {
        ae <- matrix(params@pred_kernel[, , idx_sp, drop = FALSE],
                     ncol = no_w) %*% t(sweep(n, 2, params@w * params@dw,
                                              "*"))
        diet[, , 1:no_sp] <- ae

        for(iRes in 1:no_other)
        {
            if(is.null(getComponent(params, "MR")))
                diet[, , no_sp + iRes] <-
                    rowSums(sweep(params@pred_kernel, 3, params@dw_full * params@w_full * n_pp, "*")
                            , dims = 2) else
                                diet[, , no_sp + iRes] <-
                                    rowSums(sweep(params@pred_kernel, 3, params@dw_full * params@w_full * n_other$MR[iRes,], "*")
                                            , dims = 2)
        }
    } else {
        prey <- matrix(0, nrow = no_sp + no_other, ncol = no_w_full)
        prey[1:no_sp, idx_sp] <- sweep(n, 2, params@w * params@dw, "*")

        if(is.null(getComponent(params, "MR")))
            prey[no_sp + 1, ] <- n_pp * params@w_full * params@dw_full else
                prey[(no_sp + 1):(no_sp + no_other), ] <- sweep(n_other$MR,2, params@w_full * params@dw_full, "*")

        ft <- array(rep(params@ft_pred_kernel_e, times = no_sp + no_other) *
                        rep(mvfft(t(prey)), each = no_sp), dim = c(no_sp, no_w_full, no_sp + no_other))
        ft <- matrix(aperm(ft, c(2, 1, 3)), nrow = no_w_full)
        ae <- array(Re(mvfft(ft, inverse = TRUE)/no_w_full),
                    dim = c(no_w_full, no_sp, no_sp + no_other))
        ae <- ae[idx_sp, , , drop = FALSE]
        ae <- aperm(ae, c(2, 1, 3))
        ae[ae < 1e-18] <- 0
        diet[, , 1:(no_sp + no_other)] <- ae
    }

    inter <- cbind(params@interaction,n_other_inter)
    diet[, , 1:(no_sp+no_other)] <- sweep(sweep(diet[, , 1:(no_sp+no_other), drop = FALSE],
                                                c(1, 3), inter, "*"), c(1, 2), params@search_vol, "*")
    if(is.null(getComponent(params, "MR")))
        f <- getFeedingLevel(object = params, n = n, n_pp = n_pp) else
            f <- getFeedingLevel(object = params, n = n, n_other = n_other)
    fish_mask <- n > 0
    diet <- sweep(diet, c(1, 2), (1 - f) * fish_mask, "*")
    if (proportion) {
        total <- rowSums(diet, dims = 2)
        diet <- sweep(diet, c(1, 2), total, "/")
        diet[is.nan(diet)] <- 0
    }
    return(diet)
}


#' Plot diet, resolved by prey species, as function of predator at size.
#'
#' `r lifecycle::badge("experimental")`
#' Plots the proportions with which each prey species contributes to the total
#' biomass consumed by the specified predator species, as a function of the
#' predator's size. These proportions are obtained with `getDiet()`.
#'
#' Prey species that contribute less than 1 permille to the diet are suppressed
#' in the plot.
#'
#' @param object An object of class \linkS4class{MizerSim} or
#'   \linkS4class{MizerParams}.
#' @param species The name of the predator species for which to plot the diet.
#' @param time_range The time range (either a vector of values, a vector of min
#'   and max time, or a single value) to average the abundances over. Default is
#'   the final time step. Ignored when called with a \linkS4class{MizerParams}
#'   object.
#' @param wlim A numeric vector of length two providing lower and upper limits
#'   for the w axis. Use NA to refer to the existing minimum or maximum.
#' @param return_data A boolean value that determines whether the formatted data
#' used for the plot is returned instead of the plot itself. Default value is FALSE
#' @param ... Other arguments (currently unused)
#'
#' @return A ggplot2 object, unless `return_data = TRUE`, in which case a data
#'   frame with the three variables 'w', 'Proportion', 'Prey' is returned.
#' @export
#' @seealso [getDiet()]
#' @family plotting functions

plotDiet <- function (object, species = NULL, time_range, wlim = c(1, NA), return_data = FALSE)
{
    assert_that(is.flag(return_data))
    # species <- valid_species_arg(object, species, return.logical = TRUE)


    if (is(object, "MizerSim")) {
        if (missing(time_range)) time_range <- max(as.numeric(dimnames(object@n)$time))
        time_elements <- get_time_elements(object, time_range)
        params <- validParams(object@params)
        n <- apply(object@n[time_elements, , , drop = FALSE], 2:3, mean)
        if(is.null(getComponent(params, "MR")))
        {
            n_pp <- apply(object@n_pp[time_elements, , drop = FALSE], 2, mean)
            diet <- getDiet(params, n = n, n_other = n_other)
        } else {
            n_other <- list()
            n_other$MR <- apply(simplify2array(object@n_other[time_elements, ]), 1:2, mean)
            diet <- getDiet(params, n = n, n_other = n_other)
        }
    } else if (is(object, "MizerParams")) {
        params <- validParams(object)
        diet <- getDiet(params)
    } else {
        stop("The first argument must be either a MizerSim or a MizerParams object")
    }

    SpIdx <- factor(params@species_params$species,
                    levels = params@species_params$species)
    if(is.null(species)) species <- SpIdx

    plot_dat <- melt(diet)
    plot_dat <- plot_dat[plot_dat$value > 0.001, ]
    colnames(plot_dat) <- c("Predator", "size", "Prey", "Proportion")
    plot_dat$Prey <- factor(plot_dat$Prey, levels = rev(unique(plot_dat$Prey)))
    plot_dat <- plot_dat[, c("size","Proportion","Prey","Predator")]
    plot_dat <- dplyr::filter(plot_dat, Predator %in% species)

    if (return_data)  return(plot_dat)

    mizerExperimental::plotDataFrame(plot_dat, params, style = "area", wrap_var = "Predator", xtrans = "log10",
                  xlab = "Size [g]",
                  wrap_scale = "free")
}

#' @rdname plotDiet
#' @export
plotlyDiet <- function(object,
                       species = NULL,
                       time_range,
                       wlim = c(1, NA),
                        ...) {
    argg <- c(as.list(environment()), list(...))
    ggplotly(do.call("plotDiet", argg),
             tooltip = c("size","Proportion","Prey"))
}
