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
#' The prey index \eqn{j} runs over all species and the resources. It also runs
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
#' @param n_other A list of abundances for other dynamical components of the
#'   ecosystem
#' @param proportion If TRUE (default) the function returns the diet as a
#'   proportion of the total consumption rate. If FALSE it returns the
#'   consumption rate in grams per year.
#'
#' @return An array (predator species  x predator size x
#'   (prey species + resources) )
#' @export
#' @family summary functions
#' @concept summary_function
#' @seealso [plotDietMR()]



getDietMR <- function (params, n = initialN(params),
                       n_other = initialNOther(params), proportion = TRUE)
{
    params <- validParams(params)
    species <- params@species_params$species
    no_sp <- length(species)
    no_w <- length(params@w)
    no_w_full <- length(params@w_full)
    no_other <- dim(n_other$MR)[1]
    other_names <- rownames(n_other$MR)

    assert_that(identical(dim(n), c(no_sp, no_w)), dim(n_other$MR)[2] == no_w_full)
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
            diet[, , no_sp + iRes] <- rowSums(sweep(params@pred_kernel, 3,
                                                    params@dw_full * params@w_full * n_other$MR[iRes,], "*"), dims = 2)
        }
    }
    else {
        prey <- matrix(0, nrow = no_sp + no_other, ncol = no_w_full)
        prey[1:no_sp, idx_sp] <- sweep(n, 2, params@w * params@dw, "*")
        prey[(no_sp + 1):(no_sp + no_other), ] <- sweep(n_other$MR,2,
                                                        params@w_full * params@dw_full, "*")
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

    inter <- cbind(params@interaction,resource_interaction(params))
    diet[, , 1:(no_sp+no_other)] <- sweep(sweep(diet[, , 1:(no_sp+no_other), drop = FALSE],
                                                c(1, 3), inter, "*"), c(1, 2), params@search_vol, "*")

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
#' predator's size. These proportions are obtained with `getDietMR()`.
#'
#' Prey species that contribute less than 1 permille to the diet are suppressed
#' in the plot.
#'
#' @inheritParams plotSpectra
#' @param species The name of the predator species for which to plot the diet.
#'
#' @return A ggplot2 object, unless `return_data = TRUE`, in which case a data
#'   frame with the three variables 'w', 'Proportion', 'Prey' is returned.
#' @export
#' @seealso [getDietMR()]
#' @family plotting functions

plotDietMR <- function (object, species = NULL, time_range, wlim = c(1, NA), return_data = FALSE)
{
    if (is(object, "MizerSim")) {
        if (missing(time_range)) time_range <- max(as.numeric(dimnames(object@n)$time))
        time_elements <- get_time_elements(object, time_range)
        n <- apply(object@n[time_elements, , , drop = FALSE], 2:3, mean)
        n_other <- list()
        n_other$MR <- apply(simplify2array(object@n_other[time_elements, ]), 1:2, mean)
        params <- object@params
        diet <- getDietMR(params, n = n, n_other = n_other)
    } else if (is(object, "MizerParams")) {
        params <- object
        diet <- getDietMR(params)
    } else {
        stop("The first argument must be either a MizerSim or a MizerParams object")
    }

    plot_dat <- melt(diet)
    plot_dat <- plot_dat[plot_dat$value > 0.001, ]
    colnames(plot_dat) <- c("Predator", "size", "Prey", "Proportion")
    plot_dat$Prey <- factor(plot_dat$Prey, levels = rev(unique(plot_dat$Prey)))

    if (is.null(species))
        p <- ggplot(plot_dat) + facet_wrap(. ~ Predator, scales = "free")
    else p <- ggplot(dplyr::filter(plot_dat, Predator == species))

    legend_levels <- intersect(names(params@linecolour), plot_dat$Prey)
    p <- p + geom_area(aes(x = size, y = Proportion, fill = Prey)) +
        scale_x_continuous(limits = wlim, name = "Size [g]", trans = "log10") +
        scale_fill_manual(values = params@linecolour[legend_levels]) +
        theme(legend.position = "right", legend.key = element_rect(fill = "black"),
              panel.background = element_blank(), panel.grid.minor = element_line(color = "gray"),
              strip.background = element_blank())

    if (return_data)
        return(plot_dat)
    else return(p)
}
