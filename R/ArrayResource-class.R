# MRArrayResourceBySize and MRArrayTimeByResourceBySize S3 classes for the
# multiple-resource size spectra of mizerMR.
#
# These mirror mizer's ArrayResourceBySize / ArrayTimeByResourceBySize classes
# but carry an extra `resource` dimension, so that resource quantities for
# several resources can be printed, summarised, plotted and turned into a data
# frame. They are the multi-resource analogues of mizer's ArraySpeciesBySize /
# ArrayTimeBySpeciesBySize classes, with each resource playing the role that a
# species plays there.

# MRArrayResourceBySize --------------------------------------------------------

#' S3 class for multiple-resource size spectra
#'
#' Several mizerMR functions return a `resource x size` matrix holding a
#' resource-related quantity such as the resource number density or the
#' resource mortality. The `MRArrayResourceBySize` class wraps such a matrix to
#' provide convenient `print()`, `summary()`, `plot()`, and `as.data.frame()`
#' methods. It is the multiple-resource analogue of mizer's
#' `ArrayResourceBySize` class.
#'
#' An `MRArrayResourceBySize` object behaves just like a regular matrix for
#' arithmetic and subsetting. It carries `value_name`, `units` and `params`
#' attributes, mirroring the mizer array classes.
#'
#' @param x A matrix (resource x size), with resource names as row names.
#' @param value_name A string giving the human-readable name for the value.
#' @param units A string giving the units (e.g. "1/year").
#' @param params A MizerParams object. Used for the resource colours and the
#'   size grid in the `plot()` method.
#'
#' @return An `MRArrayResourceBySize` object (inherits from `matrix`/`array`).
#' @seealso [mizer::ArrayResourceBySize()]
#' @export
MRArrayResourceBySize <- function(x, value_name = NULL, units = NULL,
                                  params = NULL) {
    if (!is.matrix(x)) {
        stop("`x` must be a matrix.")
    }
    structure(x,
              class = c("MRArrayResourceBySize", "matrix", "array"),
              value_name = value_name,
              units = units,
              params = params)
}

#' Test if an object is an MRArrayResourceBySize
#'
#' @param x An object to test.
#' @return `TRUE` if `x` is an `MRArrayResourceBySize` object, `FALSE` otherwise.
#' @export
is.MRArrayResourceBySize <- function(x) {
    inherits(x, "MRArrayResourceBySize")
}

#' @export
print.MRArrayResourceBySize <- function(x, ...) {
    value_name <- attr(x, "value_name") %||% "MRArrayResourceBySize"
    units_str <- attr(x, "units")
    dims <- dim(x)
    header <- paste0(value_name, " (", dims[1], " resources x ", dims[2],
                     " sizes)")
    if (!is.null(units_str) && nzchar(units_str)) {
        header <- paste0(header, " [", units_str, "]")
    }
    cat(header, "\n")
    res_names <- rownames(x)
    if (!is.null(res_names)) {
        vals <- apply(unclass(x), 1, function(row) {
            row <- row[is.finite(row)]
            if (length(row) == 0) return("all NA/Inf")
            paste0("min=", signif(min(row), 3),
                   " mean=", signif(mean(row), 3),
                   " max=", signif(max(row), 3))
        })
        for (i in seq_along(res_names)) {
            cat("  ", res_names[i], ": ", vals[i], "\n", sep = "")
        }
    }
    invisible(x)
}

#' @export
summary.MRArrayResourceBySize <- function(object, ...) {
    value_name <- attr(object, "value_name") %||% "MRArrayResourceBySize"
    units_str <- attr(object, "units")
    mat <- unclass(object)
    df <- data.frame(
        Resource = rownames(object),
        Min = apply(mat, 1, min, na.rm = TRUE),
        Mean = apply(mat, 1, mean, na.rm = TRUE),
        Max = apply(mat, 1, max, na.rm = TRUE),
        row.names = NULL,
        stringsAsFactors = FALSE
    )
    result <- list(value_name = value_name, units = units_str,
                   dims = dim(object), per_resource = df)
    class(result) <- "summary.MRArrayResourceBySize"
    result
}

#' @export
print.summary.MRArrayResourceBySize <- function(x, ...) {
    header <- x$value_name
    if (!is.null(x$units) && nzchar(x$units)) {
        header <- paste0(header, " [", x$units, "]")
    }
    cat(header, "\n")
    cat(x$dims[1], "resources x", x$dims[2], "sizes\n\n")
    print(x$per_resource, row.names = FALSE)
    invisible(x)
}

#' Plot mizerMR resource arrays
#'
#' `plot()` creates a ggplot2 figure with one line for each resource, showing
#' the values against size. It is the multiple-resource analogue of the
#' `plot()` method for mizer's `ArrayResourceBySize` objects and reuses the same
#' internal mizer plotting machinery, so resources are drawn in the colours
#' registered in the `params` object.
#'
#' @param x An `MRArrayResourceBySize` or `MRArrayTimeByResourceBySize` object.
#' @param resources Optional vector of resource names (or indices) to restrict
#'   the plot to.
#' @param return_data Logical. If TRUE, return the plotting data frame instead
#'   of the plot.
#' @param log_x,log_y Logical flags for logarithmic axes.
#' @param log Alternative way to specify the log axes, see
#'   [mizer::parsePlotLog()].
#' @param wlim,ylim Length-2 numeric vectors giving the weight and value limits.
#' @param y_ticks Approximate number of ticks on the y-axis.
#' @param time For `MRArrayTimeByResourceBySize`, the time at which to plot the
#'   spectrum. Defaults to the final time step.
#' @param ... Unused.
#' @return A ggplot object, or a data frame if `return_data = TRUE`.
#' @export
plot.MRArrayResourceBySize <- function(x, resources = NULL, return_data = FALSE,
                                       log_x = TRUE, log_y = TRUE, log = NULL,
                                       wlim = c(NA, NA), ylim = c(NA, NA),
                                       y_ticks = 6, ...) {
    log_axes <- mizer::parsePlotLog(log, log_x = log_x, log_y = log_y)
    value_name <- attr(x, "value_name") %||% "value"
    units_str <- attr(x, "units")
    params <- attr(x, "params")

    plot_dat <- prepare_MRArrayResourceBySize_plot_data(
        x, resources = resources, wlim = wlim)
    if (return_data) return(plot_dat)

    y_label <- value_name
    if (!is.null(units_str) && nzchar(units_str)) {
        y_label <- paste0(value_name, " [", units_str, "]")
    }

    mizer::plotDataFrame(
        plot_dat, params, xlab = "Weight (g)", ylab = y_label,
        xtrans = if (log_axes$log_x) "log10" else "identity",
        ytrans = if (log_axes$log_y) "log10" else "identity",
        xlim = wlim, ylim = ylim, y_ticks = y_ticks, legend_var = "Legend")
}

prepare_MRArrayResourceBySize_plot_data <- function(x, resources = NULL,
                                                    wlim = c(NA, NA)) {
    mat <- unclass(x)
    if (!is.null(resources)) {
        mat <- mat[resources, , drop = FALSE]
    }
    w <- get_MRArrayResourceBySize_w(x)
    res_names <- rownames(mat)
    plot_dat <- data.frame(
        w = rep(w, each = nrow(mat)),
        value = as.numeric(mat),
        Spectra = rep(res_names, times = ncol(mat)),
        Legend = rep(res_names, times = ncol(mat)),
        stringsAsFactors = FALSE
    )
    mizer::apply_wlim(plot_dat, wlim)
}

# Internal helper: the size grid for an MRArrayResourceBySize object.
get_MRArrayResourceBySize_w <- function(x) {
    params <- attr(x, "params")
    if (!is.null(params) && ncol(x) == length(params@w_full)) {
        return(params@w_full)
    }
    w <- as.numeric(colnames(x))
    if (any(is.na(w))) w <- seq_len(ncol(x))
    w
}

#' @rdname plot.MRArrayResourceBySize
#' @export
plotHover.MRArrayResourceBySize <- function(x, ...) {
    mizer::plotHover(plot(x, ...), ...)
}

#' @export
as.data.frame.MRArrayResourceBySize <- function(x, row.names = NULL,
                                                optional = FALSE, ...) {
    w <- get_MRArrayResourceBySize_w(x)
    mat <- unclass(x)
    data.frame(
        w = rep(w, each = nrow(mat)),
        value = as.numeric(mat),
        resource = rep(rownames(mat), times = ncol(mat)),
        row.names = row.names,
        check.names = !optional,
        stringsAsFactors = FALSE
    )
}

#' @export
`[.MRArrayResourceBySize` <- function(x, i, j, ..., drop = TRUE) {
    result <- NextMethod()
    if (is.matrix(result) && length(dim(result)) == 2) {
        attr(result, "value_name") <- attr(x, "value_name")
        attr(result, "units") <- attr(x, "units")
        attr(result, "params") <- attr(x, "params")
        class(result) <- c("MRArrayResourceBySize", "matrix", "array")
    }
    result
}

#' @export
Ops.MRArrayResourceBySize <- function(e1, e2) {
    if (is.MRArrayResourceBySize(e1)) e1 <- unclass_mr_resource(e1)
    if (!missing(e2) && is.MRArrayResourceBySize(e2)) {
        e2 <- unclass_mr_resource(e2)
    }
    op <- match.fun(.Generic)
    if (missing(e2)) op(e1) else op(e1, e2)
}

unclass_mr_resource <- function(x) {
    x <- unclass(x)
    attr(x, "value_name") <- NULL
    attr(x, "units") <- NULL
    attr(x, "params") <- NULL
    x
}

#' @export
str.MRArrayResourceBySize <- function(object, ...) {
    params <- attr(object, "params")
    attr(object, "params") <- NULL
    class(object) <- c("matrix", "array")
    out <- utils::capture.output(utils::str(object, ...))
    out[1] <- paste0(" 'MRArrayResourceBySize' ", sub("^ ", "", out[1]))
    cat(paste0(out, collapse = "\n"), "\n", sep = "")
    if (!is.null(params)) {
        cat(" - attr(*, \"params\")=Formal class 'MizerParams' [package \"mizer\"] with ",
            length(methods::slotNames(params)), " slots\n", sep = "")
    }
    invisible(NULL)
}


# MRArrayTimeByResourceBySize --------------------------------------------------

#' S3 class for time x resource x size arrays
#'
#' [NResource()] for a mizerMR simulation returns a three-dimensional array
#' (time x resource x size) holding the resource number densities through time.
#' The `MRArrayTimeByResourceBySize` class wraps this array to provide
#' convenient `print()`, `summary()`, `plot()`, and `as.data.frame()` methods.
#' It is the multiple-resource analogue of mizer's `ArrayTimeByResourceBySize`
#' class.
#'
#' @param x A three-dimensional array (time x resource x size).
#' @param value_name A string giving the human-readable name for the value.
#' @param units A string giving the units (e.g. "1/g").
#' @param params A MizerParams object. Used for the resource colours and the
#'   size grid in the `plot()` method.
#'
#' @return An `MRArrayTimeByResourceBySize` object (inherits from `array`).
#' @seealso [mizer::ArrayTimeByResourceBySize()]
#' @export
MRArrayTimeByResourceBySize <- function(x, value_name = NULL, units = NULL,
                                        params = NULL) {
    if (!is.array(x) || length(dim(x)) != 3) {
        stop("`x` must be a three-dimensional array.")
    }
    structure(x,
              class = c("MRArrayTimeByResourceBySize", "array"),
              value_name = value_name,
              units = units,
              params = params)
}

#' Test if an object is an MRArrayTimeByResourceBySize
#'
#' @param x An object to test.
#' @return `TRUE` if `x` is an `MRArrayTimeByResourceBySize` object, `FALSE`
#'   otherwise.
#' @export
is.MRArrayTimeByResourceBySize <- function(x) {
    inherits(x, "MRArrayTimeByResourceBySize")
}

#' @export
print.MRArrayTimeByResourceBySize <- function(x, ...) {
    value_name <- attr(x, "value_name") %||% "MRArrayTimeByResourceBySize"
    units_str <- attr(x, "units")
    dims <- dim(x)
    header <- paste0(value_name, " (", dims[1], " times x ", dims[2],
                     " resources x ", dims[3], " sizes)")
    if (!is.null(units_str) && nzchar(units_str)) {
        header <- paste0(header, " [", units_str, "]")
    }
    cat(header, "\n")
    vals <- unclass(x)
    vals <- vals[is.finite(vals)]
    if (length(vals) == 0) {
        cat("  all NA/Inf\n")
    } else {
        cat("  min=", signif(min(vals), 3),
            " mean=", signif(mean(vals), 3),
            " max=", signif(max(vals), 3), "\n", sep = "")
    }
    invisible(x)
}

#' @export
summary.MRArrayTimeByResourceBySize <- function(object, ...) {
    value_name <- attr(object, "value_name") %||% "MRArrayTimeByResourceBySize"
    units_str <- attr(object, "units")
    res_names <- dimnames(object)[[2]]
    arr <- unclass(object)
    df <- data.frame(
        Resource = res_names,
        Min = apply(arr, 2, min, na.rm = TRUE),
        Mean = apply(arr, 2, mean, na.rm = TRUE),
        Max = apply(arr, 2, max, na.rm = TRUE),
        row.names = NULL,
        stringsAsFactors = FALSE
    )
    result <- list(value_name = value_name, units = units_str,
                   dims = dim(object), per_resource = df)
    class(result) <- "summary.MRArrayTimeByResourceBySize"
    result
}

#' @export
print.summary.MRArrayTimeByResourceBySize <- function(x, ...) {
    header <- x$value_name
    if (!is.null(x$units) && nzchar(x$units)) {
        header <- paste0(header, " [", x$units, "]")
    }
    cat(header, "\n")
    cat(x$dims[1], "times x", x$dims[2], "resources x", x$dims[3],
        "sizes\n\n")
    print(x$per_resource, row.names = FALSE)
    invisible(x)
}

#' @rdname plot.MRArrayResourceBySize
#' @export
plot.MRArrayTimeByResourceBySize <- function(x, time = NULL, ...) {
    slice <- MRArrayTimeByResourceBySize_slice(x, time = time)
    plot.MRArrayResourceBySize(slice, ...)
}

MRArrayTimeByResourceBySize_slice <- function(x, time = NULL) {
    params <- attr(x, "params")
    value_name <- attr(x, "value_name")
    units <- attr(x, "units")
    arr <- unclass(x)

    times <- as.numeric(dimnames(arr)[[1]])
    if (is.null(time)) {
        tidx <- dim(arr)[1]
    } else {
        tidx <- which.min(abs(times - time))
    }
    slice <- matrix(arr[tidx, , , drop = FALSE],
                    nrow = dim(arr)[2],
                    dimnames = dimnames(arr)[2:3])
    MRArrayResourceBySize(slice, value_name = value_name,
                          units = units, params = params)
}

#' @rdname plot.MRArrayResourceBySize
#' @export
plotHover.MRArrayTimeByResourceBySize <- function(x, ...) {
    mizer::plotHover(plot(x, ...), ...)
}

#' @export
as.data.frame.MRArrayTimeByResourceBySize <- function(x, row.names = NULL,
                                                      optional = FALSE, ...) {
    arr <- unclass(x)
    times <- as.numeric(dimnames(arr)[[1]])
    if (any(is.na(times))) times <- seq_len(dim(arr)[1])
    res_names <- dimnames(arr)[[2]]
    if (is.null(res_names)) res_names <- seq_len(dim(arr)[2])
    w <- as.numeric(dimnames(arr)[[3]])
    if (any(is.na(w))) w <- seq_len(dim(arr)[3])

    data.frame(
        expand.grid(time = times, resource = res_names, w = w,
                    stringsAsFactors = FALSE),
        value = as.numeric(arr),
        row.names = row.names,
        check.names = !optional
    )
}

#' @export
`[.MRArrayTimeByResourceBySize` <- function(x, i, j, k, ..., drop = TRUE) {
    result <- NextMethod()
    if (is.array(result) && length(dim(result)) == 3) {
        attr(result, "value_name") <- attr(x, "value_name")
        attr(result, "units") <- attr(x, "units")
        attr(result, "params") <- attr(x, "params")
        class(result) <- c("MRArrayTimeByResourceBySize", "array")
    } else if (is.matrix(result) &&
               identical(names(dimnames(result)), c("resource", "w"))) {
        # A single time step was selected, leaving a resource x size spectrum.
        result <- MRArrayResourceBySize(result,
                                        value_name = attr(x, "value_name"),
                                        units = attr(x, "units"),
                                        params = attr(x, "params"))
    }
    result
}

#' @export
Ops.MRArrayTimeByResourceBySize <- function(e1, e2) {
    if (is.MRArrayTimeByResourceBySize(e1)) e1 <- unclass_mr_resource(e1)
    if (!missing(e2) && is.MRArrayTimeByResourceBySize(e2)) {
        e2 <- unclass_mr_resource(e2)
    }
    op <- match.fun(.Generic)
    if (missing(e2)) op(e1) else op(e1, e2)
}

#' @export
str.MRArrayTimeByResourceBySize <- function(object, ...) {
    params <- attr(object, "params")
    attr(object, "params") <- NULL
    class(object) <- "array"
    out <- utils::capture.output(utils::str(object, ...))
    out[1] <- paste0(" 'MRArrayTimeByResourceBySize' ", sub("^ ", "", out[1]))
    cat(paste0(out, collapse = "\n"), "\n", sep = "")
    if (!is.null(params)) {
        cat(" - attr(*, \"params\")=Formal class 'MizerParams' [package \"mizer\"] with ",
            length(methods::slotNames(params)), " slots\n", sep = "")
    }
    invisible(NULL)
}
