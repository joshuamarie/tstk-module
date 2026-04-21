box::use(
    stats[stl, ts, frequency, time],
)

#' Decompose a time series using STL
#'
#' Applies STL (Seasonal and Trend decomposition using Loess) to a numeric
#' vector or `ts` object. Returns a data frame with columns for the original
#' series, trend, seasonal, and remainder components.
#'
#' @param x A `ts` object or numeric vector.
#' @param period Seasonal period. Inferred from `x` if it is a `ts` object.
#' @param s_window Seasonal window — either `"periodic"` (fixed) or an odd
#' integer controlling smoothness. Default: `"periodic"`.
#' @param t_window Trend window. If `NULL`, a sensible default is chosen by
#' \code{stats::stl}. Default: `NULL`.
#' @param robust Logical. Use robust fitting to reduce the influence of
#' outliers? Default: `FALSE`.
#'
#' @return A data frame with columns: `time`, `observed`, `trend`,
#' `seasonal`, `remainder`.
#'
#' @export
decompose_stl = function(x,
    period = NULL,
    s_window = "periodic",
    t_window = NULL,
    robust = FALSE) {

    if (!inherits(x, "ts")) {
        if (is.null(period)) {
            stop("`period` must be supplied when `x` is not a `ts` object.")
        }
        x = ts(x, frequency = period)
    }

    stl_args = list(
        x = x,
        s.window = s_window,
        robust = robust
    )
    if (!is.null(t_window)) {
        stl_args$t.window = t_window
    }

    fit = do.call(stl, stl_args)
    comp = fit$time.series

    result = data.frame(
        time = as.numeric(time(x)),
        observed = as.numeric(x),
        trend = as.numeric(comp[, "trend"]),
        seasonal = as.numeric(comp[, "seasonal"]),
        remainder = as.numeric(comp[, "remainder"])
    )
    class(result) = c("stl_decomp", "data.frame")
    result
}

#' Strength of trend and seasonality from an STL decomposition
#'
#' Computes the variance-based strength metrics described in
#' Wang, Smith & Hyndman (2006).
#'
#' @param x A data frame returned by \code{decompose_stl()}.
#'
#' @return A named numeric vector with elements `trend` and `seasonal`,
#' each in \eqn{[0, 1]}.
#'
#' @export
stl_strength = function(x) {
    var_r = var(x$remainder)
    f_trend = max(0, 1 - var_r / var(x$trend + x$remainder))
    f_seasonal = max(0, 1 - var_r / var(x$seasonal + x$remainder))
    c(trend = f_trend, seasonal = f_seasonal)
}
