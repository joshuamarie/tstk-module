box::use(
    stats[decompose, ts, time, frequency],
)

#' Decompose a time series using classical decomposition
#'
#' Wraps \code{stats::decompose()} and returns a tidy data frame with the
#' four components. Supports both additive and multiplicative models.
#'
#' @param x A `ts` object or numeric vector.
#' @param period Seasonal period. Required when `x` is not a `ts` object.
#' @param type Decomposition type: `"additive"` or `"multiplicative"`.
#' Default: `"additive"`.
#' @param filter Optional moving-average filter weights passed to
#' \code{stats::decompose()}. Default: `NULL`.
#'
#' @return A data frame with columns: `time`, `observed`, `trend`,
#' `seasonal`, `remainder`.
#'
#' @export
decompose_classical = function(x,
    period = NULL,
    type = c("additive", "multiplicative"),
    filter = NULL) {

    type = match.arg(type)

    if (!inherits(x, "ts")) {
        if (is.null(period)) {
            stop("`period` must be supplied when `x` is not a `ts` object.")
        }
        x = ts(x, frequency = period)
    }

    if (frequency(x) < 2) {
        stop("Classical decomposition requires a seasonal period of at least 2.")
    }

    fit = decompose(x, type = type, filter = filter)

    result = data.frame(
        time = as.numeric(time(x)),
        observed = as.numeric(x),
        trend = as.numeric(fit$trend),
        seasonal = as.numeric(fit$seasonal),
        remainder = as.numeric(fit$random)
    )
    class(result) = c("classical_decomp", "data.frame")
    result
}

#' Seasonal indices from a classical decomposition
#'
#' Extracts one full cycle of seasonal factors — useful for reporting
#' or further modelling.
#'
#' @param x A `ts` object or numeric vector.
#' @param period Seasonal period. Required when `x` is not a `ts` object.
#' @param type `"additive"` or `"multiplicative"`. Default: `"additive"`.
#'
#' @return A numeric vector of length `period` containing the seasonal indices.
#'
#' @export
seasonal_indices = function(x,
    period = NULL,
    type = c("additive", "multiplicative")) {

    type = match.arg(type)

    if (!inherits(x, "ts")) {
        if (is.null(period)) {
            stop("`period` must be supplied when `x` is not a `ts` object.")
        }
        x = ts(x, frequency = period)
    }

    fit = decompose(x, type = type)
    p = frequency(x)
    as.numeric(fit$seasonal[seq_len(p)])
}
