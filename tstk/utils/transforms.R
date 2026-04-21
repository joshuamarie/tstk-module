box::use(
    stats[ts, tsp, lag, diff, median],
)

#' Log-transform a time series
#'
#' Applies \code{log(x + offset)} with an optional offset to handle zeros.
#' Preserves `ts` attributes if `x` is a `ts` object.
#'
#' @param x A `ts` object or numeric vector.
#' @param offset A non-negative value added to `x` before taking the log,
#' useful when `x` contains zeros. Default: `0`.
#'
#' @return A transformed object of the same class as `x`.
#'
#' @export
log_transform = function(x, offset = 0) {
    if (any(x + offset <= 0, na.rm = TRUE)) {
        stop("All values of `x + offset` must be strictly positive.")
    }
    result = log(x + offset)
    if (is.ts(x)) {
        result = ts(result, start = tsp(x)[1], frequency = tsp(x)[3])
    }
    result
}

#' Inverse log-transform a time series
#'
#' Reverses \code{log_transform()}: computes \code{exp(x) - offset}.
#'
#' @param x A `ts` object or numeric vector of log-transformed values.
#' @param offset The same offset used in \code{log_transform()}. Default: `0`.
#'
#' @return A transformed object of the same class as `x`.
#'
#' @export
log_inverse = function(x, offset = 0) {
    result = exp(x) - offset
    if (is.ts(x)) {
        result = ts(result, start = tsp(x)[1], frequency = tsp(x)[3])
    }
    result
}

#' Difference a time series
#'
#' Wraps \code{base::diff()} with named arguments and preserves the `ts`
#' class. Supports seasonal differencing via `lag`.
#'
#' @param x A `ts` object or numeric vector.
#' @param lag The lag at which to difference. Use the seasonal period for
#' seasonal differencing. Default: `1`.
#' @param differences The number of times to apply differencing. Default: `1`.
#'
#' @return A differenced object of the same class as `x`, shorter by
#' `lag * differences` observations.
#'
#' @export
difference = function(x, lag = 1, differences = 1) {
    result = diff(x, lag = lag, differences = differences)
    if (is.ts(x)) {
        result = ts(result, frequency = tsp(x)[3])
    }
    result
}

#' Scale a time series to [0, 1]
#'
#' Applies min-max scaling. Returns the scaled series along with the
#' min and max used, so the transform can be reversed.
#'
#' @param x A `ts` object or numeric vector.
#'
#' @return A list with elements:
#' \describe{
#'   \item{`scaled`}{The scaled series.}
#'   \item{`min`}{The minimum value of the original series.}
#'   \item{`max`}{The maximum value of the original series.}
#' }
#'
#' @export
scale_minmax = function(x) {
    xmin = min(x, na.rm = TRUE)
    xmax = max(x, na.rm = TRUE)
    if (xmin == xmax) {
        stop("Cannot scale a constant series.")
    }
    scaled = (x - xmin) / (xmax - xmin)
    if (is.ts(x)) {
        scaled = ts(scaled, start = tsp(x)[1], frequency = tsp(x)[3])
    }
    list(scaled = scaled, min = xmin, max = xmax)
}

#' Reverse a min-max scaling
#'
#' @param x A scaled `ts` object or numeric vector.
#' @param min The minimum from the original series, as returned by
#' \code{scale_minmax()}.
#' @param max The maximum from the original series.
#'
#' @return The rescaled series.
#'
#' @export
scale_minmax_inverse = function(x, min, max) {
    x * (max - min) + min
}
