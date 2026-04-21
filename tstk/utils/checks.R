box::use(
    stats[is.ts],
)

#' Check that an object is a valid time series input
#'
#' Validates that `x` is either a `ts` object or a numeric vector.
#' Throws an informative error on failure.
#'
#' @param x A `ts` object or numeric vector.
#' @param min_length Minimum number of observations required. Default: `3`.
#'
#' @return `x` invisibly, so the function can be used inline.
#'
#' @export
check_ts = function(x, min_length = 3) {
    if (!is.numeric(x) && !is.ts(x)) {
        stop("`x` must be a numeric vector or `ts` object, got: ", class(x)[1])
    }
    if (length(x) < min_length) {
        stop("`x` must have at least ", min_length, " observations.")
    }
    invisible(x)
}

#' Check that a seasonal period is valid
#'
#' @param period A positive integer giving the seasonal period.
#'
#' @return `period` invisibly.
#'
#' @export
check_period = function(period) {
    if (!is.numeric(period) || length(period) != 1 ||
        period < 2 || period != round(period)) {
        stop("`period` must be a single integer >= 2.")
    }
    invisible(period)
}

#' Check that a forecast horizon is valid
#'
#' @param h A positive integer giving the forecast horizon.
#'
#' @return `h` invisibly.
#'
#' @export
check_horizon = function(h) {
    if (!is.numeric(h) || length(h) != 1 || h < 1 || h != round(h)) {
        stop("`h` must be a single positive integer.")
    }
    invisible(h)
}

#' Check that a confidence level is valid
#'
#' @param level A numeric value strictly between 0 and 1.
#'
#' @return `level` invisibly.
#'
#' @export
check_level = function(level) {
    if (!is.numeric(level) || length(level) != 1 || level <= 0 || level >= 1) {
        stop("`level` must be a single numeric value strictly between 0 and 1.")
    }
    invisible(level)
}
