box::use(
    stats[ts, tsp, frequency, acf, spec.pgram, qchisq],
)

#' Estimate the dominant seasonal period from a time series
#'
#' Uses the periodogram to find the frequency with the highest spectral power
#' and converts it to a period. Useful when the seasonal period is unknown.
#'
#' @param x A `ts` object or numeric vector.
#' @param min_period Minimum period to consider. Default: `2`.
#' @param max_period Maximum period to consider. Default: `floor(length(x) / 2)`.
#'
#' @return A single integer giving the estimated dominant period.
#'
#' @export
estimate_period = function(x, min_period = 2, max_period = NULL) {
    n = length(x)
    if (is.null(max_period)) {
        max_period = floor(n / 2)
    }

    spec = spec.pgram(x, plot = FALSE, taper = 0)
    freq = spec$freq
    power = spec$spec
    period = 1 / freq
    keep = period >= min_period & period <= max_period
    if (!any(keep)) {
        stop("No frequency found within the requested period range.")
    }

    as.integer(round(period[keep][which.max(power[keep])]))
}

#' Test for the presence of seasonality using autocorrelation
#'
#' Computes the autocorrelation at the seasonal lag and tests whether it
#' exceeds the 95 % significance threshold for white noise.
#'
#' @param x A `ts` object or numeric vector.
#' @param period Seasonal period. Inferred from `x` if it is a `ts` object.
#'
#' @return A list with elements:
#' \describe{
#'   \item{`seasonal`}{Logical. `TRUE` if significant seasonality is detected.}
#'   \item{`acf_at_lag`}{The autocorrelation at the seasonal lag.}
#'   \item{`threshold`}{The 95 \% significance threshold.}
#'   \item{`period`}{The period tested.}
#' }
#'
#' @export
has_seasonality = function(x, period = NULL) {
    if (is.ts(x) && is.null(period)) {
        period = frequency(x)
    }
    if (is.null(period)) {
        stop("`period` must be supplied when `x` is not a `ts` object.")
    }

    n = length(x)
    acf_result = acf(x, lag.max = period, plot = FALSE)
    acf_at_lag = acf_result$acf[period + 1]
    threshold = qnorm(0.975) / sqrt(n)

    list(
        seasonal = abs(acf_at_lag) > threshold,
        acf_at_lag = acf_at_lag,
        threshold = threshold,
        period = period
    )
}

#' Summarise seasonality across multiple candidate periods
#'
#' Runs \code{has_seasonality()} for each candidate period and returns a
#' data frame ranking them by absolute autocorrelation.
#'
#' @param x A `ts` object or numeric vector.
#' @param periods An integer vector of candidate periods to test.
#'
#' @return A data frame with columns `period`, `acf_at_lag`, `threshold`,
#' and `seasonal`, sorted by descending `acf_at_lag`.
#'
#' @export
seasonality_summary = function(x, periods) {
    results = lapply(periods, function(p) {
        tryCatch(
            has_seasonality(x, period = p),
            error = function(e) NULL
        )
    })
    results = Filter(Negate(is.null), results)

    rows = lapply(results, function(r) {
        data.frame(
            period = r$period,
            acf_at_lag = r$acf_at_lag,
            threshold = r$threshold,
            seasonal = r$seasonal
        )
    })

    out = do.call(rbind, rows)
    out[order(-abs(out$acf_at_lag)), ]
}
