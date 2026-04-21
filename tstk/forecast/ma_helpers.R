box::use(
    slider[slide_dbl],
    cli[cli_abort]
)

#' Simple Moving Average (SMA)
#' Uses a symmetric or trailing window via slider::slide_dbl
compute_sma = function(x, window) {
    slide_dbl(x, mean, .before = window - 1L, .complete = TRUE)
}

#' Exponential Moving Average (EMA)
#' alpha = 2 / (window + 1) by default (standard finance convention)
compute_ema = function(x, window, alpha = NULL) {
    if (is.null(alpha)) alpha = 2 / (window + 1)
    if (!is.numeric(alpha) || alpha <= 0 || alpha > 1) {
        cli_abort(c(
            "{.arg alpha} must be a numeric value in (0, 1].",
            "i" = "You've supplied {.val {alpha}}"
        ))
    }
    out = numeric(length(x))
    out[1L] = x[1L]
    for (i in seq_along(x)[-1L]) {
        out[i] = alpha * x[i] + (1 - alpha) * out[i - 1L]
    }
    out
}

#' Weighted Moving Average (WMA)
#' Weights are 1, 2, ..., window — linearly increasing, then normalised
compute_wma = function(x, window) {
    weights = seq_len(window)
    slide_dbl(
        x,
        \(x) sum(x * weights / sum(weights)),
        .before = window - 1L,
        .complete = TRUE
    )
}

validate_window = function(x, window) {
    if (!is.numeric(window) || length(window) != 1L || window < 1L) {
        cli_abort(c(
            "{.arg window} must be a single positive integer.",
            "i" = "You've supplied {.val {window}}"
        ))
    }
    if (window > length(x)) {
        cli_abort(c(
            "{.arg window} ({window}) is larger than the length of {.arg x} ({length(x)}).",
            "i" = "Reduce {.arg window} or supply more data."
        ))
    }
}
