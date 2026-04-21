box::use(
    dplyr[mutate],
    slider[slide_dbl],
    cli[cli_abort],
    stats[as.ts, setNames],
    ./ma_helpers[compute_sma, compute_ema, compute_wma, validate_window]
)

#' Moving Averages
#'
#' Compute simple (SMA), exponential (EMA), or weighted (WMA) moving averages
#' for time-series, numeric vectors, and data frames.
#'
#' @param x A `ts`, numeric vector, or `data.frame` / tibble.
#' @param window Integer. Rolling window width (ignored for EMA, but used to
#'   derive `alpha` when `alpha = NULL`).
#' @param type One of `"SMA"`, `"EMA"`, `"WMA"`.
#' @param ... Passed to the underlying compute function (e.g. `alpha` for EMA).
#'
#' @export
moving_ave = function(x, window, type = c("SMA", "EMA", "WMA"), ...) {
    UseMethod("moving_ave")
}

#' @export
moving_ave.ts = function(x, window, type = c("SMA", "EMA", "WMA"), ...) {
    type = match.arg(type)
    validate_window(x, window)

    values = as.numeric(x)
    smoothed = switch(type,
        SMA = compute_sma(values, window),
        EMA = compute_ema(values, window, ...),
        WMA = compute_wma(values, window)
    )

    ts(smoothed, start = start(x), frequency = frequency(x))
}

#' @export
moving_ave.numeric = function(x, window, type = c("SMA", "EMA", "WMA"), ...) {
    type = match.arg(type)
    validate_window(x, window)

    switch(type,
        SMA = compute_sma(x, window),
        EMA = compute_ema(x, window, ...),
        WMA = compute_wma(x, window)
    )
}

#' @export
moving_ave.data.frame = function(
    x,
    window,
    type = c("SMA", "EMA", "WMA"),
    col = NULL,        
    suffix = "_ma",        
    ...
) {
    type = match.arg(type)

    # Resolve target columns
    numeric_cols = names(x)[vapply(x, is.numeric, logical(1L))]
    if (is.null(col)) {
        col = numeric_cols
    } else {
        bad = setdiff(col, names(x))
        if (length(bad)) {
            cli_abort(c(
                "Column(s) not found in {.arg x}: {.val {bad}}"
            ))
        }
        non_numeric = setdiff(col, numeric_cols)
        if (length(non_numeric)) {
            cli_abort(c(
                "Column(s) must be numeric: {.val {non_numeric}}"
            ))
        }
    }

    validate_window(x[[col[1L]]], window)

    new_cols = setNames(
        lapply(col, function(nm) {
            switch(type,
                SMA = compute_sma(x[[nm]], window),
                EMA = compute_ema(x[[nm]], window, ...),
                WMA = compute_wma(x[[nm]], window)
            )
        }),
        paste0(col, suffix)
    )

    mutate(x, !!!new_cols)
}
