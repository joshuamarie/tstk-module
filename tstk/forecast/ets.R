box::use(
    rlang[exec, list2],
    forecast[ets],
    cli[cli_abort],
    fable[ETS],
    fabletools[model],
    purrr[partial]
)

#' Modified exponential smoothing state-space (ETS) model
#' 
#' @param x Dispatched arg. It can be a `ts` object or a data frame object
#' 
#' @seealso [forecast::ets()]
#' @name ets-custom
#' @export
exts = function(x, ...) {
    UseMethod("exts")
}

#' @export
exts.ts = function(x, ...) {
    extra_args = list2(...)
    
    exec(
        ets,
        y = x, 
        !!!extra_args
    )
}

#' @export
exts.tbl_ts = function(x, formula = NULL, ...) {
    if (!inherits(x, "tbl_ts")) {
        cli_abort(c(
            "{.arg x} must be a {.cls tbl_ts}",
            "i" = "You've supplied a {.cls {class(x)}}"
        ))
    }
    extra_args = list2(...)
    
    exec(
        model,
        x,
        ETS(formula),
        !!!extra_args
    )
}


