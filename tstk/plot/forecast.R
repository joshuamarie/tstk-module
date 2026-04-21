box::use(
    ggplot2[
        ggplot, aes, geom_line, geom_ribbon, geom_vline,
        scale_x_continuous, scale_color_manual,
        theme_minimal, theme, element_text, element_blank,
        labs, margin
    ]
)

#' Plot a forecast object
#'
#' S3 method for objects of class `carrier_forecast`, as returned by the
#' \code{forecast} submodules. Combines the historical series with point
#' forecasts and optional prediction interval ribbons.
#'
#' @param x A `carrier_forecast` object with elements `history` (data frame
#' with columns `time` and `value`) and `forecast` (data frame with columns
#' `time`, `mean`, and optionally `lo_80`, `hi_80`, `lo_95`, `hi_95`).
#' @param title Plot title. Default: `"Forecast"`.
#' @param x_lab x-axis label. Default: `"Time"`.
#' @param y_lab y-axis label. Default: `"Value"`.
#' @param hist_color Colour for the historical line. Default: `"#333333"`.
#' @param fc_color Colour for the forecast line and intervals.
#' Default: `"#2C7BB6"`.
#' @param show_80 Show the 80 \% prediction interval? Default: `TRUE`.
#' @param show_95 Show the 95 \% prediction interval? Default: `TRUE`.
#' @param ... Further arguments (currently unused).
#'
#' @return A `ggplot` object.
#'
#' @export
plot.carrier_forecast = function(
    x,
    title = "Forecast",
    x_lab = "Time",
    y_lab = "Value",
    hist_color = "#333333",
    fc_color = "#2C7BB6",
    show_80 = TRUE,
    show_95 = TRUE,
    ...
) {

    history = x$history
    fc = x$forecast
    origin = max(history$time)

    p = ggplot() +
        geom_vline(
            xintercept = origin,
            linetype = "dashed",
            linewidth = 0.4,
            color = "grey50"
        )

    if (show_95 && all(c("lo_95", "hi_95") %in% names(fc))) {
        p = p + geom_ribbon(
            data = fc,
            mapping = aes(x = time, ymin = lo_95, ymax = hi_95),
            fill = fc_color,
            alpha = 0.15
        )
    }

    if (show_80 && all(c("lo_80", "hi_80") %in% names(fc))) {
        p = p + geom_ribbon(
            data = fc,
            mapping = aes(x = time, ymin = lo_80, ymax = hi_80),
            fill = fc_color,
            alpha = 0.25
        )
    }

    p = p +
        geom_line(
            data = history,
            mapping = aes(x = time, y = value, color = "Observed"),
            linewidth = 0.7
        ) +
        geom_line(
            data = fc,
            mapping = aes(x = time, y = mean, color = "Forecast"),
            linewidth = 0.7
        )

    p +
        scale_color_manual(
            name = NULL,
            values = c(Observed = hist_color, Forecast = fc_color)
        ) +
        scale_x_continuous(expand = c(0.01, 0)) +
        labs(title = title, x = x_lab, y = y_lab) +
        theme_minimal(base_size = 12) +
        theme(
            plot.title = element_text(face = "bold", margin = margin(b = 10)),
            legend.position = "bottom",
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 9)
        )
}

#' Plot multiple forecasts overlaid for comparison
#'
#' Useful for visually comparing ARIMA, ETS, and Prophet side-by-side on
#' the same axes. Each model's forecast is drawn as a distinct coloured line;
#' no interval ribbons are shown so the plot stays readable.
#'
#' @param history Data frame with columns `time` and `value`.
#' @param forecasts A named list of `carrier_forecast` objects or data frames
#' with columns `time` and `mean`. Names become legend labels
#' (e.g. `list(ARIMA = arima_fc, ETS = ets_fc)`).
#' @param title Plot title. Default: `"Forecast Comparison"`.
#' @param x_lab x-axis label. Default: `"Time"`.
#' @param y_lab y-axis label. Default: `"Value"`.
#' @param hist_color Colour for the historical line. Default: `"#333333"`.
#' @param fc_colors Named character vector mapping model names to colours.
#' If `NULL`, a built-in palette is used.
#'
#' @return A `ggplot` object.
#'
#' @export
plot_forecast_comparison = function(
    history,
    forecasts,
    title = "Forecast Comparison",
    x_lab = "Time",
    y_lab = "Value",
    hist_color = "#333333",
    fc_colors = NULL
) {

    default_palette = c(
        "#2C7BB6", "#D7191C", "#1A9641",
        "#FDAE61", "#762A83", "#F46D43"
    )

    model_names = names(forecasts)
    if (is.null(model_names) || any(model_names == "")) {
        stop("`forecasts` must be a fully named list.")
    }

    if (is.null(fc_colors)) {
        fc_colors = stats::setNames(
            default_palette[seq_along(model_names)],
            model_names
        )
    }

    origin = max(history$time)

    p = ggplot() +
        geom_vline(
            xintercept = origin,
            linetype = "dashed",
            linewidth = 0.4,
            color = "grey50"
        ) +
        geom_line(
            data = history,
            mapping = aes(x = time, y = value, color = "Observed"),
            linewidth = 0.7
        )

    for (nm in model_names) {
        fc = if (inherits(forecasts[[nm]], "carrier_forecast")) {
            forecasts[[nm]]$forecast
        } else {
            forecasts[[nm]]
        }
        p = p + geom_line(
            data = fc,
            mapping = aes(x = time, y = mean, color = !!nm),
            linewidth = 0.7
        )
    }

    p +
        scale_color_manual(
            name = NULL,
            values = c(Observed = hist_color, fc_colors)
        ) +
        scale_x_continuous(expand = c(0.01, 0)) +
        labs(title = title, x = x_lab, y = y_lab) +
        theme_minimal(base_size = 12) +
        theme(
            plot.title = element_text(face = "bold", margin = margin(b = 10)),
            legend.position = "bottom",
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 9)
        )
}
