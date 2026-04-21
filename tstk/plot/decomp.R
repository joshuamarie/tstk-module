box::use(
    ggplot2[
        ggplot, aes, geom_line, facet_wrap, vars,
        labeller, theme_minimal, theme,
        element_text, element_blank, scale_x_continuous,
        labs, margin, scale_color_manual, 
        geom_col, geom_hline
    ],
    dplyr[mutate], 
    tidyr[pivot_longer],
)

#' Plot a decomposition object
#'
#' S3 method for objects of class `stl_decomp` or `classical_decomp`,
#' as returned by \code{decompose_stl()} and \code{decompose_classical()}.
#' Produces a four-panel faceted plot of the observed, trend, seasonal,
#' and remainder components.
#'
#' @param x A decomposition object.
#' @param title Plot title. Default: `"Time Series Decomposition"`.
#' @param x_lab Label for the x-axis. Default: `"Time"`.
#' @param color Line colour. Default: `"#2C7BB6"`.
#' @param remainder_color Colour for the remainder panel. Default: `"#D7191C"`.
#' @param ... Further arguments (currently unused).
#'
#' @return A `ggplot` object.
#'
#' @export
plot.stl_decomp = function(
    x,
    title = "Time Series Decomposition",
    x_lab = "Time",
    color = "#2C7BB6",
    remainder_color = "#D7191C",
    ...
) {
    plot_decomp_impl(x, title, x_lab, color, remainder_color)
}

#' @rdname plot.stl_decomp
#' @export
plot.classical_decomp = function(
    x,
    title = "Time Series Decomposition",
    x_lab = "Time",
    color = "#2C7BB6",
    remainder_color = "#D7191C",
    ...
) {

    plot_decomp_impl(x, title, x_lab, color, remainder_color)
}

#' Plot seasonal indices
#'
#' Bar chart of the seasonal indices (one bar per period), useful for
#' communicating the within-cycle pattern to non-technical audiences.
#'
#' @param indices A numeric vector of seasonal indices, as returned by
#' \code{seasonal_indices()}.
#' @param period_labels Optional character vector of labels (e.g. month
#' abbreviations). Recycled if shorter than `indices`. Default: `NULL`
#' (uses integer positions).
#' @param title Plot title. Default: `"Seasonal Indices"`.
#' @param fill Bar fill colour. Default: `"#2C7BB6"`.
#'
#' @return A `ggplot` object.
#'
#' @export
plot_seasonal_indices = function(
    indices,
    period_labels = NULL,
    title = "Seasonal Indices",
    fill = "#2C7BB6"
) {

    n = length(indices)
    x_labels = if (!is.null(period_labels)) {
        rep_len(period_labels, n)
    } else {
        as.character(seq_len(n))
    }

    tbl = data.frame(
        period = factor(x_labels, levels = x_labels),
        index = as.numeric(indices)
    )

    ggplot(tbl, aes(x = period, y = index)) +
        geom_col(fill = fill, width = 0.7) +
        geom_hline(yintercept = 0, linewidth = 0.4, linetype = "dashed") +
        labs(title = title, x = "Period", y = "Seasonal index") +
        theme_minimal(base_size = 12) +
        theme(
            plot.title = element_text(face = "bold", margin = margin(b = 10)),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank()
        )
}

plot_decomp_impl = function(x, title, x_lab, color, remainder_color) {
    component_levels = c("observed", "trend", "seasonal", "remainder")
    component_labels = c(
        observed = "Observed",
        trend = "Trend",
        seasonal = "Seasonal",
        remainder = "Remainder"
    )

    long = pivot_longer(
        x,
        cols = -time,
        names_to = "component",
        values_to = "value"
    ) |> 
        mutate(
            component = factor(component, levels = component_levels),
            is_remainder = component == "remainder"
        )

    ggplot(long, aes(x = time, y = value, color = is_remainder)) +
        geom_line(linewidth = 0.7, na.rm = TRUE) +
        facet_wrap(
            vars(component),
            ncol = 1,
            scales = "free_y",
            labeller = labeller(component = component_labels)
        ) +
        scale_x_continuous(expand = c(0.01, 0)) +
        scale_color_manual(
            values = c(`FALSE` = color, `TRUE` = remainder_color),
            guide = "none"
        ) +
        labs(title = title, x = x_lab, y = NULL) +
        theme_minimal(base_size = 12) +
        theme(
            plot.title = element_text(face = "bold", margin = margin(b = 10)),
            strip.text = element_text(face = "bold", hjust = 0),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 9)
        )
}

box::register_S3_method("plot", "classical_decomp")
box::register_S3_method("plot", "stl_decomp")
