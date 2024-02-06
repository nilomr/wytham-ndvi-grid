#' Custom ggplot2 theme
#'
#' This function returns a custom ggplot2 theme with a minimalistic design.
#'
#' @return A ggplot2 theme object.
#' @import ggplot2
#' @export
#' @examples
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point() +
#'     labs(title = "Custom ggplot2 theme", subtitle = "A minimalistic design") +
#'     titheme()
#'
#' @export
titheme <- function() {
    ggplot2::theme(
        text = ggplot2::element_text(
            size = 12, family = "Roboto Condensed",
            colour = "#272727"
        ),
        panel.grid.minor = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        panel.border = ggplot2::element_rect(
            fill = "transparent", colour = NA
        ),
        panel.background = ggplot2::element_rect(
            fill = "transparent", colour = NA
        ),
        aspect.ratio = .8,
        panel.grid.major.y = ggplot2::element_line(
            color = "#e2e2e2",
            linewidth = 0.5,
            linetype = 1
        ),
        axis.title.y = ggplot2::element_text(
            size = 12,
            margin = ggplot2::margin(t = 0, r = 10, b = 0, l = 0)
        ),
        axis.title.x = ggplot2::element_text(
            size = 12,
            margin = ggplot2::margin(t = 10, r = 0, b = 0, l = 0)
        ),
        strip.background = ggplot2::element_rect(
            fill = "transparent", colour = NA
        ),
        strip.text = ggplot2::element_text(
            size = 12, margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 10)
        ),
        plot.subtitle = ggplot2::element_text(
            margin = ggplot2::margin(t = 0, r = 0, b = 5, l = 0)
        ),
        legend.background = ggplot2::element_rect(
            fill = "transparent", colour = NA
        ),
        legend.box.background = ggplot2::element_rect(
            fill = "transparent", colour = NA
        ),
        legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
        legend.spacing.y = ggplot2::unit(0.3, "lines"),
        # title to roboto condensed, size 12
        plot.title = ggplot2::element_text(
            size = 13, face = "bold", colour = "black",
            margin = ggplot2::margin(t = 0, r = 0, b = 5, l = 0)
        ),
        plot.background = ggplot2::element_rect(
            fill = "#f3f3f3", colour = NA
        )
    )
}
