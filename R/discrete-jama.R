#' Journal of the American Medical Association color palettes
#'
#' Color palette inspired by plots in
#' _The Journal of the American Medical Association_.
#'
#' @param palette Palette type.
#'   Currently there is one available option: `"default"`
#'   (7-color palette).
#' @param alpha Transparency level, a real number in (0, 1].
#'   See `alpha` in [grDevices::rgb()] for details.
#'
#' @export pal_jama
#'
#' @importFrom grDevices col2rgb rgb
#' @importFrom scales manual_pal
#'
#' @author Nan Xiao | \email{me@nanx.me} | <https://nanx.me>
#'
#' @examples
#' library("scales")
#' show_col(pal_jama("default")(7))
#' show_col(pal_jama("default", alpha = 0.6)(7))
pal_jama <- function(palette = c("default"), alpha = 1) {
  palette <- match.arg(palette)

  if (alpha > 1L || alpha <= 0L) stop("alpha must be in (0, 1]")

  raw_cols <- ggsci_db$"jama"[[palette]]
  raw_cols_rgb <- col2rgb(raw_cols)
  alpha_cols <- rgb(
    raw_cols_rgb[1L, ], raw_cols_rgb[2L, ], raw_cols_rgb[3L, ],
    alpha = alpha * 255L, names = names(raw_cols),
    maxColorValue = 255L
  )

  manual_pal(unname(alpha_cols))
}

#' Journal of the American Medical Association color scales
#'
#' See [pal_jama()] for details.
#'
#' @inheritParams pal_jama
#' @param ... Additional parameters for [ggplot2::discrete_scale()].
#'
#' @export scale_color_jama
#'
#' @importFrom ggplot2 discrete_scale
#'
#' @author Nan Xiao | \email{me@nanx.me} | <https://nanx.me>
#'
#' @rdname scale_jama
#'
#' @examples
#' library("ggplot2")
#' data("diamonds")
#'
#' ggplot(
#'   subset(diamonds, carat >= 2.2),
#'   aes(x = table, y = price, colour = cut)
#' ) +
#'   geom_point(alpha = 0.7) +
#'   geom_smooth(method = "loess", alpha = 0.1, size = 1, span = 1) +
#'   theme_bw() +
#'   scale_color_jama()
#'
#' ggplot(
#'   subset(diamonds, carat > 2.2 & depth > 55 & depth < 70),
#'   aes(x = depth, fill = cut)
#' ) +
#'   geom_histogram(colour = "black", binwidth = 1, position = "dodge") +
#'   theme_bw() +
#'   scale_fill_jama()
scale_color_jama <- function(palette = c("default"), alpha = 1, ...) {
  palette <- match.arg(palette)
  if (is_ggplot2_350()) {
    discrete_scale("colour", palette = pal_jama(palette, alpha), ...)
  } else {
    discrete_scale("colour", scale_name = "jama", palette = pal_jama(palette, alpha), ...)
  }
}

#' @export scale_colour_jama
#' @rdname scale_jama
scale_colour_jama <- scale_color_jama

#' @export scale_fill_jama
#' @importFrom ggplot2 discrete_scale
#' @rdname scale_jama
scale_fill_jama <- function(palette = c("default"), alpha = 1, ...) {
  palette <- match.arg(palette)
  if (is_ggplot2_350()) {
    discrete_scale("fill", palette = pal_jama(palette, alpha), ...)
  } else {
    discrete_scale("fill", scale_name = "jama", palette = pal_jama(palette, alpha), ...)
  }
}
