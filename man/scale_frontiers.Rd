% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discrete-frontiers.R
\name{scale_color_frontiers}
\alias{scale_color_frontiers}
\alias{scale_colour_frontiers}
\alias{scale_fill_frontiers}
\title{Frontiers journal color scales}
\usage{
scale_color_frontiers(palette = c("default"), alpha = 1, ...)

scale_colour_frontiers(palette = c("default"), alpha = 1, ...)

scale_fill_frontiers(palette = c("default"), alpha = 1, ...)
}
\arguments{
\item{palette}{Palette type.
Currently there is one available option: \code{"default"}
(10-color palette).}

\item{alpha}{Transparency level, a real number in (0, 1].
See \code{alpha} in \code{\link[grDevices:rgb]{grDevices::rgb()}} for details.}

\item{...}{Additional parameters for \code{\link[ggplot2:discrete_scale]{ggplot2::discrete_scale()}}.}
}
\description{
See \code{\link[=pal_frontiers]{pal_frontiers()}} for details.
}
\examples{
library("ggplot2")
data("diamonds")

ggplot(
  subset(diamonds, carat >= 2.2),
  aes(x = table, y = price, colour = cut)
) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", alpha = 0.1, size = 1, span = 1) +
  theme_dark() +
  theme(
    panel.background = element_rect(fill = "#2D2D2D"),
    legend.key = element_rect(fill = "#2D2D2D")
  ) +
  scale_color_frontiers()

ggplot(
  subset(diamonds, carat > 2.2 & depth > 55 & depth < 70),
  aes(x = depth, fill = cut)
) +
  geom_histogram(colour = "black", binwidth = 1, position = "dodge") +
  theme_dark() +
  theme(
    panel.background = element_rect(fill = "#2D2D2D")
  ) +
  scale_fill_frontiers()
}
\author{
Clara Jégousse | \email{cat3@hi.is}
}
