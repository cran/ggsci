% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discrete-flatui.R
\name{scale_color_flatui}
\alias{scale_color_flatui}
\alias{scale_colour_flatui}
\alias{scale_fill_flatui}
\title{Flat UI color scales}
\usage{
scale_color_flatui(
  palette = c("default", "flattastic", "aussie"),
  alpha = 1,
  ...
)

scale_colour_flatui(
  palette = c("default", "flattastic", "aussie"),
  alpha = 1,
  ...
)

scale_fill_flatui(
  palette = c("default", "flattastic", "aussie"),
  alpha = 1,
  ...
)
}
\arguments{
\item{palette}{Palette type. Currently there are three available options:
\itemize{
\item \code{"default"} (10-color palette).
\item \code{"flattastic"} (12-color palette).
\item \code{"aussie"} (10-color palette).
}}

\item{alpha}{Transparency level, a real number in (0, 1].
See \code{alpha} in \code{\link[grDevices:rgb]{grDevices::rgb()}} for details.}

\item{...}{Additional parameters for \code{\link[ggplot2:discrete_scale]{ggplot2::discrete_scale()}}.}
}
\description{
See \code{\link[=pal_flatui]{pal_flatui()}} for details.
}
\examples{
library("ggplot2")
data("diamonds")

p1 <- ggplot(
  subset(diamonds, carat >= 2.2),
  aes(x = table, y = price, colour = cut)
) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "loess", alpha = 0.1, size = 1, span = 1) +
  theme_bw()

p2 <- ggplot(
  subset(diamonds, carat > 2.2 & depth > 55 & depth < 70),
  aes(x = depth, fill = cut)
) +
  geom_histogram(colour = "black", binwidth = 1, position = "dodge") +
  theme_bw()

p1 + scale_color_flatui()
p2 + scale_fill_flatui()

p1 + scale_color_flatui(palette = "default")
p2 + scale_fill_flatui(palette = "default")

p1 + scale_color_flatui(palette = "flattastic")
p2 + scale_fill_flatui(palette = "flattastic")

p1 + scale_color_flatui(palette = "aussie")
p2 + scale_fill_flatui(palette = "aussie")
}
\author{
Clara Jégousse | \email{cat3@hi.is}
}
