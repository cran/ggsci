% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/continuous-material.R
\name{rgb_material}
\alias{rgb_material}
\title{Material Design color palettes}
\usage{
rgb_material(
  palette = c("red", "pink", "purple", "deep-purple", "indigo", "blue", "light-blue",
    "cyan", "teal", "green", "light-green", "lime", "yellow", "amber", "orange",
    "deep-orange", "brown", "grey", "blue-grey"),
  n = 10,
  alpha = 1,
  reverse = FALSE
)
}
\arguments{
\item{palette}{Palette type. There are 19 available options:
\itemize{
\item \code{"red"}
\item \code{"pink"}
\item \code{"purple"}
\item \code{"deep-purple"}
\item \code{"indigo"}
\item \code{"blue"}
\item \code{"light-blue"}
\item \code{"cyan"}
\item \code{"teal"}
\item \code{"green"}
\item \code{"light-green"}
\item \code{"lime"}
\item \code{"yellow"}
\item \code{"amber"}
\item \code{"orange"}
\item \code{"deep-orange"}
\item \code{"brown"}
\item \code{"grey"}
\item \code{"blue-grey"}
}}

\item{n}{Number of individual colors to be generated.}

\item{alpha}{Transparency level, a real number in (0, 1].
See \code{alpha} in \code{\link[grDevices:rgb]{grDevices::rgb()}} for details.}

\item{reverse}{Logical. Should the order of the colors be reversed?}
}
\description{
Material Design 2 color palettes.
}
\examples{
library("scales")
show_col(pal_material("indigo")(10))
show_col(pal_material("indigo", n = 30, alpha = 0.6, reverse = TRUE)(30))
}
\references{
\url{https://m2.material.io/design/color/the-color-system.html}
}
\author{
Nan Xiao | \email{me@nanx.me} | \url{https://nanx.me}
}
