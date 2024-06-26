% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/continuous-gsea.R
\name{pal_gsea}
\alias{pal_gsea}
\title{The GSEA GenePattern color palettes}
\usage{
pal_gsea(palette = c("default"), n = 12, alpha = 1, reverse = FALSE)
}
\arguments{
\item{palette}{Palette type.
Currently there is one available option: \code{"default"}
(continuous palette with 12 base colors).}

\item{n}{Number of individual colors to be generated.}

\item{alpha}{Transparency level, a real number in (0, 1].
See \code{alpha} in \code{\link[grDevices:rgb]{grDevices::rgb()}} for details.}

\item{reverse}{Logical. Should the order of the colors be reversed?}
}
\description{
Color palette inspired by the colors used in the
heatmaps plotted by GSEA GenePattern.
}
\examples{
library("scales")
show_col(pal_gsea("default")(12))
show_col(pal_gsea("default", n = 30, alpha = 0.6, reverse = TRUE)(30))
}
\author{
Nan Xiao | \email{me@nanx.me} | \url{https://nanx.me}
}
