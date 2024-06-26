% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/discrete-nejm.R
\name{pal_nejm}
\alias{pal_nejm}
\title{NEJM color palettes}
\usage{
pal_nejm(palette = c("default"), alpha = 1)
}
\arguments{
\item{palette}{Palette type.
Currently there is one available option: \code{"default"}
(8-color palette).}

\item{alpha}{Transparency level, a real number in (0, 1].
See \code{alpha} in \code{\link[grDevices:rgb]{grDevices::rgb()}} for details.}
}
\description{
Color palette inspired by plots in
\emph{The New England Journal of Medicine}.
}
\examples{
library("scales")
show_col(pal_nejm("default")(8))
show_col(pal_nejm("default", alpha = 0.6)(8))
}
\author{
Nan Xiao | \email{me@nanx.me} | \url{https://nanx.me}
}
