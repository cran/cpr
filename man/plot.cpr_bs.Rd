% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot.cpr_bs.R
\name{plot.cpr_bs}
\alias{plot.cpr_bs}
\title{Plot B-spline Basis}
\usage{
\method{plot}{cpr_bs}(x, ..., show_xi = TRUE, show_x = FALSE, color = TRUE, digits = 2, n = 100)
}
\arguments{
\item{x}{a \code{cpr_bs} object}

\item{show_xi}{logical, show the knot locations, using the Greek letter xi, on the x-axis}

\item{show_x}{logical, show the x values of the knots on the x-axis}

\item{color}{logical, if \code{TRUE} (default) the splines are plotted in
color.  If \code{FALSE} all splines are black lines.}

\item{digits}{number of digits to the right of the decimal place to report
for the value of each knot.}

\item{n}{number of values to use to plot the splines, defaults to 100}

\item{\ldots}{not currently used}
}
\value{
a ggplot
}
\description{
Wrapper around several ggplot2 calls to plot a B-spline basis
}
\examples{
bmat <- bsplines(seq(-3, 2, length = 1000), iknots = c(-2, 0, 0.2))
plot(bmat, show_xi = TRUE,  show_x = TRUE)
plot(bmat, show_xi = FALSE, show_x = TRUE)
plot(bmat, show_xi = TRUE,  show_x = FALSE)  ## Default
plot(bmat, show_xi = FALSE, show_x = FALSE)
plot(bmat, show_xi = FALSE, show_x = FALSE)
plot(bmat, show_xi = FALSE, show_x = FALSE, color = FALSE)
}
\seealso{
\code{\link{bsplines}}
}
