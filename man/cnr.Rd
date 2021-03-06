% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cnr.R
\name{cnr}
\alias{cnr}
\alias{summary.cpr_cnr}
\title{Control Net Reduction}
\usage{
cnr(x, keep = -1, p = 2, margin, n_polycoef = 20L,
  progress = interactive(), ...)

\method{summary}{cpr_cnr}(object, ...)
}
\arguments{
\item{x}{a \code{cnr_cp} or \code{cnr_tensor} object}

\item{keep}{keep (store) the regression fit for the first \code{keep}
\code{cpr_cn} objects in the list returned by \code{cnr}.}

\item{p}{defaults to 2L, the L^p norm used in determining the influence
weight of each internal knot.  Passed to
\code{\link{influence_weights}}.}

\item{margin}{the margins to apply the CNR algorithm to.  Passed to
\code{\link{influence_weights}}.}

\item{n_polycoef}{the number of polynomial coefficients to use when assessing
the influence of each internal knot.}

\item{progress}{show a progress bar.}

\item{...}{not currently used}

\item{object}{a \code{cpr_cnr} object}
}
\description{
Run the Control Net Reduction Algorithm.
}
\details{
\code{cnr} runs the control net reduction algorithm.

\code{keep} will keep the regression fit as part of the \code{cnr\_cp} object
for models with up to and including keep fits.  For example, if \code{keep =
10} then the resulting \code{cnr\_cnr} object will have the regression fit
stored in the first \code{keep + 1} (zero internal knots, one internal knot,
\ldots, \code{keep} internal knots) \code{cnr\_cp} objects in the list.  The
limit on the number of stored regression fits is to keep memory usage down.
}
\seealso{
\code{\link{influence_weights}}, \code{\link{cpr}} for the
uni-variable version, Control Polygon Reduction.
}
\author{
Peter DeWitt \email{dewittpe@gmail.com}
}
