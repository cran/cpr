% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iknots_or_df.R
\name{iknots_or_df}
\alias{iknots_or_df}
\title{Internal Knots or Degrees of Freedom}
\usage{
iknots_or_df(x, iknots, df, order)
}
\arguments{
\item{x}{the support - a numeric vector}

\item{iknots}{internal knots - a numeric vector}

\item{df}{degrees of freedom - a numeric value of length 1}

\item{order}{polynomial order}
}
\value{
a numeric vector to use as the internal knots defining a B-spline.
}
\description{
Check order, degrees of freedom (df) and iknots
}
\details{
This is an internal function, not to be exported, and used in the calls for
\code{\link{bsplines}} and \code{\link{bsplineD}}.

Use \code{iknots} preferentially.  If iknots are not provided then return the
\code{\link{trimmed_quantile}} for the appropriate \code{df} and \code{order}
}
\examples{

xvec <- runif(600, min = 0, max = 3)

# return the iknots
cpr:::iknots_or_df(x = xvec, iknots = 1:2, df = NULL, order = NULL)

# return the iknots even when the df and order are provided
cpr:::iknots_or_df(x = xvec, iknots = 1:2, df = 56, order = 12)

# return numeric(0) when df <= order (df < order will also give a warning)
cpr:::iknots_or_df(x = xvec, iknots = NULL, df = 6, order = 6)

# return trimmed_quantile when df > order
# probs = (df - order) / (df - order + 1)
cpr:::iknots_or_df(x = xvec, iknots = NULL, df = 10, order = 4)
cpr::trimmed_quantile(xvec, probs = 1:6 / 7)

}
\seealso{
\code{\link{bsplines}}, \code{\link{bsplineD}},
\code{\link{trimmed_quantile}}
}
