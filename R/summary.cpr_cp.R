#' Summarize a Control Polygon Object
#'
#' @param object a \code{cpr_cp} object
#' @param wiggle logical, if \code{TRUE} then the integral of the squared second
#' derivative of the spline function will be calculated via
#' \code{\link[stats]{integrate}}.
#' @param integrate.args a list of arguments passed to \code{\link{wiggle}} and
#' ultimately \code{\link[stats]{integrate}}.
#' @param ... pass through
#'
#' @return a \code{cpr_summary_cpr_cp} object, that is just a \code{data.frame}
#'
#' @examples
#' set.seed(42)
#' x <- seq(0 + 1/5000, 6 - 1/5000, length.out = 100)
#' bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
#' theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
#' DF <- data.frame(x = x, truth = as.numeric(bmat %*% theta))
#' DF$y <- as.numeric(bmat %*% theta + rnorm(nrow(bmat), sd = 0.3))
#'
#' initial_cp <-
#'   cp(y ~ bsplines(x, iknots = c(1, 1.5, 2.3, 3.0, 4, 4.5), bknots = c(0, 6))
#'      , data = DF
#'      , keep_fit = TRUE # default is FALSE
#'   )
#'
#' summary(initial_cp)
#'
#' @export
summary.cpr_cp <- function(object, wiggle = TRUE, integrate.args = list(), ...){
  out <-
    data.frame(dfs        = length(object$cp$theta),
               n_iknots   = length(object$iknots),
               iknots     = I(list(object$iknots)))

  out[["loglik"]] <- ifelse(is.null(object$loglik), NA_real_, object$loglik)
  out[["rss"]]    <- ifelse(is.null(object$rss), NA_real_, object$rss)
  out[["rse"]]    <- ifelse(is.null(object$rse), NA_real_, object$rse)

  if (wiggle) {
    # NOTE: use wiggle.cpr_cp as the "what" in do.call so there isn't confusion
    # between the argument `wiggle` and the function `wiggle`
    wggl <- try(do.call(what = wiggle.cpr_cp, args = c(list(object = object), integrate.args)), silent = TRUE)
    fdsc <- try(do.call(what = sign_changes, args = c(list(object = object))), silent = TRUE)

    if (inherits(x = wggl, what = "integrate")) {
      out$wiggle <- as.numeric(wggl$value)
      attr(out$wiggle, "abs.error") <- wggl$abs.error
      attr(out$wiggle, "subdivisions") <- wggl$subdivisions
      attr(out$wiggle, "message") <- wggl$message
    } else {
      out$wiggle <- NA_real_
      attr(out$wiggle, "error") <- wggl
    }
    out$fdsc <- fdsc
  } else {
    out$wiggle <- NA_real_
    out$fdsc <- NA_integer_
  }

  class(out) <- c("cpr_summary_cpr_cp", class(out))
  out
}
