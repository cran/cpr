#' Vertical Difference between two Control Polygons
#'
#' @return the vertical distance between the control vertices of cp1 to the
#' control polygon cp2.
#'
#' @param cp1 a \code{cpr_cp} object
#' @param cp2 a \code{cpr_cp} object
#'
#' @seealso \code{\link{cp}}, \code{\link{cp_value}}
#'
#' @examples
#' xvec <- runif(n = 500, min = 0, max = 6)
#'
#' # Define the basis matrix
#' bmat1 <- bsplines(x = xvec, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
#' bmat2 <- bsplines(x = xvec, bknots = c(0, 6))
#'
#' # Define the control vertices ordinates
#' theta1 <- c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5)
#' theta2 <- c(1, 3.4, -2, 1.7)
#'
#' # build the two control polygons
#' cp1 <- cp(bmat1, theta1)
#' cp2 <- cp(bmat2, theta2)
#'
#' cp_diff(cp1, cp2)
#'
#' df <- data.frame(x = cp1$cp$xi_star,
#'                  y = cp1$cp$theta,
#'                  yend = cp1$cp$theta + cp_diff(cp1, cp2))
#'
#'
#' plot(cp1, cp2) +
#' ggplot2::geom_segment(data = df
#'   , mapping = ggplot2::aes(x = x, xend = x, y = y, yend = yend)
#'   , color = "red"
#'   , inherit.aes = FALSE)
#'
#' @export
cp_diff <- function(cp1, cp2) {
  UseMethod("cp_diff")
}

#' @export
cp_diff.cpr_cp <- function(cp1, cp2) {
  stopifnot(inherits(cp2, "cpr_cp"))
  unname(sapply(cp1$cp$xi_star, function(x) {cp_value(obj = cp2, x)}) - cp1$cp$theta)
}
