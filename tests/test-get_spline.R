library(cpr)

x1 <- seq(0, 5.9999, length = 500)
x2 <- seq(0, 6 - sqrt(.Machine$double.eps), length = 123)
bmat1 <- bsplines(x = x1, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
bmat2 <- bsplines(x = x2, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
theta <- c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5)

cp1 <- cp(bmat1, theta)
cp2 <- cp(bmat2, theta)

spline <- get_spline(cp1, n = 123)

stopifnot(isTRUE(
  all.equal(
            spline
            ,
            data.frame(x = x2, y = as.numeric(bmat2 %*% theta))
  )
))

# control net
initial_cn <- cn(log10(pdg) ~ btensor(list(day, age, ttm)
                        , df = list(8, 6, 3)
                        , bknots = list(c(-1, 1), c(44, 53), c(-9, -1))
                        , order = list(3, 4, 2)
                        )
          , data = spdg)

if (interactive()) {
  old_par <- par()
  par(mfrow = c(3, 1))
  get_spline(initial_cn, n = 100) |> plot(type = "l")
  get_spline(initial_cn, margin = 2, n = 100) |> plot(type = "l")
  get_spline(initial_cn, margin = 3, n = 100) |> plot(type = "l")
  par(old_par)

  #get_spline(initial_cn, n = 10) |> dput()
  #get_spline(initial_cn, margin = 2, n = 10) |> dput()
  #get_spline(initial_cn, margin = 3, n = 10) |> dput()
}

expected <- structure(list(x = c(-1, -0.777777779433462, -0.555555558866925, -0.333333338300387, -0.111111117733849, 0.111111102832688, 0.333333323399226, 0.555555543965764, 0.777777764532301, 0.999999985098839), y = c(-0.133762887106663, -0.320552726835096, -0.453930169776096, -0.461840210183064, -0.332093945041971, 0.139354295999598, 0.496456762997947, 0.686489715816332, 0.534159573348981, 0.0146225055203891)), class = "data.frame", row.names = c(NA, -10L))
stopifnot(isTRUE(all.equal(expected, get_spline(initial_cn, n = 10))))

expected <- structure(list(x = c(44, 44.9999999983443, 45.9999999966886, 46.9999999950329, 47.9999999933773, 48.9999999917216, 49.9999999900659, 50.9999999884102, 51.9999999867545, 52.9999999850988), y = c(0.686285778255235, 0.00445867954416262, -0.196819616993946, -0.14570083255892, -0.0703366883505854, -0.139325216264028, -0.22418621303892, -0.188094354192221, -0.0451158549211667, 0.175094646687039)), class = "data.frame", row.names = c(NA, -10L))
stopifnot(isTRUE(all.equal(expected, get_spline(initial_cn, margin = 2, n = 10))))

expected <- structure(list(x = c(-9, -8.1111111127668, -7.22222222553359, -6.33333333830039, -5.44444445106718, -4.55555556383398, -3.66666667660077, -2.77777778936757, -1.88888890213437, -1.00000001490116), y = c(-0.121538778218393, -0.114302628952598, -0.107066479686803, -0.0998303304210085, -0.0925941811552136, -0.0864796293184603, -0.0812970496621639, -0.0761144700058674, -0.070931890349571, -0.0657493106932745)), class = "data.frame", row.names = c(NA, -10L))
stopifnot(isTRUE(all.equal(expected, get_spline(initial_cn, margin = 3, n = 10) )))

################################################################################
# expected an error if length(margin) > 1

test <- tryCatch(get_spline(initial_cn, margin = 2:3, n = 100), error = function(e) e)
stopifnot(inherits(test, "error"))
stopifnot(identical(test$message, "use get_surface when length(margin) > 1."))

################################################################################
# expect an error if derivative is not 0, 1, or 2
test <- tryCatch(get_spline(cp1, derivative = 0:1), error = function(e) e)
stopifnot(inherits(test, "error"))
stopifnot(identical(test$message, "length(derivative) == 1L is not TRUE"))

test <- tryCatch(get_spline(cp1, derivative = 3), error = function(e) e)
stopifnot(inherits(test, "error"))
stopifnot(identical(test$message, "derivative needs to be 0, 1, or 2"))

################################################################################
# derivatives are as expected
f0 <- function(x) {
  #(x + 2) * (x - 1) * (x - 3)
  x^3 - 2 * x^2 - 5 * x + 6
}
f1 <- function(x) {
  3 * x^2 - 4 * x - 5
}
f2 <- function(x) {
  6 * x - 4
}

x <- sort(runif(n = 100, min = -3, max = 5))
bknots = c(-3, 5)
bmat <- bsplines(x, bknots = bknots)
theta <- coef(lm(f0(x) ~ bsplines(x, bknots = bknots) + 0) )

cp0 <- cp(bmat, theta)
spline0 <- get_spline(cp0, derivative = 0)
spline1 <- get_spline(cp0, derivative = 1)
spline2 <- get_spline(cp0, derivative = 2)

stopifnot(isTRUE(all.equal(f0(spline0$x), spline0$y)))
stopifnot(isTRUE(all.equal(f1(spline1$x), spline1$y)))
stopifnot(isTRUE(all.equal(f2(spline2$x), spline2$y)))

################################################################################
# Standard errors
test <- tryCatch(get_spline(cp0, se = TRUE), warning = function(w) w)
stopifnot(inherits(test, "warning"))
stopifnot(identical(test$message, "vcov_theta of cp0 is NULL"))

set.seed(742)
x <- sort(runif(n = 2222, min = -3, max = 5))
sdata <- data.frame(x = x, y = f0(x) + rnorm(2222, sd = 25))
cp0 <- cp(y ~ bsplines(x, bknots = bknots), data = sdata)
spline0 <- get_spline(cp0, se = TRUE, derivative = 0, n = 20)

stopifnot(
  isTRUE(
    all.equal(
      spline0$se
      ,
      predict(cp0, newdata = data.frame(x = spline0$x), se.fit = TRUE)$se.fit
      ,
      check.attributes = FALSE
    )
  )
)

spline1 <- get_spline(cp0, se = TRUE, derivative = 1, n = 20)
spline2 <- get_spline(cp0, se = TRUE, derivative = 2, n = 20)

if (interactive()) {
  old_par <- par()

  par(mfrow = c(1, 3))

  plot(x, f0(x), type = "l", main = "spline")
  lines(spline0$x, spline0$y, pch = 2, col = 'blue')
  lines(spline0$x, spline0$y + 1.96 * spline0$se, pch = 1, col = 'red')
  lines(spline0$x, spline0$y - 1.96 * spline0$se, pch = 1, col = 'green')

  plot(x, f1(x), type = "l", main = "first derivative")
  lines(spline1$x, spline1$y, pch = 2, col = 'blue')
  lines(spline1$x, spline1$y + 1.96 * spline1$se, pch = 1, col = 'red')
  lines(spline1$x, spline1$y - 1.96 * spline1$se, pch = 1, col = 'green')

  plot(x, f2(x), type = "l", main = "second derivative")
  lines(spline2$x, spline2$y, pch = 2, col = 'blue')
  lines(spline2$x, spline2$y + 1.96 * spline2$se, pch = 1, col = 'red')
  lines(spline2$x, spline2$y - 1.96 * spline2$se, pch = 1, col = 'green')

  par(old_par)
}

################################################################################
#                                 End of File                                  #
################################################################################
