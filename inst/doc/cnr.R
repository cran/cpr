## ----label = "setup", include = FALSE-----------------------------------------
library(qwraps2)
options(qwraps2_markup = "markdown")
knitr::opts_chunk$set(collapse = TRUE)

## -----------------------------------------------------------------------------
library(cpr)
packageVersion("cpr")

## -----------------------------------------------------------------------------
tpmat <-
  btensor(x = list(x1 = runif(51), x2 = runif(51)),
          iknots = list(numeric(0), c(0.418, 0.582, 0.676, 0.840)),
          bknots = list(c(0, 1), c(0, 1)),
          order = list(3, 4))
tpmat

## -----------------------------------------------------------------------------
theta <-
  c(-0.03760,  0.03760, -0.03760,  0.77579, -0.84546,  0.63644, -0.87674,
     0.71007, -1.21007,  0.29655, -0.57582, -0.26198,  0.23632, -0.58583,
    -0.46271, -0.39724, -0.02194, -1.23562, -0.19377, -0.27948, -1.14028,
     0.00405, -0.50405, -0.99595)

acn <- cn(tpmat, theta)

## ----fig.width = 7, fig.height = 4--------------------------------------------
par(mfrow = c(1, 2))
plot(acn, rgl = FALSE, xlab = "x1", ylab = "x2", zlab = "control net", clim = c(-1.2, 0.3), colkey = FALSE)
plot(acn, rgl = FALSE, show_net = FALSE, show_surface = TRUE, xlab = "x1", ylab = "x2", zlab = "surface", clim = c(-1.2, 0.3))

## -----------------------------------------------------------------------------
f <- function(x1, x2) {(x1 - 0.5)^2 * sin(4 * pi * x2) - x1 * x2}
set.seed(42)
cn_data <- expand.grid(x1 = sort(runif(100)), x2 = sort(runif(100)))
cn_data <- within(cn_data, {z = f(x1, x2)})
initial_cn <-
  cn(z ~ btensor(x        = list(x1, x2)
                 , iknots = list(c(0.234), c(0.418, 0.582, 0.676, 0.840))
                 , bknots = list(c(0, 1), c(0, 1))
                 , order  = list(3, 4)
                 )
     , data = cn_data)

influence_of_iknots(initial_cn)

## -----------------------------------------------------------------------------
cn1 <- update_btensor(initial_cn, iknots = list(numeric(0), c(0.418, 0.582, 0.676, 0.840)))
cn2 <- update_btensor(initial_cn, iknots = list(numeric(0.234), c(0.582, 0.676, 0.840)))

## ----fig.width = 7, fig.height = 4--------------------------------------------
par(mfrow = c(1, 3))
plot(initial_cn, rgl = FALSE, show_surface = TRUE, show_net = FALSE, colkey = FALSE, clim = c(-1.2, 0.3), main = "Original")
plot(cn1,        rgl = FALSE, show_surface = TRUE, show_net = FALSE, colkey = FALSE, clim = c(-1.2, 0.3), main = bquote(Omitting~xi[1,1]))
plot(cn2,        rgl = FALSE, show_surface = TRUE, show_net = FALSE, colkey = FALSE, clim = c(-1.2, 0.3), main = bquote(Omitting~xi[2,1]))

## -----------------------------------------------------------------------------
cnr0 <- cnr(initial_cn)
summary(cnr0)
plot(cnr0)

## ----fig.height = 4, fig.width = 7--------------------------------------------
par(mfrow = c(2, 3))
plot(cnr0[[1]], rgl = FALSE, show_surface = TRUE, show_net = FALSE, clim = c(-1.2, 0.3), main = "Index 1", colkey = FALSE)
plot(cnr0[[2]], rgl = FALSE, show_surface = TRUE, show_net = FALSE, clim = c(-1.2, 0.3), main = "Index 2", colkey = FALSE)
plot(cnr0[[3]], rgl = FALSE, show_surface = TRUE, show_net = FALSE, clim = c(-1.2, 0.3), main = "Index 3", colkey = FALSE)
plot(cnr0[[4]], rgl = FALSE, show_surface = TRUE, show_net = FALSE, clim = c(-1.2, 0.3), main = "Index 4", colkey = FALSE)
plot(cnr0[[5]], rgl = FALSE, show_surface = TRUE, show_net = FALSE, clim = c(-1.2, 0.3), main = "Index 5", colkey = FALSE)
plot(cnr0[[6]], rgl = FALSE, show_surface = TRUE, show_net = FALSE, clim = c(-1.2, 0.3), main = "Index 6", colkey = FALSE)

## ----label = "sessioninfo"----------------------------------------------------
sessionInfo()

