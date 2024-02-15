## ----label = "setup", include = FALSE-----------------------------------------
library(qwraps2)
options(qwraps2_markup = "markdown")
knitr::opts_chunk$set(collapse = TRUE)

## -----------------------------------------------------------------------------
library(cpr)
packageVersion("cpr")

## ----label = "basis_matrix"---------------------------------------------------
x <- seq(0, 5.9999, length.out = 5000)
bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
bmat

## ----label = "plot_bmat", fig.width = 7, fig.height = 4-----------------------
plot(bmat, show_xi = TRUE, show_x = TRUE)

## -----------------------------------------------------------------------------
all(bmat >= 0)
all(bmat <= 1)

## -----------------------------------------------------------------------------
all.equal(rowSums(bmat), rep(1, nrow(bmat)))

## -----------------------------------------------------------------------------
args(bsplines)
args(splines::bs)

## -----------------------------------------------------------------------------
bs_mat <- splines::bs(x, knots = attr(bmat, "iknots"), Boundary.knots = attr(bmat, "bknots"))
str(attributes(bmat))
str(attributes(bs_mat))

## -----------------------------------------------------------------------------
bspline_eg <- bsplines(c(0, 1, 2, 5, 6), bknots = c(1, 5))
bs_eg      <- splines::bs(c(0, 1, 2, 5, 6), Boundary.knots = c(1, 5), intercept = TRUE )

head(bspline_eg)
rowSums(bspline_eg)

head(bs_eg)
rowSums(bs_eg)

## ----label = "define_theta"---------------------------------------------------
theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
cp0 <- cp(bmat, theta)

## ----label = "plot_cp", fig.width = 7, fig.height = 4-------------------------
plot(cp0, show_spline = TRUE)

## ----label = "insert_xi_prime3", fig.width = 7, fig.height = 4----------------
cp1 <- insert_a_knot(cp0, xi_prime = 3)
plot(cp0, cp1, color = TRUE, show_spline = TRUE)

## ----fig.width = 7, fig.height = 7--------------------------------------------
x <- influence_of_iknots(cp0)
summary(x)

## ----fig.width = 7, fig.height = 7--------------------------------------------
ggpubr::ggarrange(
  ggpubr::ggarrange(
      plot(x, j = 3, coarsened = TRUE, restored = FALSE, color = TRUE, show_spline = TRUE) +
        ggplot2::theme(legend.position = "none")
    , plot(x, j = 3, coarsened = FALSE, restored = TRUE, color = TRUE, show_spline = TRUE) +
      ggplot2::theme(legend.position = "none")
    , labels = c("(a)", "(b)")
    , nrow = 1
  )
  , plot(x, j = 3, coarsened = TRUE, restored = TRUE, color = TRUE, show_spline = TRUE) +
    ggplot2::theme(legend.position = "bottom")
  , labels = c("", "(c)")
  , nrow = 2
  , ncol = 1
  , heights = c(1, 2)
)

## ----fig.width = 7, fig.height = 7--------------------------------------------
ggpubr::ggarrange(
  ggpubr::ggarrange(
      plot(x, j = 4, coarsened = TRUE, restored = FALSE, color = TRUE, show_spline = TRUE) +
        ggplot2::theme(legend.position = "none")
    , plot(x, j = 4, coarsened = FALSE, restored = TRUE, color = TRUE, show_spline = TRUE) +
      ggplot2::theme(legend.position = "none")
    , labels = c("(a)", "(b)")
    , nrow = 1
  )
  , plot(x, j = 4, coarsened = TRUE, restored = TRUE, color = TRUE, show_spline = TRUE) +
    ggplot2::theme(legend.position = "bottom")
  , labels = c("", "(c)")
  , nrow = 2
  , ncol = 1
  , heights = c(1, 2)
)

## ----fig.width = 7, fig.height = 4--------------------------------------------
set.seed(42)
x <- seq(0, 5.99999, length.out = 100)
bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
DF <- data.frame(x = x, truth = as.numeric(bmat %*% theta))
DF$y <- as.numeric(bmat %*% theta + rnorm(nrow(bmat), sd = 0.3))

original_data_ggplot_layers <-
  list(
    ggplot2::geom_point(data = DF
                        , mapping = ggplot2::aes(x = x, y = y)
                        , inherit.aes = FALSE
                        , color = "#6F263D"
                        , alpha = 0.2)
    ,
    ggplot2::geom_line(data = DF
                       , mapping = ggplot2::aes(x = x, y = truth)
                       , inherit.aes = FALSE
                       , color = "#6F263D"
                       , alpha = 0.6)
  )

ggplot2::ggplot(DF) + ggplot2::theme_bw() + original_data_ggplot_layers

## ----fig.width = 7, fig.height = 4--------------------------------------------
initial_cp <-
  cp(y ~ bsplines(x, iknots = c(1, 1.5, 2.3, 3.0, 4, 4.5), bknots = c(0, 6))
     , data = DF
     , keep_fit = TRUE # default is FALSE
  )

plot(initial_cp, show_spline = TRUE) + original_data_ggplot_layers

## -----------------------------------------------------------------------------
initial_cp$fit |> coef() |> unname()
initial_cp$cp$theta

## -----------------------------------------------------------------------------
initial_cp |>
  influence_of_iknots() |>
  summary()

## ----results = "asis"---------------------------------------------------------
initial_cp |>
  influence_of_iknots() |>
  summary() |>
  knitr::kable(row.names = TRUE)

## ----results = "asis"---------------------------------------------------------
first_reduction_cp <-
  cp(y ~ bsplines(x, iknots = c(1, 1.5, 2.3, 3, 4.5), bknots = c(0, 6)), data = DF)
first_reduction_cp |>
  influence_of_iknots() |>
  summary() |>
  knitr::kable(row.names = TRUE)

## ----results = "asis"---------------------------------------------------------
second_reduction_cp <-
  cp(y ~ bsplines(x, iknots = c(1, 1.5, 3, 4.5), bknots = c(0, 6)), data = DF)
second_reduction_cp |>
  influence_of_iknots() |>
  summary() |>
  knitr::kable(row.names = TRUE)

## ----results = "asis"---------------------------------------------------------
third_reduction_cp <-
  cp(y ~ bsplines(x, iknots = c(1, 3, 4.5), bknots = c(0, 6)), data = DF)
third_reduction_cp |>
  influence_of_iknots() |>
  summary() |>
  knitr::kable(row.names = TRUE)

## ----results = "asis"---------------------------------------------------------
fourth_reduction_cp <-
  cp(y ~ bsplines(x, iknots = c(1, 4.5), bknots = c(0, 6)), data = DF)
fourth_reduction_cp |>
  influence_of_iknots() |>
  summary() |>
  knitr::kable(row.names = TRUE)

## ----results = "asis"---------------------------------------------------------
fifth_reduction_cp <-
  cp(y ~ bsplines(x, iknots = 4.5, bknots = c(0, 6)), data = DF)
fifth_reduction_cp |>
  influence_of_iknots() |>
  summary() |>
  knitr::kable(row.names = TRUE)

## -----------------------------------------------------------------------------
sixth_reduction_cp <-
  cp(y ~ bsplines(x, bknots = c(0, 6)), data = DF)
sixth_reduction_cp |>
  influence_of_iknots() |>
  summary()

## ----fig.height = 4, fig.width = 7, warning = FALSE, echo = FALSE-------------
ggpubr::ggarrange(
  plot(  initial_cp , first_reduction_cp , second_reduction_cp , third_reduction_cp
       , fourth_reduction_cp , fifth_reduction_cp , sixth_reduction_cp
       , show_spline = FALSE , show_cp = TRUE , color = TRUE
       )
  ,
  plot(  initial_cp , first_reduction_cp , second_reduction_cp , third_reduction_cp
       , fourth_reduction_cp , fifth_reduction_cp , sixth_reduction_cp
       , show_spline = TRUE , show_cp = FALSE , color = TRUE
       )
  , labels = c("(a)", "(b)")
  , common.legend = TRUE
)

## ----fig.width = 7, fig.height = 4, echo = FALSE------------------------------
ggpubr::ggarrange(
    plot(initial_cp, show_spline = TRUE) +
      ggplot2::ggtitle("Initial CP") +
      ggplot2::coord_cartesian(ylim = c(-1, 5)) +
      original_data_ggplot_layers
  , plot(first_reduction_cp, show_spline = TRUE)  +
    ggplot2::ggtitle("First Reduction") +
    ggplot2::coord_cartesian(ylim = c(-1, 5)) +
    original_data_ggplot_layers
  , plot(second_reduction_cp, show_spline = TRUE) +
    ggplot2::ggtitle("Second Reduction") +
    ggplot2::coord_cartesian(ylim = c(-1, 5)) +
    original_data_ggplot_layers
  , plot(third_reduction_cp, show_spline = TRUE)  +
    ggplot2::ggtitle("Third Reduction") +
    ggplot2::coord_cartesian(ylim = c(-1, 5)) +
    original_data_ggplot_layers
  , plot(fourth_reduction_cp, show_spline = TRUE) +
    ggplot2::ggtitle("Fourth Reduction") +
    ggplot2::coord_cartesian(ylim = c(-1, 5)) +
    original_data_ggplot_layers
  , plot(fifth_reduction_cp, show_spline = TRUE) +
    ggplot2::ggtitle("Fifth Reduction") +
    ggplot2::coord_cartesian(ylim = c(-1, 5)) +
    original_data_ggplot_layers
  , common.legend = TRUE
  )

## ----results = 'asis'---------------------------------------------------------
list(  initial_cp , first_reduction_cp , second_reduction_cp , third_reduction_cp
     , fourth_reduction_cp , fifth_reduction_cp , sixth_reduction_cp) |>
  rev() |>
  lapply(summary) |>
  do.call(what = rbind, args = _) |>
  cbind(data.frame(reduction = seq(6, 0, by = -1))) |>
  knitr::kable(row.names = TRUE)

## ----results = 'hide', message = FALSE----------------------------------------
cpr0 <- cpr(initial_cp)

## -----------------------------------------------------------------------------
cpr0

## -----------------------------------------------------------------------------
all.equal( cpr0[["cps"]][[7]][["cp"]],  initial_cp[["cp"]])

# some attributes are different with the last cp due to how the automation
# creates the call vs how the call was created manually.
call_idx <- which(names(cpr0[["cps"]][[6]]) == "call")
all.equal( cpr0[["cps"]][[6]][-call_idx], first_reduction_cp [-call_idx])
all.equal( cpr0[["cps"]][[5]][-call_idx], second_reduction_cp[-call_idx])
all.equal( cpr0[["cps"]][[4]][-call_idx], third_reduction_cp [-call_idx])
all.equal( cpr0[["cps"]][[3]][-call_idx], fourth_reduction_cp[-call_idx])
all.equal( cpr0[["cps"]][[2]][-call_idx], fifth_reduction_cp [-call_idx])
all.equal( cpr0[["cps"]][[1]][-call_idx], sixth_reduction_cp [-call_idx], check.attributes = FALSE)

## ----result = "asis"----------------------------------------------------------
s0 <- summary(cpr0)
knitr::kable(s0, row.names = TRUE)

## ----fig.height = 7, fig.width = 7--------------------------------------------
ggpubr::ggarrange(
  ggpubr::ggarrange(
      plot(cpr0, color = TRUE)
    , plot(cpr0, show_cp = FALSE, show_spline = TRUE, color = TRUE)
    , ncol = 1
    , common.legend = TRUE
  )
  ,
  ggpubr::ggarrange(
      plot(s0, type = "rse")
    , plot(s0, type = "rss")
    , plot(s0, type = "loglik")
    , plot(s0, type = "wiggle")
    , plot(s0, type = "fdsc")
    , plot(s0, type = "Pr(>w_(1))")
  , common.legend = TRUE
  )
  , widths = c(2, 3)
)

## ----fig.width = 7, fig.height = 4--------------------------------------------
initial_cp <- cp(y ~ bsplines(x, df = 54, bknots = c(0, 6)), data = DF)

ggpubr::ggarrange(
  plot(initial_cp, show_cp = TRUE, show_spline = FALSE) + original_data_ggplot_layers
  ,
  plot(initial_cp, show_cp = FALSE, show_spline = TRUE) + original_data_ggplot_layers
)

## ----results = "hide", message = FALSE----------------------------------------
cpr1 <- cpr(initial_cp)

## -----------------------------------------------------------------------------
x <- summary(cpr1)
knitr::kable(head(x, 10))

## -----------------------------------------------------------------------------
plot(x, type = "rse")

## ----fig.width = 7, fig.height = 4--------------------------------------------
ggpubr::ggarrange(
  plot(cpr1[[3]], cpr1[[4]], cpr1[[5]], show_cp = TRUE, show_spline = FALSE, color = TRUE)
  ,
  plot(cpr1[[3]], cpr1[[4]], cpr1[[5]], show_cp = FALSE, show_spline = TRUE, color = TRUE)
  ,
  common.legend = TRUE
)

## ----fig.width = 7, fig.height = 4--------------------------------------------
ggpubr::ggarrange(
    plot(cpr1[[3]], show_cp = FALSE, show_spline = TRUE) +
    ggplot2::ggtitle("Model Index 3") +
    original_data_ggplot_layers
  , plot(cpr1[[4]], show_cp = FALSE, show_spline = TRUE) +
    ggplot2::ggtitle("Model Index 4") +
    original_data_ggplot_layers
  , plot(cpr1[[5]], show_cp = FALSE, show_spline = TRUE) +
    ggplot2::ggtitle("Model Index 5") +
    original_data_ggplot_layers
  , nrow = 1
  , legend = "none"
)

## ----echo = TRUE, eval = FALSE------------------------------------------------
#  vignette(topic = "cnr", package = "cpr")

## ----label = "sessioninfo"----------------------------------------------------
sessionInfo()

