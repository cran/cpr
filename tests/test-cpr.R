library(cpr)

################################################################################
#         Test that cpr reduces an inital control polygon as expected          #

set.seed(42)
x <- seq(0 + 1/5000, 6 - 1/5000, length.out = 100)
bmat <- bsplines(x, iknots = c(1, 1.5, 2.3, 4, 4.5), bknots = c(0, 6))
theta <- matrix(c(1, 0, 3.5, 4.2, 3.7, -0.5, -0.7, 2, 1.5), ncol = 1)
DF <- data.frame(x = x, truth = as.numeric(bmat %*% theta))
DF$y <- as.numeric(bmat %*% theta + rnorm(nrow(bmat), sd = 0.3))

initial_cp <-
  cp(y ~ bsplines(x, iknots = c(1, 1.5, 2.3, 3.0, 4, 4.5), bknots = c(0, 6))
     , data = DF
     , keep_fit = TRUE # default is FALSE
  )

#summary(influence_of_iknots(initial_cp))

first_reduction_cp <- cp(y ~ bsplines(x, iknots = c(1, 1.5, 2.3, 3, 4.5), bknots = c(0, 6)), data = DF)
#summary(influence_of_iknots(first_reduction_cp))

second_reduction_cp <- cp(y ~ bsplines(x, iknots = c(1, 1.5, 3, 4.5), bknots = c(0, 6)), data = DF)
#summary(influence_of_iknots(second_reduction_cp))

third_reduction_cp <- cp(y ~ bsplines(x, iknots = c(1, 3, 4.5), bknots = c(0, 6)), data = DF)
#summary(influence_of_iknots(third_reduction_cp))

fourth_reduction_cp <- cp(y ~ bsplines(x, iknots = c(1, 4.5), bknots = c(0, 6)), data = DF)
#summary(influence_of_iknots(fourth_reduction_cp))

fifth_reduction_cp <- cp(y ~ bsplines(x, iknots = 4.5, bknots = c(0, 6)), data = DF)
#summary(influence_of_iknots(fifth_reduction_cp))

sixth_reduction_cp <- cp(y ~ bsplines(x, bknots = c(0, 6)), data = DF)
#summary(influence_of_iknots(sixth_reduction_cp))

cpr0 <- cpr(initial_cp)

stopifnot(isTRUE(all.equal( cpr0[[7]][["cp"]],  initial_cp[["cp"]])))
stopifnot(isTRUE(all.equal( cpr0[[6]][["cp"]],  first_reduction_cp[["cp"]])))
stopifnot(isTRUE(all.equal( cpr0[[5]][["cp"]],  second_reduction_cp[["cp"]])))
stopifnot(isTRUE(all.equal( cpr0[[4]][["cp"]],  third_reduction_cp[["cp"]])))
stopifnot(isTRUE(all.equal( cpr0[[3]][["cp"]],  fourth_reduction_cp[["cp"]])))
stopifnot(isTRUE(all.equal( cpr0[[2]][["cp"]],  fifth_reduction_cp[["cp"]])))
stopifnot(isTRUE(all.equal( cpr0[[1]][["cp"]],  sixth_reduction_cp[["cp"]])))

################################################################################
# summary
s <- summary(cpr0)
stopifnot(identical(nrow(s), 7L))
stopifnot(identical(names(s), c("dfs", "n_iknots", "iknots", "loglik", "rss", "rse", "wiggle", "fdsc", "Pr(>w_(1))")))
stopifnot(isTRUE(all.equal(
    structure(c(3, 3, 3, 3, 3, 3), dim = 2:3, dimnames = list(c("quadratic", "linear"), c("loglik", "rss", "rse")))
    ,
    attr(s, "elbow")
    )
  )
)

################################################################################
# test that there is an error in the plotting method if type is not valid
e <- try(plot(cpr_run, type = "not-a-type"), silent = TRUE)
stopifnot(inherits(e, "try-error"))

################################################################################
# print method returns the object
printed <- print(cpr0)
stopifnot(identical(printed, cpr0))

################################################################################
###                               End of File                                ###
################################################################################
