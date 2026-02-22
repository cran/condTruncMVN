
context("Dependent.ind has length 1. Univariate Conditional Distribution")

# Common Example
library(condTruncMVN)
d <- 5
rho <- 0.9
Sigma <- matrix(0, nrow = d, ncol = d)
Sigma <- rho^abs(row(Sigma) - col(Sigma))

test_that("test if dependent.ind is univariate length of 1.", {
  l <- condtMVN(
    mean = rep(1, d),
    sigma = Sigma,
    lower = rep(-10, d),
    upper = rep(10, d),
    dependent.ind = 1,
    given.ind = 2:5, X.given = c(1, -1, 1, -1)
  )
  t <- sum(unlist(lapply(l, length)))
  expect_that(t, equals(5))
})

test_that("Univariate Density", {
  dens <- dcmvtruncnorm(
    1,
    mean = rep(1, d),
    sigma = Sigma,
    lower = rep(-10, d),
    upper = rep(10, d),
    dependent.ind = 1,
    given.ind = 2:5, X.given = c(1, -1, 1, -1),
    log = TRUE
  )
  expect_equal(round(dens, 3),  0.742)
})

test_that("Univariate CDF", {
  prob <- pcmvtruncnorm(
    -1, 0,
    mean = rep(1, d),
    sigma = Sigma,
    lower = rep(-10, d),
    upper = rep(10, d),
    dependent.ind = 1,
    given.ind = 2:5, X.given = c(1, -1, 1, -1),
    log = TRUE
  )
  expect_equal(prob, 7.080093e-08)
})

test_that("Univariate Random Variate", {
  set.seed(342)
  samp <- rcmvtruncnorm(
    1,
    mean = rep(1, d),
    sigma = Sigma,
    lower = rep(-10, d),
    upper = rep(10, d),
    dependent.ind = 1,
    given.ind = 2:5, X.given = c(1, -1, 1, -1)
  )
  expect_equal(round(samp, 7), 0.8536735)
})

# Instead of using a number, is it better to test using the function, like tmvtnorm::dtmvnorm()?
#
