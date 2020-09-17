
context("Conditional Distributions-dependent.ind and given.ind")

# Common Example
library(condTruncMVN)
d <- 5
rho <- 0.9
Sigma <- matrix(0, nrow = d, ncol = d)
Sigma <- rho^abs(row(Sigma) - col(Sigma))

test_that("Dependent.ind is empty. No conditional distribution exits", {
  expect_error(
    condtMVN(mean = rep(1, d),
      sigma = Sigma,
      lower = rep(-10, d),
      upper = rep(10, d),
      dependent.ind = numeric(),
      given.ind = 1:5, X.given = 1:5
    )
  )
})

test_that("given.ind is empty. No conditioning is taking place", {
  # is eqiuvalent to
  #     # if(length(dependent.ind) == d) #d is dimension of normal.
  #         if(length(given.ind) == 0 | X.given )
  no.given <- condtMVN(mean = rep(1, d),
    sigma = Sigma,
    lower = rep(-10, d),
    upper = rep(10, d),
    dependent.ind = c(2, 3, 5),
    given.ind = numeric(), X.given = numeric()
  )
  expect_equal(no.given$condMean, rep(1, d))
  expect_equal(no.given$condSigma, Sigma)
  expect_equal(no.given$condLower, rep(-10, d))
  expect_equal(no.given$condUpper, rep(10, d))
  expect_equal(no.given$condInit, rep(0, d))


})

test_that("`given.ind` and `X.given` matches dimensions.", {
  # given.ind has fewer variables than X.given
  expect_error(
    condtMVN(mean = rep(1, d),
      sigma = Sigma,
      lower = rep(-10, d),
      upper = rep(10, d),
      dependent.ind = c(2, 3, 5),
      given.ind = c(1), X.given = c(1, -1)
    )
  )
})


test_that("Whether you condition the random variable in Y in X.", {
  expect_warning(
    condtMVN(mean = rep(1, d),
      sigma = Sigma,
      lower = rep(-10, d),
      upper = rep(10, d),
      dependent.ind = c(2, 3, 4, 5),
      given.ind = 5, X.given = 1
    )
  )
})

# GoodPractice: Can't test in case condtMVN() produces a non-definite matrix.
# if(!isTRUE(matrixNormal::is.positive.definite(params$condVar)) )
#   warning("#> The conditional is not positive-definite. Huh? Investigate! \n", call. = FALSE, immediate. = TRUE)
