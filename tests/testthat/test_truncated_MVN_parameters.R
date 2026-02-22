context("Truncated MVN parameters have a different length")
library(condTruncMVN)
d <- 5
rho <- 0.9
Sigma <- matrix(0, nrow = d, ncol = d)
Sigma <- rho^abs(row(Sigma) - col(Sigma))

test_that("mean, lower, upper are all numeric vectors with same length", {
  # mean has different length
  expect_error(
    condtMVN(mean = rep(1, 3),
      sigma = Sigma,
      lower = rep(-10, d),
      upper = rep(10, d),
      dependent.ind = c(2, 3, 5),
      given.ind = c(1, 4), X.given = c(1, -1)
    )
  )

  # lower has different length.
  expect_error(
    condtMVN(mean = rep(1, d),
      sigma = Sigma,
      lower = rep(-10, 4),
      upper = rep(10, d),
      dependent.ind = c(2, 3, 5),
      given.ind = c(1, 4), X.given = c(1, -1)
    )
  )

  # upper has different length
  expect_error(
    condtMVN(mean = rep(1, d),
      sigma = Sigma,
      lower = rep(-10, d),
      upper = rep(10, 2),
      dependent.ind = c(2, 3, 5),
      given.ind = c(1, 4), X.given = c(1, -1)
    )
  )
})

test_that("Sigma has different length than mean, lower, upper", {
  # Sigma has different length.
  expect_error(
    condtMVN(mean = rep(1, d),
      sigma = Sigma[1:3, 1:3],
      lower = rep(-10, d),
      upper = rep(10, d),
      dependent.ind = c(2, 3, 5),
      given.ind = c(1, 4), X.given = c(1, -1)
    )
  )
})

# test_that(
# "Initial values have different length than mean, Sigma, lower, upper", {
#   expect_error(
#     condtMVN(mean = rep(1, d),
#              sigma = Sigma[1:d, 1:d],
#              lower=rep(-10,d),
#              upper=rep(10,d),
#              dependent.ind = c(2,3,5),
#              given.ind = c(1,4), X.given=c(1,-1),
#              init = c(1, 3)
#     )
#   )
# })
