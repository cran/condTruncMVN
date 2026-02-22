context("Testing rcmvtruncnorm")
# message("Need to check as the tmvmixnorm package does not check arguments well. So I added checks there.")

# Only need to test new parameters for rcmvtruncnorm: n, burn, thin.
d <- 5
rho <- 0.9
Sigma <- matrix(0, nrow = d, ncol = d)
Sigma <- 0.9^abs(row(Sigma) - col(Sigma))

# Example that works
# set.seed(2342)
# rcmvtruncnorm(2,
#           mean = rep(1, d),
#           sigma = Sigma,
#           lower=rep(-10,d),
#           upper=rep(10,d),
#           dependent.ind = c(2,3,5),
#           given.ind = c(1,4), X.given = c(1,-1),
#           thin = 1
# )

test_that("The number of samples is a positive integer.", {
  # n = -2
  expect_error(
    rcmvtruncnorm(n = -2,
      mean = rep(1, d),
      sigma = Sigma,
      lower = rep(-10, d),
      upper = rep(10, d),
      dependent.ind = c(2, 3, 5),
      given.ind = c(1, 4), X.given = c(1, -1)
    )
  )

  # n = 0
  expect_error(
    rcmvtruncnorm(n = 0,
      mean = rep(1, d),
      sigma = Sigma,
      lower = rep(-10, d),
      upper = rep(10, d),
      dependent.ind = c(2, 3, 5),
      given.ind = c(1, 4), X.given = c(1, -1)
    )
  )
})

test_that("The burn-in value should be non-negative.", {
  # Burn-in is negative
  expect_error(
    rcmvtruncnorm(2,
      mean = rep(1, d),
      sigma = Sigma,
      lower = rep(-10, d),
      upper = rep(10, d),
      dependent.ind = c(2, 3, 5),
      given.ind = c(1, 4), X.given = c(1, -1),
      burn = -2L
    )
  )

  # Burn-in is 0.
  set.seed(1203)
  B <- rcmvtruncnorm(1,
    mean = rep(1, d),
    sigma = Sigma,
    lower = rep(-10, d),
    upper = rep(10, d),
    dependent.ind = c(2, 3, 5),
    given.ind = c(1, 4), X.given = c(1, -1),
    burn = 0L
  )
  expect_equal(round(B[1], 7),  0.1516405)
})

# test_that("Thinning rate is positive number", {
#
#   #Thining rate is negative
#   expect_error(
#     rcmvtruncnorm(2,
#                   mean = rep(1, d),
#                   sigma = Sigma,
#                   lower=rep(-10,d),
#                   upper=rep(10,d),
#                   dependent.ind = c(2,3,5),
#                   given.ind = c(1,4), X.given = c(1,-1),
#                   burn = 2L,
#                   thin = -1L
#     )
#   )
#
#   #Thining rate is zero.
#   set.seed(1203)
#   ret <-   rcmvtruncnorm(1,
#                          mean = rep(1, d),
#                          sigma = Sigma,
#                          lower=rep(-10,d),
#                          upper=rep(10,d),
#                          dependent.ind = c(2,3,5),
#                          given.ind = c(1,4), X.given = c(1,-1),
#                          burn = 2,
#                          thin = 0L
#   )
#   expect_equal(round(ret[1], 7), 0.141756)
#
#   #Thining rate is one. (Default)
#   #No test needed
#
# })
