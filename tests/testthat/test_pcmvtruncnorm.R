context("Testing pcmvtruncnorm")

## From Good Practice, testing multivariate Conditional Probabilites is not needed
message("No need to check multivariate density. All checking is done either in condtMVN or in tmvtnorm package. The tmvtnorm package does a good job checking arguments.")
# prob <- tmvtnorm::ptmvnorm(
#    lowerY,  upperY,
#   mean =  params$condMean,
#   sigma = params$condVar,
#   lower = params$condLower,
#   upper = params$condUpper,
#   ...
# )

# Example: truncated between -10 and 10.
d <- 5
rho <- 0.9
Sigma <- matrix(0, nrow = d, ncol = d)
Sigma <- rho^abs(row(Sigma) - col(Sigma))



# Test lowerY and upperY only here.
test_that("lowerY and upperY must have same dimension as the conditional distribution (dependent.ind),
          not Z", {
  expect_error(
    pcmvtruncnorm(rep(-0.5, 2), rep(0, 3),
      mean = rep(1, d),
      sigma = Sigma,
      lower = rep(-10, d),
      upper = rep(10, d),
      dependent.ind = c(2, 3, 5),
      given.ind = c(1, 4), X.given = c(1, -1)
    )
  )

  expect_error(
    pcmvtruncnorm(rep(-0.5, 3), rep(0, 4),
      mean = rep(1, d),
      sigma = Sigma,
      lower = rep(-10, d),
      upper = rep(10, d),
      dependent.ind = c(2, 3, 5),
      given.ind = c(1, 4), X.given = c(1, -1)
    )
  )
})



test_that("lowerY must be less than upperY", {
  # lowerY = upperY in all dimensions
  # Condition: if(identical(lowerY, upperY))
  ret <- pcmvtruncnorm(rep(-0.5, 3), rep(-0.5, 3),
    mean = rep(1, d),
    sigma = Sigma,
    lower = rep(-10, d),
    upper = rep(10, d),
    dependent.ind = c(2, 3, 5),
    given.ind = c(1, 4), X.given = c(1, -1)
  )
  expect_equal(ret, 0)
  #   warning("Warning: lowerY and upperY are equal in all dimenseions. Returning zero")
  #   return(0)

  # If lowerY > upperY in any element, expect an error.
  expect_error(
    pcmvtruncnorm(rep(0, 3), rep(-0.5, 3),
      mean = rep(1, d),
      sigma = Sigma,
      lower = rep(-10, d),
      upper = rep(10, d),
      dependent.ind = c(2, 3, 5),
      given.ind = c(1, 4), X.given = c(1, -1)
    )
  )

  # if one element of lowerY = one element of upper Y,
  # want function to run if one value in lowerY equals upperY
  # An error in truncnorm that needs to be fixed. Ideally, you can just take conditional in two dimensions, and remove the zero.
  expect_error(
    pcmvtruncnorm(c(-0.5, -0.5, 0),  c(0, 0, 0),
      mean = rep(1, d),
      sigma = Sigma,
      lower = rep(-10, d),
      upper = rep(10, d),
      dependent.ind = c(2, 3, 5),
      given.ind = c(1, 4), X.given = c(1, -1)
    )
  )
})
