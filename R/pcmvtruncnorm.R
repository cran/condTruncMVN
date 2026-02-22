#' CDF for the Conditional Truncated Multivariate Normal
#'
#' @description
#' Computes the distribution function for  a conditional truncated multivariate normal random variate **Y|X**.
#'
#' @details
#' Calculates the probability that **Y|X** is between \code{lowerY} and \code{upperY}. Z = (X, Y) is the fully joint multivariate normal distribution with mean equal mean and covariance matrix sigma, truncated between lower and upper.  See the vignette for more information.
#'
#' @note
#'   For one-dimension conditionals **Y|X**, this function uses the ptruncnorm() function in the truncnorm package. Otherwise, this function uses **tmvtnorm::**\code{\link[tmvtnorm]{ptmvnorm}}().
#'
#' @inheritParams condtMVN
#' @param lowerY	the vector of lower limits for **Y|X**. Passed to tmvtnorm::\code{\link[tmvtnorm]{ptmvnorm}}().
#' @param upperY	the vector of upper limits for **Y|X**. Must be greater than lowerY. Passed to tmvtnorm::\code{\link[tmvtnorm]{ptmvnorm}}().
#' @param ... Additional arguments passed to **tmvtnorm::**\code{\link[tmvtnorm]{ptmvnorm}}(). The CDF is calculated using the Genz algorithm based on these arguments: maxpts, abseps, and releps.
#'
#' @importFrom tmvtnorm ptmvnorm
#' @importFrom truncnorm ptruncnorm
#' @export
#'
#' @examples
#' # Example 1: Let X2,X3,X5|X2,X4 ~ N_3(1, Sigma)
#' # truncated between -10 and 10.
#' d <- 5
#' rho <- 0.9
#' Sigma <- matrix(0, nrow = d, ncol = d)
#' Sigma <- rho^abs(row(Sigma) - col(Sigma))
#'
#' # Find P(-0.5 < X2,X3,X5 < 0 | X2,X4)
#' pcmvtruncnorm(rep(-0.5, 3), rep(0, 3),
#'   mean = rep(1, d),
#'   sigma = Sigma,
#'   lower = rep(-10, d),
#'   upper = rep(10, d),
#'   dependent.ind = c(2, 3, 5),
#'   given.ind = c(1, 4), X.given = c(1, -1)
#' )
#'
#' # Example 2: Let X1| X2 = 1, X3 = -1, X4 = 1, X5 = -1 ~ N(1, Sigma) truncated
#' # between -10 and 10. Find P(-0.5 < X1 < 0 | X2 = 1, X3 = -1, X4 = 1, X5 = -1).
#' pcmvtruncnorm(-0.5, 0,
#'   mean = rep(1, d),
#'   sigma = Sigma,
#'   lower = rep(-10, d),
#'   upper = rep(10, d),
#'   dependent.ind = 1,
#'   given.ind = 2:5, X.given = c(1, -1, 1, -1)
#' )
pcmvtruncnorm <- function(
                          lowerY, upperY,
                          mean, sigma, lower, upper,
                          dependent.ind, given.ind, X.given,
                          ...
) {
  params <- condtMVN(
    mean = mean,
    sigma = sigma,
    lower = lower,
    upper = upper,
    dependent.ind = dependent.ind,
    given.ind = given.ind, X.given = X.given,
    init = 0
  )

  ## Check lowerY, upperY
  d <- length(lowerY)  # Dimension of conditional distribution
  if (length(lowerY) != length(dependent.ind))
    stop("Error: `lowerY` refers to the lower probability of Y|X and must equal to length of dependent.ind.", call. = FALSE)
  if (length(upperY) != length(dependent.ind))
    stop("Error: `upperY` refers to the upper probability of Y|X and must equal to length of dependent.ind.", call. = FALSE)

  # Check if lowerY < upperY in all dimensions
  when.equal <- lowerY == upperY
  if (identical(lowerY, upperY)) {  # lowerY == upperY in all dimensions
    # equivalent to sum(when.equal) == d
    warning("Warning: lowerY and upperY are equal in all dimenseions. Returning zero")
    prob <- 0
    return(prob)

  } else if (0 < sum(when.equal) & sum(when.equal) < d) { # lowerY == upperY in some dimensions
    # tmvtnorm does not allow lowerY to equal upperY in some dimensions.
    stop("Error: lowerY is equal to upperY in at least one dimension; tmvtnorm does not calculate the CDF",
      call. = FALSE)
    #   warning("Warning: lowerY is equal to upperY in at least one dimension. CDF is calculated  ")
    #   lowerY <- lowerY[!when.equal]
    #   upperY <- upperY[!when.equal]

  } else if (any(lowerY > upperY)) {        # lowerY > upperY in any dimension
    stop("Error: lowerY is higher than to upperY in at least one dimension. Did you mean to switch them?", call. = FALSE)

  } # else lowerY < upperY in all dimensions, which is right!

  if (length(dependent.ind) == 1) {
    # "Computing conditional truncated normal CDF
    message("univariate CDF: using truncnorm::ptruncnorm")
    prob <-
      truncnorm::ptruncnorm(
        upperY,
        mean = params$condMean,
        sd = params$condVar,
        a = params$condLower,
        b = params$condUpper
      ) -
      truncnorm::ptruncnorm(
        lowerY,
        mean = params$condMean,
        sd = params$condVar,
        a = params$condLower,
        b = params$condUpper
      )

  } else {
    prob <- tmvtnorm::ptmvnorm(
      lowerY, upperY,
      mean =  params$condMean,
      sigma = params$condVar,
      lower = params$condLower,
      upper = params$condUpper,
      ...
    )
  }

  return(prob)
}
