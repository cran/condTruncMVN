#' Random Sample from Conditional Truncated Multivariate Normal

#' @description Randomly samples from conditional truncated multivariate normal distribution variate, **Y|X**, where **Z = (X, Y)** is the fully joint multivariate normal distribution with \code{mean}, covariance matrix \code{sigma}, and truncated between \code{lower} and \code{upper}.  See the vignette for more information.

#' @note
#'    Uses \code{\link[tmvmixnorm]{rtmvn}} from the **tmvmixnorm** package to find the random variate.
#' @inheritParams condtMVN
#' @inheritParams tmvmixnorm::rtmvn
#' @param burn the burn-in, which is the number of initial iterations to be discarded. Default: 10. Passed to \code{\link[tmvmixnorm]{rtmvn}}().

#' @importFrom truncnorm rtruncnorm
#' @importFrom tmvmixnorm rtmvn
#' @export

#' @examples
#' # Generate 2 random numbers from X2,X3,X5|X2,X4 ~ N_3(1, Sigma)
#' # truncated between -10 and 10.
#' d <- 5
#' rho <- 0.9
#' Sigma <- matrix(0, nrow = d, ncol = d)
#' Sigma <- rho^abs(row(Sigma) - col(Sigma))
#'
#' set.seed(2342)
#' rcmvtruncnorm(2,
#'   mean = rep(1, d),
#'   sigma = Sigma,
#'   lower = rep(-10, d),
#'   upper = rep(10, d),
#'   dependent.ind = c(2, 3, 5),
#'   given.ind = c(1, 4), X.given = c(1, -1)
#' )
#'
#' # Example 2: Generate two random numbers from
#' # X1|X2, X3, X4, X5 ~ N(1, Sigma) truncated between -10 and 10.
#' set.seed(2342)
#' rcmvtruncnorm(2,
#'   mean = rep(1, d),
#'   sigma = Sigma,
#'   lower = rep(-10, d),
#'   upper = rep(10, d),
#'   dependent.ind = 1,
#'   given.ind = 2:5, X.given = c(1, -1, 1, -1)
#' )
rcmvtruncnorm <- function(
                          n,
                          mean, sigma, lower, upper,
                          dependent.ind, given.ind, X.given,
                          init  = rep(0, length(mean)),
                          burn  = 10L, thin = 1
) { # check.sigma  = TRUE

  check_constants(n)
  if (n == 0) {stop("Error: The number of random samples must be positive integer.")}
  check_constants(burn)
  check_constants(thin)
  #  if(!is.wholenumber(burn)) stop("Error: The burn-in must be a non-negative number.")
  #  if(!is.naturalnumber(thin)) stop("Error: The thinning must be a positive number.")

  params <- condtMVN(
    mean = mean, sigma = sigma,
    lower = lower, upper = upper,
    dependent.ind = dependent.ind,
    given.ind = given.ind, X.given = X.given,
    init = init
  )
  # Return the density
  if (length(dependent.ind) == 1) {
    # Conditional Truncated Multivariate Normal
    val <- truncnorm::rtruncnorm(
      n = n,
      mean = params$condMean,
      sd = params$condVar,
      a = params$condLower,
      b = params$condUpper
    )

  } else {
    val <- tmvmixnorm::rtmvn(
      n = n,
      Mean = params$condMean,
      Sigma = params$condVar,
      lower = params$condLower,
      upper = params$condUpper,
      int = params$condInit,
      burn = burn,
      thin = thin
    )
  }
  return(val)
}


check_constants <- function(K) {
  if (is.null(K))
    stop(sprintf(" must be non-null."), call. = TRUE)
  if (!is.numeric(K) | length(K) != 1)
    stop(sprintf(" must be numeric of length 1"), call. = TRUE)
  if (!is.wholenumber(K)) {
    stop(sprintf(" must be a non-negative integer"), call. = TRUE)
  }
  return(K)
}

is.integer <- function(x, tol = .Machine$double.eps^0.5) {
  abs(x - round(x)) < tol
}

# #' @rdname is.integer
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5) {
  x > -1 && is.integer(x, tol)
}

# #' @rdname is.integer
is.naturalnumber <- function(x, tol = .Machine$double.eps^0.5) {
  x > 0 && is.integer(x, tol)
}
