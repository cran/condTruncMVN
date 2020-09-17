#' Density of the Conditional Truncated Multivariate Normal
#'
#' Calculates the density of truncated conditional multivariate normal Y|X: \eqn{f(Y = y | X = X.given)}. See the vignette for more information.
#'
#' @param y vector or matrix of quantiles of Y. If a matrix, each row is taken to be a quantile. This is the quantity that the density is calculated from.
#' @inheritParams condtMVN
#' @inheritParams condMVNorm::dcmvnorm

#'
#' @importFrom tmvtnorm dtmvnorm
#' @importFrom truncnorm dtruncnorm
#' @export
#'
#' @references
#'  Horrace, W.C. 2005. Some results on the multivariate truncated normal distribution. Journal of Multivariate Analysis, 94, 209–221. \url{https://surface.syr.edu/cgi/viewcontent.cgi?article=1149&context=ecn}

#' @examples
#' # Example 1: X2,X3,X5|X2,X4 ~ N_3(1, Sigma)
#' # truncated between -10 and 10.
#' d <- 5
#' rho <- 0.9
#' Sigma <- matrix(0, nrow = d, ncol = d)
#' Sigma <- rho^abs(row(Sigma) - col(Sigma))
#'
#' # Log-density of 0
#' dcmvtruncnorm(
#'   rep(0, 3),
#'   mean = rep(1, 5),
#'   sigma = Sigma,
#'   lower = rep(-10, 5),
#'   upper = rep(10, d),
#'   dependent.ind = c(2, 3, 5),
#'   given.ind = c(1, 4), X.given = c(1, -1),
#'   log = TRUE
#' )
dcmvtruncnorm <- function(
                          y,
                          mean, sigma, lower, upper,
                          dependent.ind, given.ind, X.given,
                          log = FALSE
) {

  if (length(y) != length(dependent.ind))
    stop("lengths of `y' and `dependent.ind' must be same")

  # Remaining function checks done within condtMVN
  ret <- condtMVN(
    mean = mean,
    sigma = sigma,
    lower = lower,
    upper = upper,
    dependent.ind = dependent.ind,
    given.ind = given.ind, X.given = X.given,
    init = 0
  )

  # Return the density
  if (length(dependent.ind) == 1) {
    # Conditional Truncated Normal density.
    val <- truncnorm::dtruncnorm(
      y,
      mean = ret$condMean,
      sd = ret$condVar,
      a = ret$condLower,
      b = ret$condUpper
    )
    dens <- ifelse(log, log(val), val)

  } else {
    dens <- tmvtnorm::dtmvnorm(
      y,
      mean = ret$condMean,
      sigma = ret$condVar,
      lower = ret$condLower,
      upper = ret$condUpper,
      log = log,
      margin = NULL
    )
  }

  return(dens)
}


#  @details
# Let Z have an n-variate truncated normal distribution for mean \eqn{\mu} and covariance \eqn{\Sigma}, truncated between c and d. The pdf of a truncated multivariate normal is given by:
#     \deqn{ f(y, u; mu, Sigma, c, d) = (2*\pi)^n/2 * det(\Sigma)^(-1/2) * exp(-1/2 * (y-\mu)' * \Sigma^(-1) *(y-\mu))/}

#   The function indirectly uses \code{\link[mvtnorm]{dmvnorm}} and \code{\link[mvtnorm]{pmvnorm}} when \code{\link[tmvtnorm]{dtmvnorm}} is called.
