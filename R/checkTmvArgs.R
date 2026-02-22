#' Checking Truncated Arguments
#' @description Checks arguments for truncated normal distribution
#' @inheritParams condtMVN
#' @return The same parameters modified after checking.
#' @details
#'  Function taken from tmvtnorm.
#'  Included because tmvtnorm checks the arguments well, rtmixnorm does not.
#' @importFrom matrixNormal is.positive.definite
#' @noRd
#'
checkTmvArgs <- function(mean, sigma, lower, upper) {
  if (is.null(lower) || any(is.na(lower)))
    stop("Error: ", sQuote("lower"), "is not specified or contains NA", call. = FALSE)
  if (is.null(upper) || any(is.na(upper)))
    stop("Error: ", sQuote("upper"), "is not specified or contains NA", call. = FALSE)
  if (!is.numeric(mean) || !is.vector(mean))
    stop("Error: ", sQuote("mean"), " is not a numeric vector", call. = FALSE)
  if (is.null(sigma) || any(is.na(sigma)))
    stop("Error: ", sQuote("sigma"), "is not specified or contains NA", call. = FALSE)
  if (!is.matrix(sigma)) {
    sigma <- as.matrix(sigma)
  }
  if (NCOL(lower) != NCOL(upper)) {
    stop("Error: lower and upper have non-conforming size", call. = FALSE)
  }
  if (!matrixNormal::is.positive.definite(sigma)) {
    # second argument. tol = .Machine$double.eps^0.5))
    stop("Error: sigma is not positive definite. Perhaps raise tolerance to check positive definiteness.", call. = FALSE)
  }
  if (length(mean) != NROW(sigma)) {
    stop("Error: mean and sigma have non-conforming size", call. = FALSE)
  }
  if (length(lower) != length(mean) || length(upper) != length(mean)) {
    stop("Error: mean, lower and upper must have the same length", call. = FALSE)
  }
  if (any(lower >= upper)) {
    stop("Error: lower must be smaller than or equal to upper (lower<=upper)", call. = FALSE)
  }
  cargs <- list(mean = mean, sigma = sigma, lower = lower,
    upper = upper)
  return(cargs)
}

## Check if lower bound is less than upper bound
# if(length(lower) != length(mean))
# if(lower > mean)
# stop("lower is greater than mean vector. Please adjust.")
#  if(mean  > upper)
#  stop("mean is greater than upper vector. Please adjust mean or upper.")
# if(length(lower) != length(upper))
#  stop("lower must have the same length as upper.")
#
