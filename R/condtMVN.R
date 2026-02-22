#' @title Conditional Truncated Multivariate Normal Parameters

#' @description Suppose that **Z = (X,Y)** is from a fully-joint multivariate normal distribution of dimension *n* with \code{mean} and covariance matrix \code{sigma} truncated between \code{lower} and \code{upper}. This function provides the parameters for the conditional mean and covariance matrix of **Y** given **X**. See the vignette for more information.

#' @seealso \code{\link[condMVNorm]{cmvnorm}}, \code{\link[mvtnorm]{pmvnorm}}, \code{\link[mvtnorm]{Mvnorm}}

#' @param mean the mean vector for Z of length of n
#' @param sigma the symmetric and positive-definite covariance matrix of dimension n x n of Z.
#' @param lower a vector of lower bounds of length n that truncate Z
#' @param upper a vector of upper bounds of length n that truncate Z
#' @inheritParams condMVNorm::condMVN
#' @param init initial value used for random generation of truncated multivariate normal in a Gibbs sampler. Default: A vector of zeros, equal to the number of components. For details, see **tmvmixnorm::**\code{\link[tmvmixnorm]{rtmvn}}().

#' @return Returns a list of: \itemize{
#'  \item condMean - conditional mean of **Y|X**
#'  \item condVar - conditional variance of **Y|X**
#'  \item condLower - the lower bound of **Y|X**
#'  \item condUpper - the upper bound of **Y|X**
#'  \item condInit - the initial values adjusted to match the dimension of **Y|X**. These are used to randomly generate the truncated multivariate normal \link{rcmvtruncnorm}.
#'  }
#'
#' @importFrom condMVNorm condMVN
#' @importFrom matrixNormal is.positive.definite
#' @export
#'
#' @details
#'
#' The first four arguments are the parameters of multivariate normal and the truncation space. \code{dependent.ind, given.ind, X.given, init} are all arguments that determines the conditional truncated MVN.
#'
#' Using the full data **Z**, the conditional mean and conditional variance of **Y|X** are determined (Wang, 2006). Additionally, to reflect the reduced dimension of **Y|X**, the truncation limits are also adjusted.
#'
#' See the vignette for more information.
#'
#' @note
#' This function is based on \code{\link[condMVNorm]{condMVN}} from the **condMVNorm** package.
#'
#' @references
#' Wang, R. 2006. Marginal and conditional distributions of multivariate normal distribution. \url{http://www.ccs.neu.edu/home/vip/teach/MLcourse/3_generative_models/lecture_notes}.
#'
#' @examples
#' # Suppose X2,X3,X5|X2,X4 ~ N_3(1, Sigma) and truncated between -10 and 10.
#' d <- 5
#' rho <- 0.9
#' Sigma <- matrix(0, nrow = d, ncol = d)
#' Sigma <- rho^abs(row(Sigma) - col(Sigma))
#'
#' # Conditional Truncated Normal Parameters
#' condtMVN(mean = rep(1, d),
#'   sigma = Sigma,
#'   lower = rep(-10, d),
#'   upper = rep(10, d),
#'   dependent.ind = c(2, 3, 5),
#'   given.ind = c(1, 4), X.given = c(1, -1)
#' )
condtMVN <- function(
                     # Parameters of truncated Multivariate Normal
                     mean,
                     sigma,
                     lower,
                     upper,
                     # How to condition the truncated MVN
                     dependent.ind,
                     given.ind, X.given,
                     init = rep(0, length(mean))
) {

  # Check parameters of distribution
  cargs <- checkTmvArgs(mean, sigma, lower, upper)
  mean <- cargs$mean
  sigma <- cargs$sigma
  lower <- cargs$lower
  upper <- cargs$upper

  ### Dependent and Given Indeces
  ## If there are no dependent indeces, there's nothing to do.
  if (length(dependent.ind) == 0) {
    stop("You must specify the indices of dependent random
           variables in `dependent.ind'", call. = FALSE, immediate. = TRUE)
  }
  ## Not Condition on Anything: Truncated Multivariate Normal.
  if (length(given.ind) == 0 | (missing(given.ind) & missing(X.given))) {
    # eqv to if(length(dependent.ind) == length(mean))
    warning("There is nothing to condition X on.
              Returning unconditional variables",
      call. = FALSE, immediate. = TRUE)
    return(list(
      condMean  = mean,
      condSigma = sigma,
      condLower = lower,
      condUpper = upper,
      condInit = init
    ))
  }
  ## Length
  ind <- sort(c(dependent.ind, given.ind))
  if (length(ind) < length(mean))
    warning("You are conditioning on fewer variables than n. Check `dependent.ind` or `given.ind`.", call. = FALSE, immediate. = TRUE)

  ## Warning: if sampling and conditioning on same variable. Probability will be 1.
  if (any(dependent.ind %in% given.ind)) {
    warning("You are conditioning in X", ind[duplicated(ind)], "
         the same random variable in Y. ",
      "Did you mean to remove this variable from X or Y?",
      call. = FALSE, immediate. = TRUE
    )
  }

  # Check Initial Values
  # if(length(init) != length(mean)){
  #  stop("Error: The initial values do not match the conditional distribution")
  # }

  l1 <-   condMVNorm::condMVN(
    mean = mean,
    sigma = sigma,
    dependent.ind = dependent.ind,
    given.ind = given.ind,  X.given = X.given,
    check.sigma = FALSE
  )
  # check.sigma does not need to be true; Sigma is already checked in checkTmvArgs().
  l2 <- list(
    condLower = lower[dependent.ind],
    condUpper = upper[dependent.ind],
    condInit = init[dependent.ind]
  )
  params <- c(l1, l2)

  # Check if Conditional Sigma is positive definite
  if (!isTRUE(matrixNormal::is.positive.definite(params$condVar)))
    warning("#> The conditional is not positive-definite. Huh? Investigate! \n",
      call. = FALSE, immediate. = TRUE
    )

  return(params)
}



# #' mcmc.simulate A list of two elements to simulate Y given X in the MCMC chain from the truncated multivariate normal distribution. May need to change if issues with sampling. \itemize{
# #'      \item chain.length.impute The length of the chain used in Gibbs sampler. Passed to burn in \code{\link[tmvmixnorm]{rtmvn}}.Default: 80
