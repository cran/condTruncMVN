context("Testing dcmvtrunnorm")
message("No need to check. All checking is done either in condtMVN or in tmvtnorm package. The tmvtnorm package does a good job checking arguments.")

### Running goodpractice(), the ability to find multivariate densities does not need to be tested for reason above.
# dens <- tmvtnorm::dtmvnorm(
#   y,
#   mean = ret$condMean,
#   sigma = ret$condVar,
#   lower = ret$condLower,
#   upper = ret$condUpper,
#   log = log,
#   margin = NULL
# )

# From GoodPractice, This test is too simple to check.
# if (length(y) != length(dependent.ind))
#    stop("lengths of `y' and `dependent.ind' must be same")
