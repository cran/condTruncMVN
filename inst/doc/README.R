## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,  # If TRUE, all output would be in the code chunk.
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%",
  results = "markup",
  prompt = TRUE,
  strip.white = TRUE,
  tidy = TRUE,
  tidy.opts = list(width.cutoff = 90), # options for tidy to remove blank lines [blank = FALSE] and set the approximate line width to be 80.
  fig.show = "asis",
  fig.height = 4.5,  # inches
  fig.width = 4.5   # inches
)
library("formatR")

## -----------------------------------------------------------------------------
library("condTruncMVN")

## ---- include=FALSE-----------------------------------------------------------
# install.packages("devtools")
#  devtools::install_github("phargarten2/ggplot2")

## ----example------------------------------------------------------------------
library(condTruncMVN)
d <- 5
rho <- 0.9
Sigma <- matrix(0, nrow = d, ncol = d)
Sigma <- rho^abs(row(Sigma) - col(Sigma))
Sigma

## -----------------------------------------------------------------------------
condtMVN(mean  = rep(1, d),
  sigma = Sigma,
  lower = rep(-10, d),
  upper = rep(10, d),
  dependent.ind = c(2, 3, 5),
  given.ind = c(1, 4), X.given = c(1, -1)
)

## -----------------------------------------------------------------------------
dcmvtruncnorm(
  rep(0, 3),
  mean  = rep(1, 5),
  sigma = Sigma,
  lower = rep(-10, 5),
  upper = rep(10, d),
  dependent.ind = c(2, 3, 5),
  given.ind = c(1, 4), X.given = c(1, -1),
  log = TRUE
)

## -----------------------------------------------------------------------------
pcmvtruncnorm(
  rep(-0.5, 3), rep(0, 3),
  mean = rep(1, d),
  sigma = Sigma,
  lower = rep(-10, d),
  upper = rep(10, d),
  dependent.ind = c(2, 3, 5),
  given.ind = c(1, 4), X.given = c(1, -1)
)

## -----------------------------------------------------------------------------
set.seed(2342)
rcmvtruncnorm(2,
  mean = rep(1, d),
  sigma = Sigma,
  lower = rep(-10, d),
  upper = rep(10, d),
  dependent.ind = c(2, 3, 5),
  given.ind = c(1, 4), X.given = c(1, -1)
)

## -----------------------------------------------------------------------------
pcmvtruncnorm(-0.5, 0,
  mean  = rep(1, d),
  sigma = Sigma,
  lower = rep(-10, d),
  upper = rep(10, d),
  dependent.ind = 1,
  given.ind = 2:5, X.given = c(1, -1, 1, -1)
)

## -----------------------------------------------------------------------------
set.seed(2342)
rcmvtruncnorm(2,
  mean  = rep(1, d),
  sigma = Sigma,
  lower = rep(-10, d),
  upper = rep(10, d),
  dependent.ind = 1,
  given.ind = 2:5, X.given = c(1, -1, 1, -1)
)

## ----echo=FALSE---------------------------------------------------------------
# sessioninfo::session_info() # makes a mess! Instead

cat(" -- Session info ---------------------------------------------------")
sessioninfo::platform_info()
cat("--  Packages -------------------------------------------------------")
tmp.df <- sessioninfo::package_info(
  c("condMVNorm", "matrixNormal", "tmvmixnorm", "tmvtnorm", "truncnorm"),
  dependencies = FALSE
)
print(tmp.df)

