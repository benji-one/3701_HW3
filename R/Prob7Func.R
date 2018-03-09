#' Finds the MLE for a generic data distribution.
#'
#'@param x A data set
#'@param interval A vector interval
#'@param FUN A distribution function
#'@return The MLE of the parameter theta
#'
#'@examples
#'gamma.FUN <- function(theta, x) dgamma(x, shape = theta, log = TRUE)
#'cauchy.FUN <- function(theta, x) dcauchy(x, location = theta, log = TRUE)
#'binom.FUN <- function(theta, x) dbinom(x, 20, prob = 1 / (1 + exp(- theta)), log = TRUE)
#'
#'xgam <- load("data/xgam.rda")
#'xcau <- load("data/xcau.rda")
#'xbin <- load("data/xbin.rda")
#'interval <- 1:100
#'
#'mle(xgam,interval,gamma.FUN)
#'mle(xcau,interval,cauchy.FUN)
#'mle(xbin,interval,binom.FUN)

mle <- function(x,interval,FUN)
{
  theta <- pi

  log1 <- function(theta,x)
  {
    sum(FUN(theta,x))
  }

  return(optimize(log1,maximum = TRUE, interval,x=x)$maximum)
}

gamma.FUN <- function(theta, x) dgamma(x, shape = theta, log = TRUE)
cauchy.FUN <- function(theta, x) dcauchy(x, location = theta, log = TRUE)
binom.FUN <- function(theta, x) dbinom(x, 20, prob = 1 / (1 + exp(- theta)), log = TRUE)
