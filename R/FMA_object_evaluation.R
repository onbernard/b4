# ~~~~~~~~ FMA object evaluation

#' Returns the actual FMA reward expectation vector based on visitor reward
#'
#' @param bandit A FMA bandit object
#'
#' @export
b4_mu <- function(bandit) {
  UseMethod("b4_mu")
}

#' @export
b4_mu.FMA <- function(bandit) {
  validate_FMA(bandit)
  return(colMeans(b4_visitor_reward(bandit)))
}

# ~~~~~~~~

#' Returns the estimated FMA reward expectation vector
#'
#' @param bandit A FMA bandit object
#'
#' @export
b4_mu_hat <- function(bandit) {
  UseMethod("b4_mu_hat")
}

#' @export
b4_mu_hat.FMA <- function(bandit) {
  validate_FMA(bandit)
  return(b4_S(bandit)[1,])
}

# ~~~~~~~~~~~~~~~~

#' Generic for all FMA algorithm methods
#'
#' @param bandit A bandit object on which to append new observations
#' @param visitor_reward visitor reward matrix. New observations
#'
FMA_simulate <- function(bandit, visitor_reward) {
  UseMethod("FMA_simulate", bandit)
}

# FMA_simulate methods implemented in subclass_policies files

# ~~~~~~~~~~~~~~~~
