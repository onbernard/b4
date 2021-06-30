# ~~~~~~~~~~~ B4FMA getters

#' @export
B4FMA_get_visitor_reward <- function(bandit) {
  validate_B4FMA(bandit)
  bandit$visitor_reward
}

#' @export
B4FMA_get_choice <- function(bandit) {
  validate_B4FMA(bandit)
  bandit$choice
}

#' @export
B4FMA_get_S <- function(bandit) {
  validate_B4FMA(bandit)
  bandit$S
}

#' @export
B4FMA_get_time <- function(bandit) {
  validate_B4FMA(bandit)
  bandit$time
}

#' @export
B4FMA_get_horizon <- function(bandit) {
  validate_B4FMA(bandit)
  bandit$H
}

#' @export
B4FMA_get_K <- function(bandit) {
  validate_B4FMA(bandit)
  bandit$K
}
