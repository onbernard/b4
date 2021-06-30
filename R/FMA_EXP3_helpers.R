# ~~~~~~~~~~~ B4EXP3 getters

#' @export
B4EXP3_get_gamma <- function(bandit, gamma = NULL) {
  validate_B4EXP3(bandit)
  if (!is.null(gamma)) {
    stopifnot(is.double(gamma))
    bandit$gamma <- gamma
  }
  bandit$gamma
}

#' @export
B4EXP3_get_weight <- function(bandit) {
  validate_B4EXP3(bandit)
  bandit$weight
}

#' @export
B4EXP3_get_reward <- function(bandit) {
  validate_B4EXP3(bandit)
  bandit$reward
}

#' @export
B4EXP3_get_estimated_reward <- function(bandit) {
  validate_B4EXP3(bandit)
  bandit$estimated_reward
}

#' @export
B4EXP3_get_prob <- function(bandit) {
  validate_B4EXP3(bandit)
  bandit$prob
}
