# ~~~~~~~~~~~ B4UCB getters

#' @export
B4UCB_get_alpha <- function(bandit, alpha = NULL) {
  validate_B4UCB(bandit)
  if (!is.null(alpha)) {
    stopifnot(is.double(alpha))
    bandit$alpha <- alpha
  }
  bandit$alpha
}

# ~~~~~~~~~~~

#' @export
B4UCB_get_probamax <- function(bandit) {
  validate_B4UCB(bandit)
  bandit$probamax
}
