# ~~~~~~~~~~~

# Generic is in BASE_helpers

#' @export
b4_proba.UCB <- function(bandit) {
  validate_UCB(bandit)
  bandit$proba
}

# ~~~~~~~~~~~

# Generic is in BASE_helpers

#' @export
b4_bandit_parameters.UCB <- function(bandit) {
  validate_UCB(bandit)
  return(c(list(alpha=bandit$alpha), NextMethod()))
}

# ~~~~~~~~~~~

#' @export
print.UCB <- function(x, ...) { # TODO
  print("lol UCB")
}
