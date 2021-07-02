# ~~~~~~~~~~~ TS getters

# Generic is defined in FMA_helpers

#' @export
b4_proba.TS <- function(bandit) {
  validate_TS(bandit)
  bandit$proba
}

# ~~~~~~~~~~~

# Generic is in BASE superclass

#' @export
b4_bandit_parameters.TS <- function(bandit) {
  validate_TS(bandit)
  return(c(list(alpha=bandit$alpha, beta=bandit$beta), NextMethod()))
}

# ~~~~~~~~~~~

#' @export
print.TS <- function(x, ...) { # TODO
  print("LOL TS")
}
