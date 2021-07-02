# ~~~~~~~~~~~ EXP3 getters

#' @export
print.EXP3 <- function(x, ...) {
  print("LOL EXP3")
}

# ~~~~~~~~~~~

# Generic is in BASE superclass

#' @export
b4_bandit_parameters.EXP3 <- function(bandit) {
  validate_EXP3(bandit)
  return(c(list(gamma = bandit$gamma,
                weight = bandit$weight,
                estimated_reward = bandit$estimated_reward,
                prob = bandit$prob), NextMethod()))
}

# ~~~~~~~~~~~
