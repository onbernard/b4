# ~~~~~~~~~~~ FMA helpers

# ~~~~~~~~~~~ getters

#' Return the S slot of a FMA bandit object
#'
#' @param bandit A FMA bandit object
#' @export
b4_S <- function(bandit) {
  UseMethod("b4_S")
}

#' @export
b4_S.FMA <- function(bandit) {
  validate_FMA(bandit)
  bandit$S
}

# ~~~~~~~~~~~

#' Generic is in BASE superclass

b4_bandit_parameters.FMA <- function(bandit) {
  validate_FMA(bandit)
  return(c(list(), NextMethod()))
}
