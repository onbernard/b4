# ~~~~~~~~~~~

# Generic is BASE_helpers

#' proba getter method
#'
#' @description Returns the proba history vector pertaining to a LINUCB bandit
#'   instance
#'
#' @param bandit A LINUCB bandit
#'
#' @export
b4_proba.LINUCB <- function(bandit) {
  validate_LINUCB(bandit)
  bandit$proba
}

# ~~~~~~~~~~~

# TODO

#' Printing LINUCB bandit
#'
#' @param x A LINUCB bandit object
#' @param ... Additional print arguments
#'
#' @export
print.LINUCB <- function(x, ...) {
  print("LOL LINUCB")
}

# ~~~~~~~~~~~

# Generic is in BASE_helpers

#' LINUCB bandit parameters
#'
#' @description Returns a list of the LINUCB policy parameters supplied when
#'   instantiating a LINUCB bandit object. Includes :
#' \itemize{
#'   \item{alpha}{ Exploration parameter of the LINUCB algorithm}
#' }
#'
#' @param bandit A LINUCB bandit
#'
#' @export
b4_bandit_parameters.LINUCB <- function(bandit) {
  validate_LINUCB(bandit)
  return(c(list(alpha = bandit$alpha), NextMethod()))
}

# ~~~~~~~~~~~
