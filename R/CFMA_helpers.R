# ~~~~~~~~~~~ CFMA HELPERS

#' Bandit context
#'
#' @description Returns the context matrix of a bandit object
#'
#' @param bandit A CFMA bandit
#'
#' @export
b4_dt <- function(bandit) {
  UseMethod("b4_dt")
}

#' @export
b4_dt.CFMA <- function(bandit) {
  validate_CFMA(bandit)
  bandit$dt
}

# ~~~~~~~~~~~

#' Contextual dimension
#'
#' @description Returns the dimension of a CFMA bandit object, i.e. the number
#'   of columns in the dt matrix
#'
#' @param bandit A CFMA bandit
#'
#' @export
b4_n_f <- function(bandit) {
  UseMethod("b4_n_f")
}

#' @export
b4_n_f.CFMA <- function(bandit) {
  validate_CFMA(bandit)
  bandit$n_f
}

# ~~~~~~~~~~~

#' Affine component of CFMA regression
#'
#' @description Returns the b matrix of the regression pertaining to a
#'   contextual bandit
#'
#' @param bandit A CFMA bandit
#'
#' @export
b4_b <- function(bandit) {
  UseMethod("b4_b")
}

#' @export
b4_b.CFMA <- function(bandit) {
  validate_CFMA(bandit)
  bandit$b
}

# ~~~~~~~~~~~

#' Linear component of CFMA regression
#'
#' @description Returns the A matrix of the regression pertaining to a
#'   contextual bandit
#'
#' @param bandit A CFMA bandit
#'
#' @export
b4_A<- function(bandit) {
  UseMethod("b4_A")
}

#' @export
b4_A.CFMA <- function(bandit) {
  validate_CFMA(bandit)
  bandit$A
}

# ~~~~~~~~~~~

#' Estimated reward expectation of a CFMA bandit
#'
#' @description Returns the theta_hat matrix of a bandit object
#'
#' @param bandit A CFMA bandit
#'
#' @export
b4_th_hat <- function(bandit) {
  UseMethod("b4_th_hat")
}

#' @export
b4_th_hat.CFMA <- function(bandit) {
  validate_CFMA(bandit)
  bandit$th_hat
}

# ~~~~~~~~~~~

#' booleanReward getter
#'
#' @description Returns wheter the visitor rewards are boolean values as
#'   specified when instantiating a CFMA bandit
#'
#' @param bandit A CFMA bandit
#'
#' @export
b4_booleanReward <- function(bandit) {
  UseMethod("b4_booleanReward")
}

#' @export
b4_booleanReward.CFMA <- function(bandit) {
  validate_CFMA(bandit)
  bandit$booleanReward
}

# ~~~~~~~~~~~

# Generic is in BASE superclass

b4_bandit_parameters.CFMA <- function(bandit) {
  validate_CFMA(bandit)
  return(c(list(), NextMethod()))
}
