# ~~~~~~~~~~~ BASE HELPERS

#' Returns the number of arms of a bandit object
#'
#' @param bandit A bandit of any class
#'
#' @export
b4_K <- function(bandit) {
  UseMethod("b4_K")
}

#' @export
b4_K.BASE <- function(bandit) {
  validate_BASE(bandit)
  bandit$K
}

# ~~~~~~~~~~~

#' Returns the visitor reward matrix of a bandit object
#'
#' @param bandit A bandit of any class

#' @export
b4_visitor_reward <- function(bandit) {
  UseMethod("b4_visitor_reward")
}

#' @export
b4_visitor_reward.BASE <- function(bandit) {
  validate_BASE(bandit)
  bandit$visitor_reward
}

# ~~~~~~~~~~~

#' Returns the choice history of a bandit object
#'
#' @param bandit A bandit of any class
#'
#' @export
b4_choices <- function(bandit) {
  UseMethod("b4_choices")
}

#' @export
b4_choices.BASE <- function(bandit) {
  validate_BASE(bandit)
  bandit$choices
}

# ~~~~~~~~~~~

#' Returns the proba slot of a bandit object
#'
#' @param bandit A bandit of a class that implements
#'
#' @return A vector whose meaning depends on the bandit class. Not all classes
#' uses such a slot.
#'
#' @export
b4_proba <- function(bandit) {
  UseMethod("b4_proba")
}

# Methods are implemented, or not, in subclasses

# ~~~~~~~~~~~

#' Returns the elapsed time during the iteration of the policy
#'
#' @param bandit A bandit of any class
#'
#' @return An object of type "proc_time". See also
#' \code{\link[base]{system.time}}
#'
#' @export
b4_time <- function(bandit) {
  UseMethod("b4_time")
}

#' @export
b4_time.BASE <- function(bandit) {
  validate_BASE(bandit)
  bandit$time
}

# ~~~~~~~~~~~

#' Returns the number of observation in the visitor reward of any bandit
#'
#' @param bandit A bandit of any class
#'
#' @export
b4_horizon <- function(bandit) {
  UseMethod("b4_horizon")
}

#' @export
b4_horizon.BASE <- function(bandit) {
  validate_BASE(bandit)
  bandit$H
}

# ~~~~~~~~~~~

#' Returns the reward history of a bandit object
#'
#' @param bandit A bandit of any class
#'
#' @export
b4_rewards <- function(bandit) {
  UseMethod("b4_rewards")
}

#' @export
b4_rewards.BASE <- function(bandit) {
  validate_BASE(bandit)
  bandit$rewards
}

# ~~~~~~~~~~~

#' Returns the specific policy parameters of a bandit object
#'
#' @param bandit A bandit of any class
#'
#' @return A list containing the policy parameters. These change depending on
#' the specific policy class
#'
#' @export
b4_bandit_parameters <- function(bandit) {
  UseMethod("b4_bandit_parameters")
}

#' @export
b4_bandit_parameters.BASE <- function(bandit) {
  validate_BASE(bandit)
  return(list())
}
