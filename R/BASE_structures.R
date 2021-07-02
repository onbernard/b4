# ~~~~~~~~ BASE constructors

#' BASE bandit object constructor
#'
#' @description BASE acts like an abstract class and serves as a foundation for
#'   actual implementation. Their purpose is to store policy parameters,
#'   regression variables and iteration history in a list.
#'
#'   @param K number of arms
#'   @param H horizon, aka number of observation
#'   @param visitor_reward matrix
#'   @param choices choice history
#'   @param rewards reward history
#'   @param time elapsed time during policy iterations
#'   @param ... additional slots for class extension
#'   @param class class extension names
#'
new_BASE <- function(K,
                     H,
                     visitor_reward,
                     choices,
                     rewards,
                     time,
                     ...,
                     class = character()) {
  structure(
    list(
      K = K,
      H = H,
      visitor_reward = visitor_reward,
      choices = choices,
      rewards = rewards,
      time = time,
      ...
    ),
    class = c(class, "BASE")
  )
}

#' BASE bandit object helper
#'
#' @description Initiates a BASE object
#'
#' @param K number of arms
#' @param ... additional slots for class extension
#' @param class class extension names
#'
empty_BASE <- function(K, ..., class) {
  new_BASE(K = K,
           H = 0,
           visitor_reward = matrix(0,nrow = 0, ncol = K),
           choices = vector(),
           rewards = vector(),
           time = 0,
           ...,
           class = class)
}

#' BASE bandit object validator
#'
#' @description Checks the class value and whether its slot are valid
#'
#' @param bandit A BASE bandit object
#'
validate_BASE <- function(bandit) {
  stopifnot(inherits(bandit, what = c("BASE")))
  # TODO
}
