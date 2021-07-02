# ~~~~~~~~ Structure constructors

new_EPSG <- function(K,
                     H,
                     visitor_reward,
                     choices,
                     rewards,
                     time,
                     S,
                     epsilon,
                     ...,
                     class = character()) {
  new_FMA(
    K = K,
    H = H,
    visitor_reward = visitor_reward,
    choices = choices,
    rewards = rewards,
    time = time,
    S = S,
    epsilon = epsilon,
    ...,
    class = c(class, "EPSG"))
}

empty_EPSG <- function(K, epsilon, ..., class = character()) {
  empty_FMA(K = K,
            epsilon = epsilon,
            ...,
            class = c(class, "EPSG"))
}

validate_EPSG <- function(bandit) {
  stopifnot(inherits(bandit, what = c("EPSG")))
  # TODO
  validate_FMA(bandit)
}
