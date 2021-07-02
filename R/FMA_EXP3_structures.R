# ~~~~~~~~ Structure constructors

new_EXP3 <- function(K,
                     H,
                     visitor_reward,
                     choices,
                     rewards,
                     time,
                     S,
                     gamma,
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
    gamma = gamma,
    ...,
    class = c(class, "EXP3")
  )
}

empty_EXP3 <- function(K, gamma, ..., class = character()) {
  empty_FMA(K = K,
            weight = rep(1, K),
            estimated_reward = vector(),
            prob = rep(0, K),
            gamma = gamma,
            ...,
            class = c(class, "EXP3"))
}

validate_EXP3 <- function(bandit) {
  stopifnot(inherits(bandit, what = c("EXP3")))
  # TODO
  validate_FMA(bandit)
}
