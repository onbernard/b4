# ~~~~~~~~ Structure constructors

new_TS <- function(K,
                   H,
                   visitor_reward,
                   choices,
                   rewards,
                   time,
                   S,
                   proba,
                   alpha,
                   beta,
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
    proba = proba,
    alpha = alpha,
    beta = beta,
    ...,
    class = c(class, "TS")
  )
}

empty_TS <- function(K, alpha, beta, ..., class = character()) {
  empty_FMA(
    K = K,
    proba = vector(),
    alpha = alpha,
    beta = beta,
    ...,
    class = c(class, "TS")
  )
}

validate_TS <- function(bandit) {
  stopifnot(inherits(bandit, what = c("TS")))
  # TODO
  validate_FMA(bandit)
}
