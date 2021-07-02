# ~~~~~~~~ Structure constructors

new_UCB <- function(K,
                    H,
                    visitor_reward,
                    choices,
                    rewards,
                    time,
                    S,
                    proba,
                    alpha,
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
    ...,
    class = c(class, "UCB"))
}

empty_UCB <- function(K, alpha, ..., class = character()) {
  empty_FMA(
    K = K,
    proba = vector(),
    alpha = alpha,
    ...,
    class = c(class, "UCB")
  )
}

validate_UCB <- function(bandit) {
  stopifnot(inherits(bandit, what = c("UCB")))
  # TODO
  validate_FMA(bandit)
}
