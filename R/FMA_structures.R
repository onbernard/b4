# ~~~~~~~~ Structure constructors

new_FMA <- function(K,
                    H,
                    visitor_reward,
                    choices,
                    rewards,
                    time,
                    S,
                    ...,
                    class = character()) {
  new_BASE(
    K = K,
    H = H,
    visitor_reward = visitor_reward,
    choices = choices,
    rewards = rewards,
    time = time,
    S = S,
    ...,
    class = c(class, "FMA"))
}

empty_FMA <- function(K, ..., class = character()) {
  empty_BASE(K, S = b4_generate_S(K), ..., class = c(class, "FMA"))
}

validate_FMA <- function(bandit) {
  stopifnot(inherits(bandit, what = c("FMA")))
  # TODO
  validate_BASE(bandit)
}
