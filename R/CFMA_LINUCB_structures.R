# ~~~~~~~~ Structure constructors

new_LINUCB <- function(K,
                       H,
                       n_f,
                       dt,
                       visitor_reward,
                       booleanReward,
                       b,
                       A,
                       th_hat,
                       choices,
                       rewards,
                       time,
                       proba,
                       alpha,
                       ...,
                       class = character()) {
  new_CFMA(
    K = K,
    H = H,
    visitor_reward = visitor_reward,
    choices = choices,
    rewards = rewards,
    time = time,
    dt = dt,
    n_f = n_f,
    b = b,
    A = A,
    th_hat = th_hat,
    booleanReward = booleanReward,
    proba = proba,
    alpha = alpha,
    ...,
    class = c(class, "LINUCB")
  )
}

empty_LINUCB <- function(K, n_f, booleanReward, alpha, ..., class=character()) {
  empty_CFMA(
    K = K,
    n_f = n_f,
    proba = vector(),
    booleanReward = booleanReward,
    alpha = alpha,
    ...,
    class = c(class, "LINUCB")
  )
}

validate_LINUCB <- function(bandit) {
  stopifnot(inherits(bandit, what = c("LINUCB")))
  # TODO
  validate_CFMA(bandit)
}
