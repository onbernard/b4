# ~~~~~~~~ Structure constructors


new_CFMA <- function(K,
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
                     ...,
                     class = character()) {
  new_BASE(
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
    ...,
    class = c(class, "CFMA"))
}

empty_CFMA <- function(K, n_f, booleanReward, ..., class) {
  empty_BASE(
    K = K,
    dt = matrix(0, nrow = 0, ncol = n_f),
    n_f = n_f,
    b = matrix(0, nrow = K, ncol = n_f),
    A = array(diag(n_f), c(n_f, n_f, K)),
    th_hat = array(0, dim = c(K, n_f)),
    booleanReward = booleanReward,
    ...,
    class = c(class, "CFMA")
  )
}

validate_CFMA <- function(bandit) {
  stopifnot(inherits(bandit, what = c("CFMA")))
  # TODO
  validate_BASE(bandit)
}
