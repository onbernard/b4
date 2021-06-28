# Where I keep my constructor helpers

B4ucb <- function(visitor_reward, alpha = 1) {
  stopifnot(is.data.frame(visitor_reward))
  stopifnot(is.double(alpha))

  visitor_reward <- as_tibble(visitor_reward)

  fma_simulate(structure(
    visitor_reward = visitor_reward,
    alpha = alpha,
    S,
    choice,
    proba,
    time,
    mu_hat,
    mu,
    class = c("B4ucb", "B4fma")
  ))
}

B4exp3 <- function(visitor_reward, gamma = 0.05) {
  stopifnot(is.data.frame(visitor_reward))
  stopifnot(is.double(gamma))

  visitor_reward <- as_tibble(visitor_reward)

  fma_simulate(structure(
    visitor_reward,
    gamma = gamma,
    S,
    choice,
    proba,
    time,
    mu_hat,
    mu,
    class = c("B4exp3", "B4fma")
  ))
}


B4epsilon_greedy <- function(visitor_reward, epsilon = 0.25) {
  stopifnot(is.data.frame(visitor_reward))
  stopifnot(is.double(epsilon))

  visitor_reward <- as_tibble(visitor_reward)

  fma_simulate(structure(
    visitor_reward,
    epsilon = epsilon,
    S,
    choice,
    proba,
    time,
    mu_hat,
    mu,
    class = c("B4eps", "B4fma")
  ))
}

B4thompson_sampling <- function(visitor_reward, alpha = 1, beta = 1) {
  stopifnot(is.data.frame(visitor_reward))
  stopifnot(is.double(alpha))
  stopifnot(is.double(beta))

  visitor_reward <- as_tibble(visitor_reward)

  fma_simulate(structure(
    visitor_reward,
    alpha = alpha,
    beta = beta,
    S,
    choice,
    proba,
    time,
    mu_hat,
    mu,
    class = c("B4ts", "B4fma")
    ))
}
