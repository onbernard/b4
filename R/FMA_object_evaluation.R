# ~~~~~~~~ FMA object evaluation

#' @export
B4FMA_cumulative_regret <- function(bandit) {
  UseMethod("B4FMA_cumulative_regret")
}

#' @export
B4FMA_cumulative_regret.B4FMA <- function(bandit) {
  # TODO
}

# ~~~~~~~~

#' @export
B4FMA_regret <- function(bandit) {
  UseMethod("B4FMA_regret")
}

#' @export
B4FMA_regret.B4FMA <- function(bandit) {
  # TODO
}

# ~~~~~~~~

#' @export
B4FMA_mu <- function(bandit) {
  UseMethod("B4FMA_mu")
}

#' @export
B4FMA_mu.B4FMA <- function(bandit) {
  # TODO
}

# ~~~~~~~~

#' @export
B4FMA_mu_hat <- function(bandit) {
  UseMethod("B4FMA_mu_hat")
}

#' @export
B4FMA_mu_hat.B4FMA <- function(bandit) {
  # TODO
}

# ~~~~~~~~

B4FMA_simulate <- function(x, reward) {
  UseMethod("B4FMA_simulate", x)
}

# You will find methods for fma_simulate in fma_pol files
