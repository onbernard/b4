#'EpsilonGreedy algorithm
#'
#'Generate a matrix to save the results (S).
#'At each iteration play the best arm with a probability of 1-epsilon and
#'other arm with probability epsilon
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param epsilon Numeric value (optional)
#'
#'@examples
#'## Generates 10000 numbers from 2 binomial  distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'#Run epsilon Greedy algorithm
#'epsilon_greedy_alloc  <- b4_EpsilonGreedy(visitor_reward,epsilon  = 0.25)
#'@export
b4_EpsilonGreedy <- function(visitor_reward, epsilon = 0.25, bandit = NULL) {
  stopifnot(is.double(epsilon))

  visitor_reward <- check_visitor_reward(visitor_reward)
  K <- ncol(visitor_reward)

  if (is.null(bandit)) { # Create new B4EXP3 object
    bandit <- empty_EPSG(K = K, epsilon  = epsilon)
  }
  else {
    validate_EPSG(bandit) # Check validity of supplied object
  }

  if (ncol(visitor_reward)!= b4_K(bandit)) { # Check compatibility
    stop(
      "visitor reward and bandit are not compatible : different number of arms",
      call. = FALSE
    )
  }

  FMA_simulate(bandit, visitor_reward)
}
