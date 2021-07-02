#'UCB algorithm
#'
#'Generate a matrix to save the results (S).
#' \itemize{ At each iteration
#'  \item Calculates the arm probabilities
#'  \item Choose the arm with the maximum upper bound (with alpha parameter)
#'  \item Receives a reward in visitor_reward for the arm and associated iteration
#'  \item Updates the results matrix S.uire \code{\link{tic}} and \code{\link{toc}} from \code{\link{tictoc}} library
#'}
#'@param visitor_reward Dataframe of integer or numeric values
#'@param alpha Numeric value (optional)
#'
#'
#'@examples
#'## Generates 10000 numbers from 2 binomial  distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame(cbind(K1,K2) )
#'#Run UCB algorithm
#'ucb_alloc  <- b4_UCB(visitor_reward,alpha = 10)
#'@export
b4_UCB <- function(visitor_reward, alpha = 1, bandit = NULL) {
  stopifnot(is.double(alpha))

  visitor_reward <- check_visitor_reward(visitor_reward)

  if (is.null(bandit)) { # Create new UCB object
    bandit <- empty_UCB(K = ncol(visitor_reward),
                        alpha = alpha)
  }
  else {
    validate_UCB(bandit) # Check validity of supplied object
  }

  if (ncol(visitor_reward)!= b4_K(bandit)) { # Check compatibility
    stop(
      "visitor reward and bandit are not compatible : different number of arms",
      call. = FALSE
    )
  }

  FMA_simulate(bandit, visitor_reward)
}
