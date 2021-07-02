#'ThompsonSampling
#'
#'A thompson sampling (TS) bandit strategy implemented by sampling, in each round, averages from a posterior
#'distribution  \code{\link{ConditionForThompsonSampling}}, and choosing the action that maximizes the expected reward given the
#'sampled average. Conceptually, this means that the player instantiates their beliefs
#'randomly in each round, and then acts optimally according to them.
#'Generate a matrix to save the results (S).
#' \itemize{ At each iteration
#'  \item Sample an averages from a posterior in S for each arm (beta distribution with alpha and beta parameters)
#'  \item Choose the arm with the highest average
#'  \item Receives a reward in visitor_reward for the arm and associated iteration
#'  \item Updates the results matrix S.
#'  }
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param alpha Numeric value (optional)
#'@param beta Numeric value (optional)
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame( cbind(K1,K2) )
#'b4_ThompsonSampling(visitor_reward)
#'@export
b4_ThompsonSampling <- function(visitor_reward, alpha=1, beta=1, bandit=NULL) {
  stopifnot(is.double(alpha))
  stopifnot(is.double(beta))

  visitor_reward <- check_visitor_reward(visitor_reward, binariness = TRUE)

  if (is.null(bandit)) { # Create new TS object
    bandit <- empty_TS(K = ncol(visitor_reward),
                       alpha = 1,
                       beta = 1)
  }
  else {
    validate_TS(bandit) # Check validity of supplied object
  }

  if (ncol(visitor_reward)!= b4_K(bandit)) { # Check compatibility
    stop(
      "visitor reward and bandit are not compatible : different number of arms",
      call. = FALSE
    )
  }

  FMA_simulate(bandit, visitor_reward)
}
