#'EXP3 algorithm
#'
#'Exponential  Weights  for  Exploration  and  Exploitation (EXP3) bandit strategy. Uses a list of weigths which evolve according
#'to arm's reward. The gamma parameter is a coefficient for balancing between exploitation and exploration.

#'Generate a matrix to save the results (S).
#' \itemize{ At each iteration
#'  \item Update weight parameter for each arm
#'  \item Choose randomly an arm according to the distribution of proba
#'  \item Receives a reward in visitor_reward for the arm and associated iteration
#'  \item Updates the results matrix S.
#'  }
#'
#'@param visitor_reward Dataframe of integer or numeric values
#'@param gamma Numeric value (optional)
#'
#'@examples
#'## Generates 1000 numbers from 2 uniform distributions
#'set.seed(4434)
#'K1 <- rbinom(1000, 1, 0.6)
#'K2 <- rbinom(1000, 1, 0.7)
#'## Define a dataframe of rewards
#'visitor_reward <- as.data.frame( cbind(K1,K2) )
#'EXP3_alloc <- b4_EXP3(visitor_reward)
#'@export
b4_EXP3 <- function(visitor_reward, gamma = 0.05, bandit = NULL) {
  stopifnot(is.double(gamma))

  visitor_reward <-
    check_visitor_reward(visitor_reward, binariness = TRUE)

  if (is.null(bandit)) {
    # Create new EXP3 object
    bandit <- empty_EXP3(K = ncol(visitor_reward),
                         gamma = gamma)
  }
  else {
    validate_EXP3(bandit) # Check validity of supplied object
  }

  if (ncol(visitor_reward) != b4_K(bandit)) {
    # Check compatibility
    stop("visitor reward and bandit are not compatible : different number of arms",
         call. = FALSE)
  }

  FMA_simulate(bandit, visitor_reward)
}
