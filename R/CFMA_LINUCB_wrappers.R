#'LINUCB algorithm
#'
#' \itemize{ At each iteration
#'  \item Calculates the arm probabilities according to a linear regression of context in dt dataframe
#'  \item Choose the arm with the maximum upper bound (with alpha parameter)
#'  \item Receives a reward in visitor_reward for the arm and associated iteration
#'  \item Updates the results matrix S.
#'  }

#'@param dt Dataframe of integer or numeric values
#'@param visitor_reward Dataframe of integer or numeric values
#'@param alpha Numeric value (optional)
#'
#'@examples
#'size.tot = 1000
#'set.seed(4649)                          # this makes the example exactly reproducible
#'x1 = runif(size.tot, min=0, max=10)          # you have 4, largely uncorrelated predictors
#'x2 = runif(size.tot, min=0, max=10)
#'x3 = runif(size.tot, min=0, max=10)
#'x4 = runif(size.tot, min=0, max=10)
#'dt = cbind(x1,x2,x3,x4)
#'#arm reward
#'arm_1 <-  as.vector(c(-1,9,-8,4))
#'K1 = crossprod(t(dt),arm_1)
#'arm_2 <-  as.vector(c(-1,2,1,0))
#'K2 = crossprod(t(dt),arm_2)
#'arm_3 <-  as.vector(c(-1,-5,1,10))
#'K3 = crossprod(t(dt),arm_3)
#'visitor_reward <-  data.frame(K1,K2,K3)
#'dt <- as.data.frame(dt)
#'bandit <- b4_LINUCB(dt,visitor_reward)
#'@export
b4_LINUCB <-
  function(dt,
           visitor_reward,
           alpha = 1,
           booleanReward = FALSE,
           bandit = NULL) {

    stopifnot(is.double(alpha))
    stopifnot(is.logical(booleanReward))

    visitor_reward <- check_visitor_reward(visitor_reward) # TODO handle booleans
    dt <- as.matrix(dt) # TODO : check dt

    if (is.null(bandit)) {
      bandit <-
        empty_LINUCB(
          K = ncol(visitor_reward),
          n_f = ncol(dt),
          booleanReward = booleanReward,
          alpha = alpha
        )
    }
    else {
      validate_LINUCB(bandit)
    }

    # TODO : check compatibility
    CFMA_simulate(bandit, dt, visitor_reward)
}
