# ~~~~~~~~

CFMA_simulate <- function(bandit, dt, visitor_reward) {
  UseMethod("CFMA_simulate")
}

# ~~~~~~~~

#' Returns the actual CFMA reward expectation vector based on visitor reward
#'
#' @param bandit A CFMA bandit object
#'
#' @export
b4_theta <- function(bandit) {
  UseMethod("b4_theta")
}

#' @export
b4_theta.CFMA <- function(bandit) { # TODO
  K <- b4_K(bandit)
  n_f <- b4_n_f(bandit)
  dt <- b4_dt(bandit)
  visitor_reward <- b4_visitor_reward(bandit)

  th <- vector()

  temp <- as.data.frame(dt)
  if (b4_booleanReward(bandit)) {
    for (i in 1:K) {
      temp$prediction <- visitor_reward[,i]
      # Build linear regression model on full data
      logitMod <-
        stats::glm(prediction ~ .,
                   family = stats::binomial(link = 'logit'),
                   data = temp)
      # Intercept is not saved
      th <- rbind(th ,  linearMod$coefficients[-1])

      temp$prediction <- NULL
    }
  }
  else {
    for (i in 1:K) {
      temp$prediction <- visitor_reward[,i]
      # Build linear regression model on full data
      linearMod <- stats::lm(prediction ~., data = temp)
      # Intercept is not saved
      th <- rbind(th, linearMod$coefficients[-1])

      temp$prediction <- NULL
    }
  }
  # TODO colnames(th) <- colnames(dt)
  # TODO rownames(th) <- colnames(visitor_reward)
  return(th)
}
