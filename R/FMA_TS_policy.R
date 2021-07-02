FMA_simulate.TS <- function(bandit, visitor_reward) {
  # Unpack FMA bandit parameters
  K <- b4_K(bandit)
  H1 <- b4_horizon(bandit) + 1
  choices <- b4_choices(bandit)
  rewards <- b4_rewards(bandit)
  S <- b4_S(bandit)
  proba <- b4_proba(bandit)
  # Unpack TS bandit parameters
  param <- b4_bandit_parameters(bandit)
  alpha <- param$alpha
  beta <- param$beta
  # Extend bandit visitor_reward data with additional visitor_reward
  visitor_reward <- rbind(b4_visitor_reward(bandit), visitor_reward)
  H2 <- nrow(visitor_reward)

  # Iterate over horizon
  time <- system.time({
    for (t in H1:H2) { # TIC
      if (t <= K) {  # INITIALIZATION : PULL EVERY ARM
        S <- b4_play_arm(iter=t, arm=t, S=S, visitor_reward)
        choices[t] <- t
        rewards[t] <- visitor_reward[t,t]
        proba[t] <- 1/K
      }
      else { # STANDARD TS BEHAVIOUR AFTERWARD
        # Sample an average from posterior distribution
        temp <- b4_condition_for_thompson_sampling(S, K, alpha, beta)
        # Save the chosen arm
        choices[t] <- temp$choice
        # Save probability sampled
        proba[t] <- temp$proba
        rewards[t] <- visitor_reward[t, choices[t]]
        # Update S
        S <- b4_play_arm(iter=t, arm=choices[t], S, visitor_reward)
      }
    }
  }) + b4_time(bandit) # TOC


  invisible(new_TS(K = K,
                   H = H2,
                   visitor_reward = visitor_reward,
                   choices = choices,
                   rewards = rewards,
                   time = time,
                   S = S,
                   proba = proba,
                   alpha = alpha,
                   beta = beta))
}

#' Condition for Thompson Sampling
#'
#' @description Returns a list containing the next arm to be pulled and the
#'   expected reward
#'
#' @param S FMA regression parameter
#' @param K number of arms
#' @param alpha,beta Thompson Sampling regression parameters
#'
#' @export
b4_condition_for_thompson_sampling <- function(S, K=ncol(S), alpha=1, beta=1) {
  distrib <- vector()
  for (j in 1:K) {
    # Sample a mean from a beta distribution of means
    distrib[j] <- stats::rbeta(1, alpha +  S[1,j]*S[2,j], beta + S[2,j] - S[1,j]*S[2,j])
  }
  list(choice = which.max(distrib), proba = max(distrib))
}
