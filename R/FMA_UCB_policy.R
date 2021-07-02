# ~~~~~~~~ Implementation of UCB policy

FMA_simulate.UCB <- function(bandit, visitor_reward) {
  # Unpack bandit parameters
  K <- b4_K(bandit)
  H1 <- b4_horizon(bandit) + 1
  choices <- b4_choices(bandit)
  rewards <- b4_rewards(bandit)
  S <- b4_S(bandit)
  # Unpack UCB parameters
  alpha <- b4_bandit_parameters(bandit)$alpha
  proba <- b4_proba(bandit)
  # Build new reward and horizon
  visitor_reward <- rbind(b4_visitor_reward(bandit), visitor_reward)
  H2 <- nrow(visitor_reward)

  # Iterate over horizon
  time <- system.time({ # TIC
    for (t in H1:H2) {
      if (t <= K) { # INITIALIZATION : PULL EVERY ARM
        choices[t] <- t
        rewards[t] <- visitor_reward[t,t]
        proba[t] <- Inf
        S <- b4_play_arm(iter=t, arm=t, S=S, visitor_reward)
      }
      else { # STANDARD UCB BEHAVIOUR AFTERWARD
        upper_confidence_bounds <- b4_proba_max_ucb(S, iter=t, alpha, K)
        choices[t] <- which.max(upper_confidence_bounds)
        rewards[t] <- visitor_reward[t,choices[t]]
        proba[t] <-  max(upper_confidence_bounds)

        S <- b4_play_arm(iter = t,
                      arm = choices[t],
                      S,
                      visitor_reward)
      }
    }
  }) + b4_time(bandit) # TOC

  invisible(new_UCB(K = K,
                    H = H2,
                    visitor_reward = visitor_reward,
                    choices = choices,
                    rewards = rewards,
                    time = time,
                    S = S,
                    proba = proba,
                    alpha = alpha))
}


#' @export
b4_proba_max_ucb <- function(S, iter, alpha, K) {
  choice <- c()
  for (j in 1:K)
    if(S[2,j] == 0){
      choice[j] <- Inf
    }
  else{
    choice[j] <- S[1, j] + alpha * sqrt((2 * log(iter)) / S[2, j])
  }
  return (choice)
}
