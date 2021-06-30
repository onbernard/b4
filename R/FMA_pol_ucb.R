# ~~~~~~~~ Implementation of UCB policy

B4FMA_simulate.B4UCB <- function(bandit, visitor_reward) {
  # Unpack bandit parameters
  K <- ncol(visitor_reward)
  H1 <- B4FMA_get_horizon(bandit) + 1
  choice <- B4FMA_get_choice(bandit)
  S <- B4FMA_get_S(bandit)
  alpha <- B4UCB_get_alpha(bandit)
  probamax <- B4UCB_get_probamax(bandit)
  # Build new reward and horizon
  visitor_reward <- rbind(B4FMA_get_visitor_reward(bandit), visitor_reward)
  H2 <- nrow(visitor_reward)

  # Iterate over horizon
  time <- system.time({ # TIC
    for (t in H1:H2) {
      if (t <= K) { # INITIALIZATION : # INITIALIZATION : PULL EVERY ARM
        choice[t] <- t
        probamax[t] <- Inf
        S <- B4FMA_play_arm(iter=t, arm=t, S=S, visitor_reward)
      }
      else { # STANDARD UCB BEHAVIOUR AFTERWARD
        upper_confidence_bounds <- B4FMA_proba_max_ucb(S, iter=t, alpha, K)

        choice[t] <- which.max(upper_confidence_bounds)
        probamax[t] <-  max(upper_confidence_bounds)

        S <- B4FMA_play_arm(iter = t,
                      arm = choice[t],
                      S,
                      visitor_reward)
      }
    }
  }) + B4FMA_get_time(bandit) # TOC

  invisible(new_B4FMA(K = K,
                      H = H2,
                      visitor_reward = visitor_reward,
                      choice = choice,
                      S = S,
                      time = time,
                      alpha = alpha,
                      probamax = probamax,
                      class = "B4UCB"))
}


#' @export
B4FMA_proba_max_ucb <- function(S, iter, alpha, K) {
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
