B4FMA_simulate.B4EXP3 <- function(bandit, visitor_reward) {
  # Unpack B4FMA bandit parameters
  K <- ncol(visitor_reward)
  H1 <- B4FMA_get_horizon(bandit) + 1
  choice <- B4FMA_get_choice(bandit)
  S <- B4FMA_get_S(bandit)
  # Unpack B4EXP3 bandit parameters
  gamma <- B4EXP3_get_gamma(bandit)
  weight <- B4EXP3_get_weight(bandit)
  reward <- B4EXP3_get_reward(bandit)
  estimated_reward <- B4EXP3_get_estimated_reward(bandit)
  prob <- B4EXP3_get_prob(bandit)
  # Extend bandit visitor_reward data with additional visitor_reward
  visitor_reward <- rbind(B4FMA_get_visitor_reward(bandit), visitor_reward)
  H2 <- nrow(visitor_reward)

  # Iterate over horizon
  time <- system.time({

    for (t in H1:H2) { # TIC
      if (t <= K) {  # INITIALIZATION : PULL EVERY ARM
        choice[t] <- t
        weight_sum <- sum(weight)
        S <- B4FMA_play_arm(iter=t, arm=t, S, visitor_reward)
        prob[t]  <- (1 - gamma) * (weight[t]/weight_sum) + (gamma/K)
        reward[t] <- visitor_reward[t,t]

        estimated_reward[t] <- reward[t]/prob[t]
        weight[t] <- weight[t]*exp(gamma*estimated_reward[t]/K)
      }
      else { # STANDARD EXP3 BEHAVIOUR AFTERWARD
        for (j in 1:K){
          weight_sum <- sum(weight)
          prob[j] <- (1 - gamma) * (weight[j]/weight_sum) + (gamma/K)
        }
        choice[t] <- (sample(1:K, size=1, replace=TRUE, prob = prob))
        S <- B4FMA_play_arm(iter=t, arm=choice[t], S, visitor_reward)

        reward[t] <- visitor_reward[t,choice[t]]
        estimated_reward[t] <- reward[t]/prob[choice[t]]
        # Update weight
        weight[choice[t]] <- weight[choice[t]]*exp(gamma*estimated_reward[t]/K)
      }
    }
  }) + B4FMA_get_time(bandit) # TOC


  invisible(new_B4FMA(K = K,
                      H = H2,
                      visitor_reward = visitor_reward,
                      choice = choice,
                      S = S,
                      time = time,
                      gamma = gamma,
                      weight = weight,
                      reward = reward,
                      estimated_reward = estimated_reward,
                      prob = prob,
                      class = "B4EXP3"))
}
