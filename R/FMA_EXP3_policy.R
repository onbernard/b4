# ~~~~~~~~ Implementation of EXP3  policy

FMA_simulate.EXP3 <- function(bandit, visitor_reward) {
  # Unpack FMA bandit parameters
  K <- b4_K(bandit)
  H1 <- b4_horizon(bandit) + 1
  choices <- b4_choices(bandit)
  rewards <- b4_rewards(bandit)
  S <- b4_S(bandit)
  # Unpack EXP3 bandit parameters
  param <- b4_bandit_parameters(bandit)
  gamma <- param$gamma
  weight <- param$weight
  estimated_reward <- param$estimated_reward
  prob <- param$prob
  # Extend bandit visitor_reward data with additional visitor_reward
  visitor_reward <- rbind(b4_visitor_reward(bandit), visitor_reward)
  H2 <- nrow(visitor_reward)

  # Iterate over horizon
  time <- system.time({

    for (t in H1:H2) { # TIC
      if (t <= K) {  # INITIALIZATION : PULL EVERY ARM
        choices[t] <- t
        weight_sum <- sum(weight)
        S <- b4_play_arm(iter=t, arm=t, S, visitor_reward)
        prob[t]  <- (1 - gamma) * (weight[t]/weight_sum) + (gamma/K)
        rewards[t] <- visitor_reward[t,t]
        estimated_reward[t] <- rewards[t]/prob[t]
        weight[t] <- weight[t]*exp(gamma*estimated_reward[t]/K)
      }
      else { # STANDARD EXP3 BEHAVIOUR AFTERWARD
        for (j in 1:K){
          weight_sum <- sum(weight)
          prob[j] <- (1 - gamma) * (weight[j]/weight_sum) + (gamma/K)
        }
        choices[t] <- (sample(1:K, size=1, replace=TRUE, prob = prob))
        S <- b4_play_arm(iter=t, arm=choices[t], S, visitor_reward)

        rewards[t] <- visitor_reward[t,choices[t]]
        estimated_reward[t] <- rewards[t]/prob[choices[t]]
        # Update weight
        weight[choices[t]] <- weight[choices[t]]*exp(gamma*estimated_reward[t]/K)
      }
    }
  }) + b4_time(bandit) # TOC


  invisible(new_EXP3(K = K,
                      H = H2,
                      visitor_reward = visitor_reward,
                      choices = choices,
                      rewards = rewards,
                      S = S,
                      time = time,
                      gamma = gamma,
                      weight = weight,
                      estimated_reward = estimated_reward,
                      prob = prob))
}
