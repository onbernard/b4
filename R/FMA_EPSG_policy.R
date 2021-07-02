# ~~~~~~~~ Implementation of Epsilon Greedy policy

FMA_simulate.EPSG <- function(bandit, visitor_reward) {
  # Unpack bandit parameters
  K <- b4_K(bandit)
  H1 <- b4_horizon(bandit) + 1
  choices <- b4_choices(bandit)
  rewards <- b4_rewards(bandit)
  S <- b4_S(bandit)
  # Unpack Epsilon Greedy parameters
  epsilon <- b4_bandit_parameters(bandit)$epsilon
  # Build reward and horizon out of new and old
  visitor_reward <- rbind(b4_visitor_reward(bandit), visitor_reward)
  H2 <- nrow(visitor_reward)

  # Iterate over horizon
  time <- system.time({ # TIC
    for (t in H1:H2) {
      if (t <= K) { # INITIALIZATION : PULL EVERY ARM
        S <- b4_play_arm(iter=t, arm=t, S, visitor_reward)
        choices[t] <- t
        rewards[t] <- visitor_reward[t,t]
      }
      else { # STANDARD EPSILON GREEDY BEHAVIOUR AFTERWARD
        choices[t] <- b4_condition_for_epsilon_greedy(S=S, epsilon=epsilon)
        S <- b4_play_arm(iter=t, arm=choices[t], S, visitor_reward)
        rewards[t] <- visitor_reward[t, choices[t]]
      }
    }
  }) + b4_time(bandit) # TOC

  invisible(
    new_EPSG(K = K,
             H = H2,
             visitor_reward = visitor_reward,
             choices = choices,
             rewards = rewards,
             time = time,
             S = S,
             epsilon = epsilon))
}

#' Condition for Epsilon Greedy
#'
#' @description Return the next arm to be pulled
#'
#' @param S FMA regression parameter
#' @param epsilon Epsilon Greedy exploration parameter
#' @param K number of arms
#'
#' @export
b4_condition_for_epsilon_greedy <- function(S, epsilon=0.25, K=ncol(S)) {
  switch(sample(c("exploit","explore"),
                 size=1,
                 replace=TRUE,
                 prob=c(1-epsilon,epsilon)),
  "exploit" = which.max(S[1,]),
  "explore" = sample(1:K, size=1, replace=T))
}
