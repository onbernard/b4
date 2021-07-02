
CFMA_simulate.LINUCB <- function(bandit, dt, visitor_reward) {
  # Unpack CFMA parameters
  K <- b4_K(bandit)
  H1 <- b4_horizon(bandit) + 1
  n_f <- b4_n_f(bandit)
  # Unpack LINUCB parameters
  param <- b4_bandit_parameters(bandit)
  alpha <- param$alpha
  # Unpack history
  choices <- b4_choices(bandit)
  rewards <- b4_rewards(bandit)
  proba <- b4_proba(bandit)
  # Unpack regression variables
  th_hat <- b4_th_hat(bandit)
  b <- b4_b(bandit)
  A <- b4_A(bandit)

  # Build new reward, horizon and contexts
  visitor_reward <- rbind(b4_visitor_reward(bandit), visitor_reward)
  dt <- rbind(b4_dt(bandit), dt)
  H2 <- nrow(dt)

  time <- system.time({ # TIC
  for (t in H1:H2) {
    x_t = dt[t,]
    p <- vector()
    for (j in 1:K) {
      A_inv      = solve(A[,,j])
      th_hat[j,] = A_inv %*% b[j,]
      ta         = t(x_t) %*% A_inv %*%  x_t
      a_upper_ci = alpha * sqrt(ta)           # Upper part of variance interval
      a_mean     = th_hat[j,] %*% x_t         # Current estimate of mean
      p[j]       = a_mean + a_upper_ci        # Top CI
    }

    # Choose the highest
    choices[t] <- which.max(p)

    # Save probability
    proba[t] <- max(p)

    # Get reward
    rewards[t] <- visitor_reward[t,choices[t]]

    # Update the input vector
    A[,,choices[t]] <- A[,,choices[t]]  + x_t %*% t(x_t) # Covariance matrix
    b[choices[t],] = b[choices[t],] +  x_t * (rewards[t] * 1)
  }
  }) + b4_time(bandit) # TOC


  invisible(new_LINUCB(K = K,
                       H = H2,
                       n_f = n_f,
                       dt = dt,
                       visitor_reward = visitor_reward,
                       booleanReward = b4_booleanReward(bandit),
                       b = b,
                       A = A,
                       th_hat = th_hat,
                       choices = choices,
                       rewards = rewards,
                       time = time,
                       proba = proba,
                       alpha = alpha))
}
