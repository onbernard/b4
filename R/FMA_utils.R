# ~~~~~~~~ functions commonly used in FMA policies

#' @export
B4FMA_play_arm <- function(iter, arm, S, visitor_reward) {
  # Update means
  if(S[1, arm] == Inf){
    S[1, arm] <- visitor_reward[iter, arm]
  }
  else{
    S[1, arm] <-
      ((S[1, arm] * S[2, arm] + visitor_reward[iter, arm]) / (S[2, arm] + 1))
  }

  # Update trials
  S[2, arm] = S[2, arm] + 1
  return (S)
}

#' @export
B4FMA_generate_S <- function(K) {
  mu_hat <- rep(Inf, K)
  N <- rep(0, K)

  S <- rbind(mu_hat, N)

  colnames(S) <- paste('bandit', 1:K)
  rownames(S) <- c("average visitor_reward", "trials")
  return(S)
}


