# ~~~~~~~~ functions commonly used in FMA policies

#' @export
b4_play_arm <- function(iter, arm, S, visitor_reward) {
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
b4_generate_S <- function(K) {
  S <- matrix(0, nrow=2, ncol=K)

  colnames(S) <- paste('bandit', 1:K)
  rownames(S) <- c("average visitor_reward", "trials")
  return(S)
}


