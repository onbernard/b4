#' @export
B4UCB <- function(visitor_reward, alpha = 1, bandit = NULL) {
  stopifnot(is.double(alpha))

  visitor_reward <- check_visitor_reward(visitor_reward)

  if (is.null(bandit)) { # Create new B4UCB object
    bandit <- B4FMA(K = ncol(visitor_reward),
                    alpha = alpha,
                    probamax = vector(),
                    class = "B4UCB")
  }
  else {
    validate_B4UCB(bandit) # Check validity of supplied object
  }

  if (ncol(visitor_reward)!= B4FMA_get_K(bandit)) { # Check compatibility
    stop(
      "visitor reward and bandit are not compatible : different number of arms",
      call. = FALSE
    )
  }

  B4FMA_simulate(bandit, visitor_reward)
}
