#' @export
B4EXP3 <- function(visitor_reward, gamma = 0.05, bandit = NULL) {
  stopifnot(is.double(gamma))

  visitor_reward <- check_visitor_reward(visitor_reward, binariness = TRUE)

  if (is.null(bandit)) { # Create new B4EXP3 object
    bandit <- B4FMA(K = ncol(visitor_reward),
                    gamma = gamma,
                    weight = rep(1, ncol(visitor_reward)),
                    reward = vector(),
                    estimated_reward = vector(),
                    prob = rep(0, ncol(visitor_reward)),
                    class = "B4EXP3")
  }
  else {
    validate_B4EXP3(bandit) # Check validity of supplied object
  }

  if (ncol(visitor_reward)!= B4FMA_get_K(bandit)) { # Check compatibility
    stop(
      "visitor reward and bandit are not compatible : different number of arms",
      call. = FALSE
    )
  }

  B4FMA_simulate(bandit, visitor_reward)
}
