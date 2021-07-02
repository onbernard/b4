# ~~~~~~~~ BASE OBJECT EVALUATION

#' Returns the cumulative regret of a bandit object
#'
#' The cumulative regret is computed as the cumulative sum of the simple regret.
#' See also \code{\link{b4_simple_regret}}. Additional arguments are passed on
#' to \code{[base]{plot}}
#'
#' @param bandit A bandit of any class
#' @param plot Logical. If TRUE, plots the cumulative regret
#' @param ... Additional plot arguments
#'
#' @export
b4_cumulative_regret <- function(bandit, plot=TRUE, ...) {
  UseMethod("b4_cumulative_regret")
}

#' @export
b4_cumulative_regret.BASE <- function(bandit, plot=TRUE, ...) {
  validate_BASE(bandit)
  cumulative_regret <- cumsum(b4_simple_regret(bandit))
  if (plot)  plot(cumulative_regret,
                  type="b",
                  main="Cumulative regret",
                  xlab="Iterations",
                  ylab="Cumulative regret",
                  ...)
  return(invisible(cumulative_regret))
}

# ~~~~~~~~

#' Simple regret
#'
#' @description Returns the simple regret of a bandit object. It is computed as,
#'   for each observation of visitor reward, its maximum minus the reward
#'   gotten. Additional arguments are passed on to \code{\link[base]{plot}}
#'
#' @param bandit A bandit of any class
#' @param plot Logical. If TRUE, plots the simple regret
#' @param ... Additional plot arguments
#'
#' @export
b4_simple_regret <- function(bandit, plot=TRUE, ...) {
  UseMethod("b4_simple_regret")
}

#' @export
b4_simple_regret.BASE <- function(bandit, plot=TRUE, ...) {
  validate_BASE(bandit)
  regret <- vector()
  for (t in 1:b4_horizon(bandit)) {
    regret[t] <- max(b4_visitor_reward(bandit)[t,])
  }
  regret <- regret - b4_rewards(bandit)

  if (plot)  plot(regret,
                  type="b",
                  main="Simple regret",
                  xlab="Iterations",
                  ylab="Simple regret",
                  ...)
  return(invisible(regret))
}

# ~~~~~~~~

#' Choice frequency histogram
#'
#' @description Plots the frequency histogram of the choice history of a bandit
#'   object.
#'
#' @param bandit A bandit of any class
#' @param ... Additional histogram plot arguments
#'
#' @export
hist.BASE <- function(x, ...) {
  validate_BASE(x)
  hist(b4_choices(x),
       main="Arm choice frequency",
       xlab="Arms",
       ylab="Frequency",
       ...)
}
