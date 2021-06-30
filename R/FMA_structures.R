# ~~~~~~~~ Structure constructors

new_B4FMA <- function(K,
                      H,
                      visitor_reward,
                      choice,
                      S,
                      time,
                      ...,
                      class = character()) {
  structure(
    list(
      K = K,
      H = H,
      visitor_reward = visitor_reward,
      choice = choice,
      S = S,
      time = time,
      ...
    ),
    class = c(class, "B4FMA")
  )
}

B4FMA <- function(K, ..., class){
  new_B4FMA(K = K,
            H = 0,
            visitor_reward = matrix(0,nrow = 0, ncol = K),
            choice = vector(),
            S = B4FMA_generate_S(K),
            time = 0,
            ...,
            class = class)
}

# ~~~~~~~~ Structure validation

validate_B4FMA <- function(bandit) {
  stopifnot(inherits(bandit, what = "B4FMA"))
  # TODO
}

# ~~~~~~~~

validate_B4UCB <- function(bandit) {
  stopifnot(inherits(bandit, what = "B4UCB"))
  validate_B4FMA(bandit)
  # TODO
}

# ~~~~~~~~

validate_B4EXP3 <- function(bandit) {
  stopifnot(inherits(bandit, what = "B4EXP3"))
  validate_B4FMA(bandit)
  # TODO
}
