# ~~~~~~~~ Data check

check_visitor_reward <- function(visitor_reward, binariness = FALSE) {
  visitor_reward <- as.matrix(visitor_reward)

  if (nrow(visitor_reward)==0) {
    stop(
      "Visitor reward should have at least one row",
      call. = FALSE
    )
  }
  if (ncol(visitor_reward)==0) {
    stop(
      "Visitor reward should have at least one column",
      call. = FALSE
    )
  }
  if (!is.numeric(visitor_reward)) {
    stop(
      "Visitor reward should only contain numeric values",
      call. = FALSE
    )
  }
  if (any(is.na(visitor_reward))) {
    stop(
      "Visitor reward should not contain any NA values",
      call. = FALSE
    )
  }

  if(binariness && any(mapply(function(x)(x!=0 && x!=1), visitor_reward))){
    stop(
      "Visitor reward should only contain binary values (1 or 0)",
      call. = FALSE
    )
  }

  return(visitor_reward)
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

# ~~~~~~~~


validate_B4TS <- function(bandit) {
  stopifnot(inherits(bandit, what = "B4TS"))
  validate_B4FMA(bandit)
  # TODO
}
# ~~~~~~~~

validate_B4EPSG <- function(bandit) {
  stopifnot(inherits(bandit, what = "B4EPSG"))
  validate_B4FMA(bandit)
}
