# b4fma generics

cumulative_regret <- function(bandit){
  UseMethod("cumulative_regret")
}

regret <- function(bandit){
  UseMethod("regret")
}

fma_simulate <- function(bandit){
  UseMethod("fma_simulate")
}

play_arm <- function(bandit, iter, arm){
  UseMethod("play_arm")
}

proba_max <- function(bandit, iter){
  UseMethod("proba_max")
}

condition <- function(bandit, iter){
  UseMethod("condition")
}
