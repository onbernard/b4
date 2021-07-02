# ~~~~~~~~~~~ EPSG getters

# Generic is defined in BASE superclass

#' @export
b4_bandit_parameters.EPSG <- function(bandit) {
  validate_EPSG(bandit)
  return(c(list(epsilon = (bandit$epsilon)), NextMethod()))
}

# ~~~~~~~~~~~

#' @export
print.EPSG <- function(x, ...) { # TODO
  print("LOL EPSG")
}
