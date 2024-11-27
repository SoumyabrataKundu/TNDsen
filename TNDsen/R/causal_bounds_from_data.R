
#' Causal Bounds for Test Negative Design
#'
#' causal_bounds calculates the bounds for a 2X2 contingency table for specified sensitivity parameters.
#'
#' @param data data matrix
#' @param w Sensitivity Parameter specifying the proportion of alternative types.
#' @param gamma Sensitivity Parameter specifying the maximum probability ratio allowed.
#' @param xi Sensitivity Parameter specifying the allowed ratio of causal odds ratio allowed.
#' @param alpha If given then this specifies the confidence level, otherwise the input \code{o.hat} is treated as true value
#'
#' @return A list containing:
#'    \item{a.upper}{The values for which upper bound is achieved.}
#'    \item{upper.bound}{Upper Causal Bound.}
#'    \item{a.lower}{The values for which lower bound is achieved.}
#'    \item{lower.bound}{Lower Causal Bound.}
#'
#' @export
TND_causal_bounds_from_data = function(data, delta, gamma, xi, alpha, conf.type, ...)
{

  # Check Input
  ## Check delta
  if(!('delta' %in% names(data)))
  {
    if(missing(delta)) stop("'delta no provided in neither data nor arguments")
    else data$delta = delta
  }

  ## Check gamma
  if(!('gamma' %in% names(data)))
  {
    if(missing(gamma)) data$gamma = Inf
    else data$gamma = gamma
  }

  ## Check xi
  if(!('xi' %in% names(data)))
  {
    if(missing(xi)) data$xi = Inf
    else data$xi = xi
  }


  return(TND_gurobi_bounds_from_data(data, delta, gamma, xi, alpha, conf.type, ...))

}
