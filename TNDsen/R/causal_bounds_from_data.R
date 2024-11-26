
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

  ## Check alpha
  if(missing(alpha) & !missing(conf.type))
  {
    warning("Confidence level 'alpha' not specified, considering default value 0.95")
    alpha = 0.95
  }

  if(!missing(alpha)){if(alpha<0 | alpha>1) stop("'alpha' must be between 0 and 1")}

  ## Checking conf.type
  if(!missing(alpha) & missing(conf.type))
  {
    warning("'conf.type' not specified, considering default value 'normal'")
    conf.type = 'normal'
  }



  if(xi != Inf)  return(TND_gurobi_bounds_from_data(data, delta, gamma, xi, alpha, conf.type, ...))
  else if(!missing(conf.type))
  {
    if(conf.type == 'quadratic')return(TND_gurobi_bounds_from_data(data, delta, gamma, xi, alpha, conf.type, ...))
  }

  return(TND_delta_gamma_bound(o.hat, delta, gamma, alpha, conf.type))

}
