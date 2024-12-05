#' @import gurobi
#'

get_gurobi_bounds = function(model, params)
{

 # Minimization
  model$modelsense = 'min'
  result <- gurobi(model, params)
  lower <- result$objval
  lower.vars = result$x
  names(lower.vars) = model$varnames


  # Maximization
  model$modelsense = 'max'
  result <- gurobi(model, params)

  upper <- result$objval
  upper.vars = result$x
  names(upper.vars) = model$varnames


  return(list(upper.bound = upper, upper.vars = upper.vars,
              lower.bound = lower, lower.vars = lower.vars))

}





















