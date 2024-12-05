get_gurobi_params = function(...)
{
  params <- list(...)
  params$NonConvex <- 2
  if(!('LogToConsole' %in% names(params))){params$LogToConsole <- 0}
  if(!('TimeLimit' %in% names(params))){params$TimeLimit <- 60}
  params = params[setdiff(names(params), 'Sigma')]

  return(params)
}
