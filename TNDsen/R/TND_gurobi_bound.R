#' @import gurobi

TND_gurobi_bounds = function(o.hat, delta, gamma, xi, alpha, conf.type, ...)
{

  random = !missing(alpha)

  # Model Building
  ## Parameters
  params = get_gurobi_params(...)

  ## Model
  model = get_gurobi_model(o.hat, random, ...)

  ## Confidence Interval and bounds
  model = add_bounds_to_model(model, delta, gamma, xi, alpha, conf.type)

  # Optimization
  result = get_gurobi_bounds(model, params)

  ########################################################## I/O ######################################################

  # Normalize o.hat
  o.hat = o.hat / sum(o.hat)

  return(list(upper.bound = result$upper.bound,
              o.upper = `if`(random,
                             o.hat-matrix(result$upper.vars[c('t00', 't10', 't01', 't11')], nrow = 2,
                                          dimnames = dimnames(o.hat)) ,
                             matrix(o.hat, nrow = 2, dimnames = dimnames(o.hat))),

              a.upper = matrix(result$upper.vars[c('a00', 'a10', 'a01', 'a11')], nrow = 2, dimnames = dimnames(o.hat)),
              b.upper = matrix(result$upper.vars[c('b00', 'b10', 'b01', 'b11')], nrow = 2, dimnames = dimnames(o.hat)),

              lower.bound = result$lower.bound,
              o.lower = `if`(random,
                             o.hat-matrix(result$lower.vars[c('t00', 't10', 't01', 't11')], nrow = 2,
                                          dimnames = dimnames(o.hat)) ,
                             matrix(o.hat, nrow = 2, dimnames = dimnames(o.hat))),
              a.lower = matrix(result$lower.vars[c('a00', 'a10', 'a01', 'a11')], nrow = 2, dimnames = dimnames(o.hat)),
              b.lower = matrix(result$lower.vars[c('b00', 'b10', 'b01', 'b11')], nrow = 2, dimnames = dimnames(o.hat))))

}
