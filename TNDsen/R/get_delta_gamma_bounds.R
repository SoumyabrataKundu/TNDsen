get_delta_gamma_bounds = function(o.hat, delta, gamma, alpha, conf.type)
{
  # Confidence Interval
  o.conf = get_confidence_interval(o.hat, alpha, conf.type)
  o.hat = o.hat / sum(o.hat)

  # Generating l_{xy} and u_{xy} from gamma,w and o_{xy}
  if(gamma == Inf) {
    l = pmax((o.conf$o.conf.lower-delta)/(1-delta), 0)
    u = pmin(o.conf$o.conf.upper/(1-delta), 1)}
  else{
    l = pmax(o.conf$o.conf.lower/((1-delta) + gamma * delta),(o.conf$o.conf.lower-delta)/(1-delta))
    u = pmin((gamma * o.conf$o.conf.upper)/(gamma * (1-delta) +  delta), 1)}


  ################################################## Optimization ########################################
  lower = minimize(matrix(l, nrow = 2),
                   matrix(u, nrow = 2))

  upper = minimize(matrix(l[c(2,1,4,3)], nrow = 2),
                   matrix(u[c(2,1,4,3)], nrow = 2))

  return(list(lower = lower, upper = upper))
}
