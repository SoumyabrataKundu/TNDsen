get_delta_gamma_bounds = function(o.hat, delta, gamma)
{
  # Confidence Interval
  o.hat = o.hat / sum(o.hat)

  # Generating l_{xy} and u_{xy} from gamma,w and o_{xy}
  if(gamma == Inf) {
    l = pmax((o.hat-delta)/(1-delta), 0)
    u = pmin(o.hat/(1-delta), 1)}
  else{
    l = pmax(o.hat/((1-delta) + gamma * delta),(o.hat-delta)/(1-delta))
    u = pmin((gamma * o.hat)/(gamma * (1-delta) +  delta), 1)}


  ################################################## Optimization ########################################
  lower = minimize(matrix(l, nrow = 2),
                   matrix(u, nrow = 2))

  upper = minimize(matrix(l[c(2,1,4,3)], nrow = 2),
                   matrix(u[c(2,1,4,3)], nrow = 2))

  return(list(lower = lower, upper = upper))
}
