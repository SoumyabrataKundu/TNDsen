TND_delta_gamma_bound = function(o.hat, delta, gamma)
{
  # Optimization
  bounds = get_delta_gamma_bounds(o.hat, delta, gamma)


  #################################################### I / O ###############################################

  # Normalizing o.hat
  if(!is.matrix(o.hat))
  {o.hat = matrix(c(o.hat), nrow = 2)}
  o.hat = o.hat / sum(o.hat)

  a.lower = bounds$lower$a
  dimnames(a.lower) = list(rownames(o.hat), colnames(o.hat))
  b.lower = (o.hat - a.lower * (1-delta)) / delta

  # The tight upper bound for this l_{xy} and u_{xy}
  a.upper = matrix(bounds$upper$a[c(2,1,4,3)], nrow = 2, dimnames = list(rownames(o.hat), colnames(o.hat)))
  b.upper = (o.hat - a.upper * (1-delta)) / delta

  return(list(upper.bound = 1/bounds$upper$value,
              o.upper = o.hat,
              p_zy.0.upper = a.upper,
              p_zy.1.upper = b.upper,

              lower.bound = bounds$lower$value,
              o.lower = o.hat,
              p_zy.0.lower = a.lower,
              p_zy.1.lower = b.lower))
}
