add_bounds_to_model = function(model, delta, gamma, xi, alpha, conf.type)
{

  random = !missing(alpha)
  n = sum(model$o.hat)
  o = model$o.hat / n

  if(random)
  {
    if(conf.type %in% c('quadratic'))
    {
      n.var = length(model$varnames)
      var = 1:n.var
      names(var) = model$varnames

      # Add Confidence Interval Constraint
      if('Sigma' %in% names(model))
      {Sigma = model$Sigma; invert = 2:4}
      else
      {Sigma = (diag(as.vector(o)) - as.vector(o) %*% t(as.vector(o))); invert = 1:3}
      Sigma.ginv = MASS :: ginv(Sigma[invert, invert])

      qc = list()
      qc$Qc = spMatrix(n.var, n.var, i = rep(var['t00'] - 1 + invert, length(invert)), j = rep(var['t00'] - 1 + invert, each = length(invert)), x = c(Sigma.ginv))
      qc$rhs = qchisq(alpha, 3) / n
      qc$sense = '<'

      model$quadcon = append(model$quadcon, list(qc))

      # t interval
      t.interval = list(o - 1, o)
    }

    else if(conf.type %in% c('transformed', 'normal'))
    {
      # Confidence Interval
      if('Sigma' %in% names(model)) o.conf = get_confidence_interval(model$o.hat, alpha, conf.type, model$Sigma)
      else o.conf = get_confidence_interval(model$o.hat, alpha, conf.type)
      t.interval = list(o - o.conf$o.conf.upper, o - o.conf$o.conf.lower)

    }

    else stop("Wrong choice 'conf.type', must be one of 'transfomed', 'normal' or 'quadratic'")

    # Parameter upper and lower bound
    model$lb <- c(0, t.interval[[1]], rep(0, 8), rep(1/gamma, 4), rep(0, 3), 1 / xi, rep(0, 3))
    model$ub <- c(delta, t.interval[[2]], rep(1, 8), rep(gamma, 4), rep(Inf, 3), xi, rep(Inf, 3))

  }

  else
  {
    # Parameter upper and lower bound
    model$lb <- c(0, rep(0, 8), rep(1/gamma, 4), rep(0, 3), 1 / xi, rep(0, 3))
    model$ub <- c(delta, rep(1, 8), rep(gamma, 4), rep(Inf, 3), xi, rep(Inf, 3))
  }



  return(model)

}
