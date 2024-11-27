#' @export

get_confidence_interval = function(o.hat, alpha, conf.type, Sigma)
{
  n = sum(o.hat)
  o = o.hat/n


  if(!missing(alpha))
  {
    if(conf.type == 'transformed')
    {
      c = sqrt(-2*log((1-alpha)/8))
      k = sapply(o, function(x){sqrt(x/(1-x))})
      Sigma = - k %*% t(k); diag(Sigma) = 1
      c = mvtnorm :: qmvnorm(alpha, tail = 'both.tails', sigma = Sigma)$quantile
      o.conf.lower =  0.5 * sin(pmax( asin(2*o-1) - c/(sqrt(n)), -pi/2)) + 0.5
      o.conf.upper =  0.5 * sin(pmin( asin(2*o-1) + c/(sqrt(n)), pi/2)) + 0.5

    }

    else if(conf.type == 'normal')
    {
      if(missing(Sigma))
      {
        se = sqrt(o*(1-o))
        k = sapply(as.vector(o), function(x){sqrt(x/(1-x))})
        Sigma = - k %*% t(k); diag(Sigma) = 1
      }
      else
      {
        se = sqrt(diag(Sigma))
        Sigma = diag(1/se) %*% Sigma %*% diag(1/se)
      }
      mvtquantile = mvtnorm :: qmvnorm(alpha, tail = 'both.tails', sigma = Sigma)$quantile
      c = mvtquantile * se / sqrt(n)
      o.conf.lower = pmax(o - c, 0)
      o.conf.upper = pmin(o + c, 1)
    }

    else if (conf.type == 'quadratic')
    {
      o.conf.lower = rep(0,4)
      o.conf.upper = rep(1,4)
    }
  }

  else
  {
    o.conf.lower = o.conf.upper = o
  }


  return(list(
    o.conf.lower = o.conf.lower,
    o.conf.upper = o.conf.upper
  ))
}
