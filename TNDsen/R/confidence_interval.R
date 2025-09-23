#' @export

get_confidence_interval = function(o.hat, alpha, conf.type)
{
  n = sum(o.hat)
  o.hat = o.hat/n

  if(!missing(alpha))
  {
    if(conf.type == 'transformed')
    {
      c = sqrt(-2*log((1-alpha)/8))
      k = sapply(o.hat, function(x){sqrt(x/(1-x))})
      Sigma = - k %*% t(k); diag(Sigma) = 1
      c = mvtnorm :: qmvnorm(alpha, tail = 'both.tails', sigma = Sigma)$quantile
      o.conf.lower =  0.5 * sin(pmax( asin(2*o.hat-1) - c/(sqrt(n)), -pi/2)) + 0.5
      o.conf.upper =  0.5 * sin(pmin( asin(2*o.hat-1) + c/(sqrt(n)), pi/2)) + 0.5

    }

    else if(conf.type == 'normal')
    {
      #Sigma = diag(o.hat) - o.hat %*% t(o.hat)

      k = sapply(o.hat, function(x){sqrt(x/(1-x))})
      Sigma = - k %*% t(k); diag(Sigma) = 1
      c = mvtnorm :: qmvnorm(alpha, tail = 'both.tails', sigma = Sigma)$quantile  * sqrt(o.hat*(1-o.hat))/ sqrt(n)
      o.conf.lower = pmax(o.hat - c, 0)
      o.conf.upper = pmin(o.hat + c, 1)
    }

    else if (conf.type == 'quadratic')
    {
      o.conf.lower = rep(0,4)
      o.conf.upper = rep(1,4)
    }
  }

  else
  {
    o.conf.lower = o.conf.upper = o.hat
  }


  return(list(
    o.conf.lower = o.conf.lower,
    o.conf.upper = o.conf.upper
  ))
}
