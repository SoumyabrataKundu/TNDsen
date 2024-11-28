#' @import gurobi
#' @import knitrProgressBar

TND_gurobi_bounds_from_data = function(formula, data, delta, gamma, xi, alpha, conf.type,...)
{
  random = !missing(alpha)
  multinomial_fit = fit_multinomial(formula, data)
  list2env(multinomial_fit, envir = environment())
  n = nrow(data)
  pb <- progress_estimated(n)


  for(i in 1:5)
  {
    # Sigma From Multinomial Logistic Regression (Delta Method)
    Sigma_o = (diag(o.hat[i,]) - o.hat[i,] %*% t(o.hat[i,]))   #  4 x 4
    Sigma_delta = kronecker(Sigma_o[2:4,], design_matrix[i,])  # 3d x 4
    Sigma = t(Sigma_delta) %*% Sigma_beta %*% Sigma_delta      #  4 x 4

    # Bounds for each predicted probability
    result = TND_causal_bounds(o.hat[i, ], data$delta[i], data$gamma[i], data$xi[i], alpha = alpha, conf.type = conf.type, Sigma = Sigma)
    data$lower.bound[i] = result$lower.bound
    data$upper.bound[i] = result$upper.bound
    update_progress(pb)
  }
  return(data)


}
