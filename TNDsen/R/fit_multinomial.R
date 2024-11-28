#' @import nnet


fit_multinomial=function(formula, data)
{
  # Fitting Multinomial regression
  formula <- as.formula(formula)
  environment(formula) <- environment()
  model = multinom(formula, data=data, trace=FALSE)

  o.hat = predict(model, type = "probs")       # predicted probabilities
  Sigma_beta = vcov(model)                     # variance-covariance matrix
  design_matrix  = model.matrix(model)         # design matrix

  return(list(o.hat = o.hat, Sigma_beta = Sigma_beta, design_matrix = design_matrix))
}
