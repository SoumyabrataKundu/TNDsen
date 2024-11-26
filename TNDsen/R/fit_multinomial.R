#' @import nnet


fit_multinomial=function(data)
{
  # Fitting Multinomial regression
  data$Label = as.factor(2*data$Y + data$Z)
  data$Label = relevel(data$Label, ref="3")    # Levels : 3 0 1 2
  model = multinom(Label ~ C, data=data)

  design_matrix  = model.matrix(model)         # Extract Design Matrix
  Sigma_beta = vcov(model)                     # Extract the variance-covariance matrix
  o.hat = predict(model, type = "probs")       # Extract the probabilities
  o.hat = o.hat[,c(2,3,4,1)]                   # Levels : 0 1 2 3

  return(list(o.hat = o.hat, Sigma_beta = Sigma_beta, design_matrix = design_matrix))
}
