library(TNDsen)
library(nnet)
library(ggplot2)
library(rlang)


# Parameter Selection

softmax = function(x)
{
  exp_x = exp(x - max(x))
  return( exp_x / sum(exp_x))
}

generate_beta = function(delta = 0.1, gamma = 5, xi = 2, n.test = 5)
{
  # Simulation parameters
  o_range = c(0.1,0.9)
  COR_range = c(0.1, 0.5)

  # Testing Covariates
  C_star = matrix(seq(0, 1, length.out=n.test), ncol=1)
  design_matrix = cbind(1, C_star)



  while(TRUE)
  {
    # Sample Beta from N(0,1)
    beta = round(rbind(0, matrix(rnorm(6, mean=0), nrow = 3, ncol = 2)), 1)

    # True Probability range
    o = t(apply(beta %*%t(design_matrix), 2, softmax))
    if(min(o)<o_range[1] | max(o)>o_range[2]) next

    # True Causal Odds Ratio range
    l = pmax(o/(delta*gamma + (1-delta)), (o-delta)/(1-delta))          # l_zy
    u = pmin(o*gamma/(delta + (1-delta)*gamma), 1)                      # u_zy
    if(min(apply(l, 1, odds.ratio), apply(u, 1, odds.ratio))<=COR_range[1] |
       max(apply(l, 1, odds.ratio), apply(u, 1, odds.ratio))>=COR_range[2]) next

    break
  }


  rownames(beta) = c('B_00', 'B_10', 'B_01', 'B_11')
  colnames(beta) = c('intercept', 'slope')

  return(beta)
}

# Simulation

data_generation = function(beta, C, n.population)
{
  # Sample Covariates from Uniform (0,1)
  if(missing(C)){C = matrix(runif(n.population), ncol = 1)}

  # o = softmax(beta @ C + beta_0)
  o = t(apply(beta %*%t(cbind(1, C)), 2, softmax))

  # 2Y+Z|C,T = 1 ~ Multinomial(o)
  sample_class <- function(prob_vector) {
    sample(0:(length(prob_vector)-1), size = 1, prob = prob_vector)}
  class_labels = apply(o, 1, sample_class)

  data = data.frame(Z = class_labels %% 2, Y = class_labels %/% 2, C)
  names(data) = c('Z', 'Y', 'C')
  return(data)
}

continuous_confounder_simultaion = function(delta = 0.1, gamma = 5, xi = 2, n.test = 5, n.population = 50000, n.sim = 3, alpha = 0.95, conf.type = 'normal')
{
  beta = generate_beta(delta = delta, gamma = gamma, xi = xi, n.test = n.test)
  print(data.frame(beta))

  C_star = matrix(seq(0, 1, length.out=n.test), ncol=1)
  design_matrix = cbind(1, C_star)
  o = t(apply(beta %*%t(design_matrix), 2, softmax))
  result = data.frame()
  index = 1

  # Confidence Bounds for Causal Odds Ratio
  for(sim in 1:n.sim)
  {
    model = multinom(2*Y + Z ~ C, data=data_generation(beta, n.population = n.population), trace=FALSE)
    o.hat = predict(model, data_generation(beta, C_star), type = 'probs')
    Sigma_beta = vcov(model)



    for (i in 1:n.test)
    {
      print(paste('Simulation', sim, ':', i, '/', n.test))

      # Confidence Bounds for Odds Ratio
      Sigma_or = kronecker(c(-1,-1,1), design_matrix[i,]) * odds.ratio(o.hat[i,])  # 1 x 3d
      variance = diag(t(Sigma_or) %*% Sigma_beta %*% Sigma_or)
      result[index, 'OR'] = odds.ratio(o[i,])
      result$OR.CI.lower[index] = odds.ratio(o.hat[i,]) - qnorm((1+alpha)/2) * sqrt(variance)
      result$OR.CI.upper[index] = odds.ratio(o.hat[i,]) + qnorm((1+alpha)/2) * sqrt(variance)

      # True Bounds for Causal Odds Ratio
      bounds = TND_causal_bounds(o[i,], delta, gamma, xi)
      result$COR.lower[index] = bounds$lower.bound
      result$COR.upper[index] = bounds$upper.bound

      # Confidence Bounds for each Causal Odd Ratio
      ## Variance Matrix using Delta Method
      Sigma_o = diag(o.hat[i,]) - o.hat[i,] %*% t(o.hat[i,])    #  4 x 4
      Sigma_delta = kronecker(Sigma_o[2:4,], design_matrix[i,]) # 3d x 4
      Sigma = t(Sigma_delta) %*% Sigma_beta %*% Sigma_delta     #  4 x 4

      ## Sensitivity Bounds
      bounds = TND_causal_bounds(o.hat[i, ], delta, gamma, xi, alpha = alpha, conf.type = conf.type, Sigma = Sigma, dim=length(coef(model)), TimeLimit = 300)
      result[index, paste0('COR.CI.lower.', conf.type)] = bounds$lower.bound
      result[index, paste0('COR.CI.upper.', conf.type)] = bounds$upper.bound

      index = index + 1
      cat('\f')  # move cursor up 1 line; clear entire line
    }

  }
  return(result)
}


# Plotting

plot_continuous_confounder_simultaion = function(delta = 0.1, gamma = 5, xi = 2, n.test = 5, n.population = 50000, n.sim = 10, alpha = 0.95, conf.type = 'normal')
{
  simulation_results = continuous_confounder_simultaion(delta = delta, gamma = gamma, xi = xi, n.test = n.test,
                                                        n.population = n.population, n.sim = n.sim, alpha = alpha, conf.type = conf.type)
  plot_data = (1-simulation_results)*100
  plot_data$C = seq(0, 1, length.out=n.test)
  plot_data$sim = rep(1:n.sim, each = n.test)

  width = 0.25
  linewidth = 1
  point_size = 2.5
  ggplot(data = plot_data) +
    # Naive CI
    geom_point(aes(x = OR.CI.lower, y = sim + width, col='Naive CI'), size = point_size) +
    geom_point(aes(x = OR.CI.upper, y = sim + width, col='Naive CI'), size = point_size) +
    geom_segment(aes(x = OR.CI.lower, y = sim + width, xend = OR.CI.upper, yend = sim + width, col='Naive CI'), linewidth = linewidth) +

    # Sensitivity Adjusted CI
    geom_point(aes(x = .data[[paste0('COR.CI.lower.', conf.type)]], y = sim, col='Senstivity Adjusted CI'), size = point_size) +
    geom_point(aes(x = .data[[paste0('COR.CI.upper.', conf.type)]], y = sim, col='Senstivity Adjusted CI'), size = point_size) +
    geom_segment(aes(x = .data[[paste0('COR.CI.lower.', conf.type)]], y = sim, xend = .data[[paste0('COR.CI.upper.', conf.type)]],
                     yend = sim, col='Senstivity Adjusted CI'), linewidth = linewidth) +

    # True Causal Bounds
    geom_rect(aes(xmin = COR.lower, xmax = COR.upper, ymax = Inf, ymin = -Inf, fill='True Sensitivity Bounds'), col = 'blue', linetype = 'dashed') +

    # Confounders
    facet_wrap(~ C, scales = "free_x", nrow=1, labeller = labeller(C = function(x) paste("C =", round(as.numeric(x),2)))) +

    # Theme
    scale_y_continuous(breaks = 1:n.sim) +
    scale_color_manual(values = c('red', 'darkgreen'))+
    scale_fill_manual(values = c(alpha('blue', 0.025)),
                      guide = guide_legend(
                        order = 3,
                        override.aes = list(
                          linetype = 'dashed',
                          color = 'blue'))
    )+
    xlab("Vaccine Efficiency 100(1-OR)%") + ylab(paste(n.sim, 'Simulations')) +
    theme(legend.title = element_blank(),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.text = element_text(size=17),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title = element_text(size=17),
          strip.text = element_text(size = 15),
          axis.text.x = element_text(size=12))
}
