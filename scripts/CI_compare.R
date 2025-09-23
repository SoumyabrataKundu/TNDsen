library(TNDsen)
library(knitrProgressBar)
library(ggplot2)
library(patchwork)
library(scales)
library(extraDistr)
library(latex2exp)


coverage = function(o, delta = 0.1, gamma = 5, xi = 2, alpha = 0.95, n.population = 1000, n.sim = 10)
{
  CI_type = c('transformed', 'normal', 'quadratic'
              )
  data = data.frame()
  pb = progress_estimated(n.sim)
  o=o/sum(o)

  i = 1
  while(i<=n.sim)
  {
    o.hat = c(rmultinom(1, n.population, o))
    if(all(o.hat>0))
    {
      for(j in 1:length(CI_type))
      {
        t0 = Sys.time()
        k = TND_causal_bounds(o.hat, delta, gamma, xi, alpha = alpha, conf.type = CI_type[j])
        data[3*i+j-3, 'o00'] = o.hat[1]
        data[3*i+j-3, 'o10'] = o.hat[2]
        data[3*i+j-3, 'o01'] = o.hat[3]
        data[3*i+j-3, 'o11'] = o.hat[4]
        
        data[3*i+j-3, 'time'] = Sys.time() - t0
        data[3*i+j-3, 'CI'] = CI_type[j]
        data[3*i+j-3, 'lower'] = k$lower.bound
        data[3*i+j-3, 'upper'] = k$upper.bound
      }
      i = i+1
      update_progress(pb)
      
    }
  }

  return(data)
}



squash_axis <- function(from, to, factor) { 
  
  trans <- function(x) {    
    # get indices for the relevant regions
    isq <- x > from & x < to
    ito <-  x >= to
    
    # apply transformation
    x[isq] <- from + (x[isq] - from)/factor
    x[ito] <- from + (to - from)/factor + (x[ito] - to)
    
    return(x)
  }
  
  inv <- function(x) {
    # get indices for the relevant regions
    isq <- x > from & x < from + (to - from)/factor
    ito <- x >= from + (to - from)/factor
    
    # apply transformation
    x[isq] <- from + (x[isq] - from) * factor
    x[ito] <- to + (x[ito] - (from + (to - from)/factor))
    
    return(x)
  }
  
  # return the transformation
  return(trans_new("squash_axis", trans, inv))
}


plot_CI_compare = function(data, o = c(0.1,0.2,0.3,0.4), delta = 0.1, gamma = 5, xi = 2, alpha = 0.95)
{
  k = TND_causal_bounds(c(o), delta, gamma, xi)
  n.sim = nrow(data) %/% length(unique(data$CI))
  
  
  # Density
  p1 = ggplot(data = data) + 
    
    stat_density(data = subset(data, CI == "normal"), aes(x = lower, col = "normal"), geom="line")+
    stat_density(data = subset(data, CI == "normal"), aes(x = upper, col = "normal"), geom="line")+
    
    stat_density(data = subset(data, CI == "transformed"), aes(x = lower, col = "transformed"), geom="line")+
    stat_density(data = subset(data, CI == "transformed"), aes(x = upper, col = "transformed"), geom="line")+
    
    stat_density(data = subset(data, CI == "quadratic"), aes(x = lower, col = "quadratic"), geom="line")+
    stat_density(data = subset(data, CI == "quadratic"), aes(x = upper, col = "quadratic"), geom="line")+
    
    scale_y_continuous(expand = c(0, 0)) +
    coord_trans(x='log10') +
    geom_hline(yintercept=0) + xlab("Bounds") + ylab("Density") +
    scale_color_discrete(name="CI Type", labels = c(TeX("$C_\\alpha^N$"), TeX("$C_\\alpha^Q$"), TeX("$C_\\alpha^T$"))) +
    theme(legend.position = c(0.85, 0.8)) +
    geom_vline(xintercept = k$lower.bound, linetype="dashed") +
    geom_vline(xintercept = k$upper.bound, linetype="dashed") +
    annotate("text", x = k$lower.bound-0.05, y = 5, label = "True Lower bound", angle = "90")+
    annotate("text", x = k$upper.bound+0.08, y = 5, label = "True Upper bound", angle = "270")
  
  # Log Difference
  p2 = ggplot(data=data) + 
    geom_boxplot(aes(x = CI, y = log(upper) - log(lower)), fill = 'lightblue') +
    scale_x_discrete(labels = c(TeX("$C_\\alpha^N$"), TeX("$C_\\alpha^Q$"), TeX("$C_\\alpha^T$")))+
    ylab("Log(Upper / Lower)")+
    theme(axis.title.x = element_blank())

  
  p3 = ggplot(data = data, aes(x=1:n.sim)) +
    geom_point(data = subset(data, CI == "normal"), aes(y = time, col = "normal")) +
    geom_point(data = subset(data, CI == "quadratic"), aes(y = time, col = "quadratic")) +
    geom_point(data = subset(data, CI == "transformed"), aes(y = time, col = "transformed")) +
    coord_trans(y='log10') +
    xlab("Simulation") + ylab("Time(s)")+
    theme(legend.position = "none")
  
            
  
  p1 + (p2/p3)
}

CI_comparison = function(o = c(0.1,0.2,0.3,0.4), delta = 0.1, gamma = 5, xi = 2, alpha = 0.95, n.population = 1000, n.sim = 20)
{
  data = coverage(o = o, delta = delta, gamma = gamma, xi = xi, alpha = alpha, n.population = n.population, n.sim = n.sim)
  plot_CI_compare(data = data, o = o,  delta = delta, gamma = gamma, xi = xi, alpha = alpha)
}

