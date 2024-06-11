library(TNDsen)
library(ggplot2)
library(knitrProgressBar)


# Getting the Bounds
get_bounds_from_data = function(data, delta, gamma, xi, alpha, conf.type='transformed', ...)
{
  
  results = expand.grid(delta = delta, gamma = gamma, xi = xi, index=1:nrow(data))
  factor = (nrow(results) %/% nrow(data))
  if(missing(alpha)){alpha = 0.95}
  pb = progress_estimated(nrow(results))
  
  for(i in 1:nrow(results))
  {
     o = as.numeric(data[results$index[i], c("o00", "o10", "o01", "o11")])
    
    
    # Estimates
    results$Our.estimate[i] = (1 - odds.ratio(o)) * 100
    
    # Bounds
    ## Our Bounds Without CI
    k = TND_causal_bounds(o, results$delta[i], results$gamma[i], results$xi[i])
    results$Our.lower.estimate[i] = (1-k$upper.bound)*100
    results$Our.upper.estimate[i] = (1-k$lower.bound)*100
    
    ## Our Bounds With CI
    k = TND_causal_bounds(o, delta = results$delta[i], gamma = results$gamma[i], xi = results$xi[i], alpha = alpha, conf.type = conf.type)
    results$Our.lower[i] = (1-k$upper.bound)*100
    results$Our.upper[i] = (1-k$lower.bound)*100
    
    ## Confidence Intervals
    n = sum(o)
    factor = exp(qnorm((1+alpha)/2) * sqrt(sum(n/o)) / sqrt(n))
    results$CI.lower[i] = (1 - odds.ratio(o) * factor) * 100
    results$CI.upper[i] = (1 - odds.ratio(o) / factor) * 100
  
    
    update_progress(pb) 
  }

  
  return(results)
}




# Plotting Function
get_graph = function(results, delta, gamma, xi, title = "Some Title", labs = 1:nrow(results), width.end = 0.05, line_size = 1, ...)
{
  n = nrow(results)
  results$index = rev(results$index)
  labs = rev(labs)
  offsets = list(Our = c(-0.15,0), CI = 0.15)
  results$location = results$index + rep(offsets[["Our"]], length(unique(results$index)))
  color = paste0("xi=", results$xi)
  
  offset = offsets[["CI"]]
  
  plots = ggplot(data = results, aes(y=as.factor(index))) +
    geom_blank() +
    
    # Naive CI
    geom_segment(aes(x = CI.lower, xend = CI.upper, col = "CI"), 
                 y = results$index + offset, yend = results$index + offset, size = line_size) +
    geom_segment(aes(x = CI.lower, xend = CI.lower, col = "CI"), 
                 y = results$index + offset + width.end, yend = results$index + offset - width.end, size = line_size) +
    geom_segment(aes(x = CI.upper, xend = CI.upper, col = "CI"), 
                 y = results$index + offset + width.end, yend = results$index + offset - width.end, size = line_size) +
    
    geom_point(aes(x = Our.estimate, col = "CI"), y= results$index+offset, size = 2.5) +
    
    # Sensitivity Adjusted CI
    geom_segment(aes(x = Our.lower, xend = Our.upper, col = color),
                 y = results$location, yend = results$location, size = line_size) +
    geom_segment(aes(x = Our.lower, xend = Our.lower, col = color),
                 y = results$location + width.end, yend = results$location - width.end, size = line_size) +
    geom_segment(aes(x = Our.upper, xend = Our.upper, col = color),
                 y = results$location + width.end, yend = results$location - width.end, size = line_size) +


    geom_point(aes(x = Our.lower.estimate, col = color), y= results$location, size = 2.5) +
    geom_point(aes(x = Our.upper.estimate, col = color), y= results$location, size = 2.5)
  

  plots = plots +

    # Labels titles and legend
    scale_x_continuous(breaks = (0:10)*10,limits = c(min(results$Our.lower), 100)) +
    scale_y_discrete(labels = labs)+
    scale_colour_manual(labels = c("Naive CI", rev(sapply(unique(xi),  function(value)`if`(value == Inf,
                                                    bquote("Sensitivity CI ("*xi == infinity * ")"),
                                                    bquote("Sensitivity CI ("*xi == .(value) * ")"))
                                                   ))),
                        breaks = c("CI", "xi=Inf", "xi=2"),
                        values=c("black", "red", "blue")
                        ) +

    theme(axis.text.y = element_text(face = "bold", color="#993333", size=18, angle=45),
          axis.text.x = element_text(face = "bold", color="#993333", size = 15),
          legend.title = element_blank(),
          legend.text = element_text(size=15),
          legend.position = c(0.25,.85),
          plot.title = element_blank(),
          plot.caption = element_blank(),
          axis.title.x = element_text(size=18))+
    xlab("Efficiency (%)") + ylab("") +
    labs(title = title, caption = bquote("Sensitivity Parameters : " *
                                           delta * " = " * .(delta) *", " *
                                           Gamma * " = " * .(gamma) *", " *
                                           xi  * " = " *  .(xi) ))
  
  return(plots)
}



real_data_graphs = function(data, delta, gamma, xi, alpha, ...)
{
  bounds = get_bounds_from_data(data, delta, gamma, xi, alpha, ...)
  return(get_graph(bounds, delta, gamma, xi, ...))
}


