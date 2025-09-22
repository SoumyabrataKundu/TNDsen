library(TNDsen)
library(ggplot2)
library(knitrProgressBar)


# Getting the Bounds
get_bounds_from_data = function(data, delta, gamma, xi, alpha, conf.type='normal')
{

  pb = progress_estimated(nrow(data))

  for(i in 1:nrow(data))
  {
    o = as.numeric(data[i, c("o00", "o10", "o01", "o11")])

    # Estimates
    data$OR.estimate[i] = (1 - odds.ratio(o)) * 100

    # Bounds
    ## Our Bounds Without CI
    k = TND_causal_bounds(o, delta = delta, gamma = gamma, xi = xi)
    data$sensitivity.lower.estimate[i] = (1-k$upper.bound)*100
    data$sensitivity.upper.estimate[i] = (1-k$lower.bound)*100

    ## Our Bounds With CI
    k = TND_causal_bounds(o, delta = delta, gamma = gamma, xi = xi, alpha = alpha, conf.type = conf.type)
    data$sensitivity.CI.lower[i] = (1-k$upper.bound)*100
    data$sensitivity.CI.upper[i] = (1-k$lower.bound)*100

    ## Confidence Intervals
    n = sum(o)
    factor = exp(qnorm((1+alpha)/2) * sqrt(sum(n/o)) / sqrt(n))
    data$OR.CI.lower[i] = (1 - odds.ratio(o) * factor) * 100
    data$OR.CI.upper[i] = (1 - odds.ratio(o) / factor) * 100


    update_progress(pb)
  }
  return(data)
}


# Plotting Function
get_graph = function(results, delta, gamma, xi)
{
  n = nrow(results)
  index = rev(1:nrow(results))
  color = paste0("xi=", results$xi)

  width.end = 0.05
  linewidth = 1
  offset = 0.15

  plots = ggplot(data = results, aes(y=as.factor(index))) +
    geom_blank() +

    # Odds Ratio based CI
    geom_segment(aes(x = OR.CI.lower, xend = OR.CI.upper, col = "OR"),
                 y = index + offset, yend = index + offset, linewidth = linewidth) +
    geom_segment(aes(x = OR.CI.lower, xend = OR.CI.lower, col = "OR"),
                 y = index + offset + width.end, yend = index + offset - width.end, linewidth = linewidth) +
    geom_segment(aes(x = OR.CI.upper, xend = OR.CI.upper, col = "OR"),
                 y = index + offset + width.end, yend = index + offset - width.end, linewidth = linewidth) +

    geom_point(aes(x = OR.estimate, col = "OR"), y= index + offset, size = 2.5) +

    # Sensitivity Adjusted CI
    geom_segment(aes(x = sensitivity.CI.lower, xend = sensitivity.CI.upper, col = "senstivity"),
                 y = index - offset, yend = index - offset, linewidth = linewidth) +
    geom_segment(aes(x = sensitivity.CI.lower, xend = sensitivity.CI.lower, col = "senstivity"),
                 y = index - offset + width.end, yend = index - offset - width.end, linewidth = linewidth) +
    geom_segment(aes(x = sensitivity.CI.upper, xend = sensitivity.CI.upper, col = "senstivity"),
                 y = index - offset + width.end, yend = index - offset - width.end, linewidth = linewidth) +

    # Sensitivity Adjusted Bound Estimate
    geom_point(aes(x = sensitivity.lower.estimate, col = "senstivity"), y = index - offset, size = 2.5) +
    geom_point(aes(x = sensitivity.upper.estimate, col = "senstivity"), y = index - offset, size = 2.5)

  plots = plots +
    # Labels titles and legend
    scale_x_continuous(breaks = (0:10)*10,limits = c(min(results$sensitivity.CI.lower), 100)) +
    scale_y_discrete(labels = rev(results$vaccine))+
    scale_colour_manual(labels = c("Naive CI", bquote("Sensitivity Adjusted CI (" * delta == .(delta) * ", " ~ Gamma == .(gamma) * ", " ~ xi == .(xi) * ")")),
                        breaks = c("OR", "senstivity"),
                        values = c("darkred", "darkgreen", NA)
    ) +
    theme(axis.text.y = element_text(face = "bold", color="#993333", size=18),
          axis.text.x = element_text(face = "bold", color="#993333", size = 15),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size=15),
          legend.key.height = unit(3, "lines"),
          legend.position = c(0.25,.75))

  return(plots)
}
