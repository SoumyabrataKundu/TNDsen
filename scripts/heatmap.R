library(ggplot2)
library(patchwork)
library(TNDsen)
library(metR)
library(cowplot)
library(knitrProgressBar)


############################## Gives the grid for the heatmap ###########################################


heatmap_grid = function(o, delta.range = c(0.1, 0.3), gamma.range = c(1,5), xi = c(2,Inf), grid = 10, alpha, conf.type = "normal")
{
  # Making the grid for heat map
  data.heatmap = expand.grid(delta.seq.heatmap=seq(delta.range[1], delta.range[2], length.out = grid),
                             gamma.seq.heatmap=seq(gamma.range[1], gamma.range[2], length.out = grid),
                             xi.seq.heatmap=xi)

  # Computing the bounds
  pb = progress_estimated(nrow(data.heatmap))
  for (i in 1:nrow(data.heatmap)) {
    
    k = TND_causal_bounds(o, data.heatmap$delta.seq.heatmap[i],
                      data.heatmap$gamma.seq.heatmap[i],
                      data.heatmap$xi.seq.heatmap[i], 
                      alpha, conf.type)

    data.heatmap$odds.ratio.upper[i] = k$upper.bound
    data.heatmap$odds.ratio.lower[i] = k$lower.bound

    update_progress(pb)

  }

  return(data.heatmap)
}


######################################## Plot Heatmap ###################################################



plot_heatmap = function(data.heatmap, o, OR, bound.type = 'upper', log.transformed = FALSE, n.contours = 6)
{


  # Plot Design
  observed.or = odds.ratio(o)
  flag = bound.type == "upper"
  plot.list = list()

  for(value in unique(data.heatmap$xi.seq.heatmap))
  {
    
    if(missing(OR))
    {
      ## Contours
      OR = `if`(observed.or<1,
                `if`(flag,
                     log(seq(exp(observed.or), exp(min(max(data.heatmap$odds.ratio.upper[data.heatmap$xi.seq.heatmap == value]), 1)), length.out = n.contours+2)),
                     exp(seq(log(min(data.heatmap$odds.ratio.lower[data.heatmap$xi.seq.heatmap == value])), log(observed.or), length.out = n.contours+2))),
                
                `if`(flag,
                     exp(seq(log(observed.or), log(max(data.heatmap$odds.ratio.upper[data.heatmap$xi.seq.heatmap == value])),length.out = n.contours+2)),
                     exp(seq(log(max(min(data.heatmap$odds.ratio.lower[data.heatmap$xi.seq.heatmap == value]), 1)), log(observed.or), length.out = n.contours+2)))
      )[-c(1, n.contours+2)]
    }
    
    ## Heatmap Limits
    
    heatmap.limits = `if`(flag, `if`(observed.or<1,
                                     c(min(data.heatmap$odds.ratio.upper[data.heatmap$xi.seq.heatmap == value]),
                                       min(max(data.heatmap$odds.ratio.upper[data.heatmap$xi.seq.heatmap == value]),1)),
                                     c(odds.ratio(o), max(data.heatmap$odds.ratio.upper[data.heatmap$xi.seq.heatmap == value]))),
                          `if`(observed.or<1,
                               c(min(data.heatmap$odds.ratio.lower[data.heatmap$xi.seq.heatmap == value]), odds.ratio(o)),
                               c(1, max(data.heatmap$odds.ratio.lower[data.heatmap$xi.seq.heatmap == value]))))
    

   
    
    plot.list[[paste0("xi = ", value)]] =
      ggplot(subset(data.heatmap, xi.seq.heatmap == value), aes(y = gamma.seq.heatmap, x = delta.seq.heatmap)) +

      # Heat map
      geom_tile(aes(fill = (1-`if`(flag, odds.ratio.upper, odds.ratio.lower))*100)) +
      # Heat map Color Gradient Scale
      scale_fill_distiller(name =  `if`(!flag, "Upper", "Lower"),
                           palette = "Blues",
                           limits = rev(1-heatmap.limits)*100,
                           direction = 1,
                           na.value = "gray75"
                           ) +

      # Contours
      geom_contour(mapping = aes(z = (1-`if`(flag, odds.ratio.upper, odds.ratio.lower))*100),
                   breaks = round((1-OR)*100,2), colour = "darkred") +

      # Contour Labels
      geom_text_contour(mapping = aes(z = (1-`if`(flag, odds.ratio.upper, odds.ratio.lower))*100),
                        breaks = round((1-OR)*100,2),
                        label.placer = label_placer_fraction(), size = 3, skip = 0,
                        stroke = 0.3) +

      # Axis labels, themes and title
      theme_classic() + theme(axis.title = element_blank(), 
                              legend.position = "right", 
                              plot.subtitle = element_text(size=15),
                              legend.title = element_text(size=15),
                              legend.text = element_text(size=12)
                              ) +
      scale_x_continuous(expand=c(0,0)) + 
      scale_y_continuous(expand=c(0,0)) + 
      labs(subtitle = `if`(value == Inf,
                           bquote("Effect Heterogeneity (" * xi ~  '=' ~ infinity * ")"),
                           bquote("Effect Heterogeneity (" * xi ~ '=' ~ .(value) * ")")
                             ))


  }

  o = round(o/sum(o),2)
  ggdraw(
    wrap_plots(plot.list,
                    ncol=floor(sqrt(length(unique(data.heatmap$xi.seq.heatmap)))))
    ) +
    annotate(geom = "text", x = 0.45, y = 0, label = bquote(" Partial Control of Confounding (" * delta * ")"), size=6.5) +
    annotate(geom = "text", x = 0, y = 0.5, label = bquote(" Confounding Strength (" * Gamma * ")"), angle = 90, size=6.5) +
    theme(plot.margin = margin(1, 1, 10, 10))

}




############################### Main function for the gamma-xi heatmap #############################


causal_bounds_heatmap = function(o,
                                 delta.range = c(0.1, 0.3),
                                 gamma.range = c(1, 5),
                                 xi = c(2, Inf),
                                 alpha, 
                                 conf.type = "normal",
                                 OR,
                                 bound.type = 'upper',
                                 log.transformed = FALSE,
                                 n.contours = 6,
                                 grid = 5)

{

  data.heatmap = heatmap_grid(o = o, delta.range = delta.range, gamma.range = gamma.range, xi = xi, grid = grid, alpha = alpha, conf.type = conf.type)
  plot_heatmap(data.heatmap, o, OR, bound.type, log.transformed, n.contours)

}




# Usage
# causal_bounds_heatmap(c(3,276,66,473), delta.range(0.1, 0.3), gamma.range = c(1,3), xi.range = c(1,3), n.deltas = 4, n.contours = 6, grid = 10)
