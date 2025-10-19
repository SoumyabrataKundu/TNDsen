library(ggplot2)
library(TNDsen)
library(metR)
library(knitrProgressBar)


############################## Gives the grid for the heatmap ###########################################


heatmap_grid = function(o, delta = c(0.1, 0.2), gamma.range = c(1,5), xi.range = c(2,5), grid = 10, alpha, conf.type)
{
  # Making the grid for heat map
  data.heatmap = expand.grid(delta.seq.heatmap=delta,
                             gamma.seq.heatmap=seq(gamma.range[1], gamma.range[2], length.out = grid),
                             xi.seq.heatmap=seq(xi.range[1], xi.range[2], length.out = grid))

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



plot_heatmap = function(data.heatmap, o, contours, bound.type = 'upper', n.contours = 6, highlight)
{


  # Plot Design
  observed.or = odds.ratio(o)
  flag = bound.type == "upper"
  plot.list = list()
  delta.values = unique(data.heatmap$delta.seq.heatmap)

  for(i in 1:length(delta.values))
  {
    delta.value = delta.values[i]
    subplot.data = data.heatmap[data.heatmap$delta.seq.heatmap == delta.value,]
    ## Heatmap Limits
    heatmap.limits = `if`(flag, `if`(observed.or<1,
                                     c(min(subplot.data$odds.ratio.upper),
                                       min(max(subplot.data$odds.ratio.upper),1)),
                                     c(observed.or, max(subplot.data$odds.ratio.upper))),
                          `if`(observed.or<1,
                               c(min(subplot.data$odds.ratio.lower), observed.or),
                               c(1, max(subplot.data$odds.ratio.lower))))

    ## Heatmap Contours
    if(missing(contours))
    {
      contour_lines = `if`(observed.or<1,
                              `if`(flag,
                                   log(seq(exp(observed.or), exp(min(max(subplot.data$odds.ratio.upper), 1)), length.out = n.contours+2)),
                                   exp(seq(log(min(subplot.data$odds.ratio.lower)), log(observed.or), length.out = n.contours+2))),

                              `if`(flag,
                                   exp(seq(log(observed.or), log(max(subplot.data$odds.ratio.upper)),length.out = n.contours+2)),
                                   exp(seq(log(max(min(subplot.data$odds.ratio.lower), 1)), log(observed.or), length.out = n.contours+2)))
      )[-c(1, n.contours+2)]
    }

    else
    {
      contour_lines = 1- (contours[[i]] / 100)
    }



    plot.list[[paste0("delta = ", delta.value)]] =
      ggplot(subplot.data, aes(y = xi.seq.heatmap, x = gamma.seq.heatmap)) +

      # Heat map
      geom_tile(aes(fill = (1-`if`(flag, odds.ratio.upper, odds.ratio.lower))*100)) +
      scale_fill_distiller(palette = "Blues",
                           limits = rev(1-heatmap.limits)*100,
                           direction = 1,
                           na.value = "gray75") +
      # Contours
      geom_contour(mapping = aes(z = (1-`if`(flag, odds.ratio.upper, odds.ratio.lower))*100),
                   breaks = round((1-contour_lines)*100,2), colour = "darkred") +

      # Contour Labels
      geom_text_contour(mapping = aes(z = (1-`if`(flag, odds.ratio.upper, odds.ratio.lower))*100),
                        breaks = round((1-contour_lines)*100,2),
                        label.placer = label_placer_fraction(), size = 3, skip = 0,
                        stroke = 0.3) +

      # Axis labels, themes and title
      theme_classic() + theme(axis.title = element_blank(),
                              legend.position = "right",
                              plot.subtitle = element_text(size=15),
                              legend.title = element_blank(),
                              legend.text = element_text(size=12),
                              axis.text = element_text(size=12)) +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0)) +
      labs(subtitle = bquote(delta ~ '=' ~ .(round(delta.value, 2))))

    # Highlighted Points
    if (!missing(highlight))
    {
      l1 = (max(data.heatmap$gamma) - min(data.heatmap$gamma)) / 25
      l2 = (max(data.heatmap$xi) - min(data.heatmap$xi)) / 25
      plot.list[[paste0("delta = ", delta.value)]] =
        plot.list[[paste0("delta = ", delta.value)]] +
        geom_point(aes(x = gamma, y = xi), color = "red", size = 2, shape = 8, data = highlight) +
        geom_text(aes(x = gamma + l1, y = xi-l2, label=c("A", "B", "C", "D", "E")), color = "red", size = 5, data = highlight)

    }
  }

  return(plot.list)


}


############################### Guessing Sensitivity Parameters from data #############################

estimate_sen_params = function(data)
{
  confounders = setdiff(rownames(data), "all")
  sen_params = data.frame(confounders=confounders)
  for(i in 1:length(confounders))
  {
    subset.data = rbind(data['all',] - data[confounders[i],], data[confounders[i],])
    p = subset.data / rowSums(subset.data)
    sen_params$delta[i] = min(rowSums(subset.data) / sum(subset.data))
    sen_params$gamma[i] = max(p[1,] / p[2,], p[2,] / p[1,])
    sen_params$xi[i] = max(odds.ratio(p[1,])/odds.ratio(p[2,]), odds.ratio(p[2,])/odds.ratio(p[1,]))
  }
  return(sen_params)

}

############################### Main function for the gamma-xi heatmap #############################


causal_bounds_heatmap = function(o,
                                 delta = c(0.1, 0.2),
                                 gamma.range = c(1, 5),
                                 xi.range = c(2, 5),
                                 alpha = 0.95,
                                 conf.type = "normal",
                                 contours,
                                 bound.type = 'upper',
                                 n.contours = 6,
                                 grid = 5,
                                 highlight)

{

  data.heatmap = heatmap_grid(o = o, delta = delta , gamma.range = gamma.range, xi.range = xi.range, grid = grid, alpha = alpha, conf.type = conf.type)
  return(plot_heatmap(data.heatmap, o, contours, bound.type, n.contours, highlight))

}

