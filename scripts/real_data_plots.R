library(ggplot2)
library(cowplot)
library(patchwork)
library(latex2exp)

vacccine_efficiency_comparison = function(data, delta, gamma, xi, alpha, conf.type)
{
  bounds = get_bounds_from_data(data, delta = delta, gamma = gamma, xi = xi, alpha = alpha, conf.type = conf.type)
  plot.hospital = get_graph(bounds[bounds$type=='hospital',], delta = delta, gamma = gamma, xi = xi)
  plot.emergency = get_graph(bounds[bounds$type=='emergency',], delta = delta, gamma = gamma, xi = xi)


    leg <- get_legend(
      plot.hospital +
        theme(legend.position = "top",
              legend.key.width = unit(25, "pt"),
              legend.direction = "horizontal",
              legend.box = "horizontal")
    )

  label_row <- ggdraw() +
    draw_label("Hospitalization",       x = 0.275, y = 0.5, size = 18) +
    draw_label("Emergency Department",  x = 0.79, y = 0.5, size = 18)

  plots <- plot_grid(
    plot.hospital  + theme(legend.position = "none"),
    plot.emergency + theme(legend.position = "none"),
    ncol = 2, align = "hv"
  )

  g <- plot_grid(leg, label_row, plots, ncol = 1,
                 rel_heights = c(0.12, 0.08, 1))

  ggdraw(g) +
    annotate(geom = "text", x = 0.55, y = -0.05, label = "Vaccine Efficiency 100(1-OR)%", size=6.5) +
    theme(plot.margin = margin(15, 1, 35, 15))
}

heatmap = function(data, grid = 10, alpha, conf.type)
{
  data.hospital = as.matrix(data[data$type=='hospital', c('o00', 'o10', 'o01', 'o11')])
  rownames(data.hospital) = data$confounder[data$type=='hospital']
  sen_params.hospital = estimate_sen_params(data.hospital)

  contours = list(
    c(88.75, 89, 89.25, 89.50, 89.7, 90),
    c(85, 85.5, 86.5, 87.6, 88.6, 89.5, 90)
  )

  plot.list.hospital = causal_bounds_heatmap(o = data.hospital['all',], delta = c(0.1, 0.3), gamma.range = c(1,2.5), xi.range = c(1,2.5), alpha = alpha, conf.type = conf.type,
                                             grid = grid, contours = contours, bound.type = "upper", highlight = sen_params.hospital)


  data.emergency = as.matrix(data[data$type=='emergency', c('o00', 'o10', 'o01', 'o11')])
  rownames(data.emergency) = data$confounder[data$type=='emergency']
  sen_params.emergency = estimate_sen_params(data.emergency)

  contours = list(
    c(87, 87.5, 88, 88.5, 89),
    c(82, 84, 85, 86, 88)
  )


  plot.list.emergency = causal_bounds_heatmap(o = data.emergency['all',], delta = c(0.1, 0.3), gamma.range = c(1,3.5), xi.range = c(1,3.5), alpha = alpha, conf.type = conf.type,
                                              grid = grid, contours = contours, bound.type = "upper", highlight = sen_params.emergency)

  labels_letter = c("A", "B", "C", "D", "E")
  labels_confounder = c(TeX(" : age$\\geq 85$"), TeX(" : $\\geq 1$ chronic respiratory diseases"), TeX(" : $\\geq 1$ chronic nonrespiratory diseases"), " : black", " : hispanic")
  ggdraw(wrap_plots(c(plot.list.hospital, plot.list.emergency), ncol=2, byrow = FALSE)) +
    annotate(geom = "text", x = 0.45, y = 0, label = bquote(" Confounding Strength (" * Gamma * ")"), size=6.5) +
    annotate(geom = "text", x = 0, y = 0.5, label = bquote("Effect Heterogeneity (" * xi * ")"), angle = 90, size=6.5) +
    annotate(geom = "text", x = c(0.24, 0.425), y = rep(-0.075, 2), label = labels_letter[1:2], size=6.5, col='red') +
    annotate(geom = "text", x = c(0.3, 0.6), y = rep(-0.075, 2), label = labels_confounder[1:2], size=6.5, col='black') +
    annotate(geom = "text", x = c(0.105, 0.556, 0.743), y = rep(-0.14, 3), label = labels_letter[3:5], size=6.5, col='red') +
    annotate(geom = "text", x = c(0.3, 0.6, 0.8), y = rep(-0.14, 3), label = labels_confounder[3:5], size=6.5, col='black') +
    annotate(geom = "text", x = c(0.225, 0.725), y = rep(1.01, 2), label = c("Hospitalization", "Emergency Deptartment"), size=6.5, col='black') +
    theme(plot.margin = margin(20, 1, 80, 15))
}


heatmap_confounders = function(data, type='hospital', delta, gamma.range, xi.range, grid, alpha=0.95, conf.type='normal')
{
  data = data[data$type==type,]
  confounders = data$confounder
  data = as.matrix(data[,c('o00', 'o10', 'o01', 'o11')])
  rownames(data) = confounders
  
  sen_params = estimate_sen_params(data)
  
  contours = list(
    'hospital' = 
      list(
        age = 
          list(list(c(79.50, 80.00, 80.50, 81.00, 81.50)),
               list(c(89.75, 89.95, 90.15, 90.50, 90.75))),
        respiratory = 
          list(list(c(90.05, 90.25, 90.50, 90.75, 91.00)),
               list(c(83.50, 83.90, 84.25, 84.55, 85.00))),
        nonrespiratory = 
          list(list(c(88.00, 88.25, 88.50, 88.75, 89.00)),
               list(c(88.75, 89.00, 89.25, 89.50, 89.75))),
        black = 
          list(list(c(80.00, 80.50, 81.05, 81.50, 82.00)),
               list(c(89.00, 89.25, 89.50, 89.75, 90.00))),
        hispanic = 
          list(list(c(88.50, 88.85, 89.15, 89.55, 89.90)),
               list(c(88.25, 88.50, 88.75, 89.00, 89.40)))
      ),
    'emergency' = 
      list(
        age = 
          list(list(c(65.00, 66.00, 67.50, 69.00, 70.00)),
               list(c(87.00, 87.50, 88.00, 88.50, 89.25))),
        respiratory = 
          list(list(c(81.50, 82.20, 82.75, 83.50, 84.50)),
               list(c(87.00, 87.50, 88.00, 88.50, 89.00))),
        nonrespiratory = 
          list(list(c(82.50, 83.25, 84.00, 84.75, 85.50)),
               list(c(88.00, 88.50, 89.00, 89.50, 90.00))),
        black = 
          list(list(c(74.20, 75.50, 76.50, 77.50, 78.50)),
               list(c(86.90, 87.50, 88.00, 88.50, 89.00))),
        hispanic = 
          list(list(c(51.00, 53.25, 55.50, 57.50, 59.50)),
               list(c(87.75, 88.25, 88.75, 89.25, 89.75)))
      )  
  )
  subtitles = list(
    age = c(TeX("age$\\geq 85$"), TeX("age$< 85$")),
    respiratory = c(TeX("$\\geq 1$ chronic respiratory diseases"), "no chronic respiratory disease"),
    nonrespiratory = c(TeX("$\\geq 1$ chronic nonrespiratory diseases"), "no chronic nonrespiratory disease"),
    black = c("black", "not black"),
    hispanic = c("hispanic", "not hispanic")
  )
  
  titles = list(
    'hospital' = 'Hospitalization',
    'emergency' = 'Emergency Department'
  )
  
  plot.list = list()
  
  for(confounder in setdiff(confounders, 'all'))
  {
    data.heatmap = heatmap_grid(data[confounder,], delta = delta, gamma.range = gamma.range, xi.range = xi.range, 
                                grid = grid, alpha = alpha, conf.type = conf.type)
    plot.list[[confounder]] = plot_heatmap(data.heatmap, data[confounder,], contours=contours[[type]][[confounder]][[1]], 
                                           bound.type="upper", highlight=sen_params)[[1]] +
      labs(subtitle = subtitles[[confounder]][1]) +
      theme(plot.subtitle = element_text(size=15, hjust = 0.5))
    
    data.heatmap = heatmap_grid(data['all',] - data[confounder,], delta = delta, gamma.range = gamma.range, xi.range = xi.range, 
                                grid = grid, alpha = alpha, conf.type = conf.type)
    plot.list[[paste0(confounder,".c")]] = plot_heatmap(data.heatmap, data['all',] - data[confounder,], contours=contours[[type]][[confounder]][[2]], 
                                                        bound.type="upper", highlight=sen_params)[[1]] +
      labs(subtitle = subtitles[[confounder]][2]) +
      theme(plot.subtitle = element_text(size=15, hjust = 0.5))
  }
  
  labels_letter = c("A", "B", "C", "D", "E")
  labels_confounder = c(TeX(" : age$\\geq 85$"), TeX(" : $\\geq 1$ chronic respiratory diseases"), TeX(" : $\\geq 1$ chronic nonrespiratory diseases"), " : black", " : hispanic")
  
  ggdraw(wrap_plots(plot.list, ncol=2, byrow = TRUE)) +
    annotate(geom = "text", x = 0.5, y = 0, label = bquote(" Confounding Strength (" * Gamma * ")"), size=6.5) +
    annotate(geom = "text", x = 0, y = 0.5, label = bquote("Effect Heterogeneity (" * xi * ")"), angle = 90, size=6.5) +
    annotate(geom = "text", x = c(0.24, 0.425), y = rep(-0.025, 2), label = labels_letter[1:2], size=6.5, col='red') +
    annotate(geom = "text", x = c(0.3, 0.6), y = rep(-0.025, 2), label = labels_confounder[1:2], size=6.5, col='black') +
    annotate(geom = "text", x = c(0.105, 0.556, 0.743), y = rep(-0.05, 3), label = labels_letter[3:5], size=6.5, col='red') +
    annotate(geom = "text", x = c(0.3, 0.6, 0.8), y = rep(-0.05, 3), label = labels_confounder[3:5], size=6.5, col='black') +
    annotate(geom = "text", x = 0.5, y = 1.01, label = titles[[type]], size=8, col='black') +
    theme(plot.margin = margin(20, 1, 80, 15))
  
}