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

heatmap_confounders = function(data, grid = 10, alpha, conf.type)
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
