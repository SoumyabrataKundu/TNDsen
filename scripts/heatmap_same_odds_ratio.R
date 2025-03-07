library(ggplot2)
library(patchwork)
library(viridis)
library(TNDsen)
library(knitrProgressBar)




same_odds_ratio_grid = function(estimated.or, delta, gamma, xi, grid.size)
{

  data = data.frame()

  # Grid
  grid = merge(x = seq(0.01, 1, length.out = grid.size),
               y = seq(0.01, 1, length.out = grid.size))

  # Progress Tracker
  .pb = progress_estimated(nrow(grid))

  for (i in 1:nrow(grid))
  {
    cont.table = get_table(estimated.or, grid$x[i], grid$y[i])
    if(!any(is.na(cont.table))){

      # Bounds
      k = TND_causal_bounds(cont.table, delta, gamma, xi)

      # Storing
      data[i,c("o00", "o10", "o01", "o11")] = c(cont.table)
      data$lower[i] = k$lower
      data$upper[i] = k$upper
      data$x[i] = grid$x[i]
      data$y[i] = grid$y[i]
    }

    update_progress(.pb)
  }

  data = data[apply(data, 1, function(x)  all(!is.na(x)) & x["upper"]>0  ),]

  return(data)
}


get_table = function(estimated.or, alpha, beta)
{
  o = matrix(0, nrow = 2, ncol = 2)

  # Input o11 + o10 and o11 + o01
  a = 1-estimated.or
  b = (alpha + beta)*(estimated.or-1) + 1
  c = -estimated.or*alpha*beta

  if(a == 0) {o[2,2] =-c/b}
  else
  {
    o[2,2] = (-b + sqrt(b^2-4*a*c))/(2*a)
    if(o[2,2]<0){o[2,2] = (-b - sqrt(b^2-4*a*c))/(2*a)}
  }

  o[2,1] = alpha - o[2,2]
  o[1,2] = beta - o[2,2]
  o[1,1] = 1-sum(o)


  if(!any(is.na(o)) & all(o>0) & all(o<1)) return(o)

  return(NA)
}




heatmap_same_odds_ratio = function(estimated.or, delta, gamma, xi, grid.size = 20, log.transform = FALSE)
{
  data = same_odds_ratio_grid(estimated.or, delta, gamma, xi, grid.size)
  xlabels = list(expression("o"["11"]+"o"["01"]), expression("o"["00"]), expression("o"["00"]))
  ylabels = list(expression("o"["11"] + "o"["10"]), expression("o"["11"]), expression("o"["10"]))


  p = ggplot(data = data, aes(x = x, y = y, fill = `if`(log.transform, log(upper) - log(lower), upper-lower))) +
    geom_tile() +
    scale_fill_viridis(name = "", option = "F") +
    theme_classic()+
    theme(axis.title = element_text(size = 13)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    ggtitle(`if`(log.transform, "Log(Upper Bound/Lower Bound)", "Upper Bound - Lower Bound"))+
    xlab("Proportion of Positive Cases")+ ylab("Proportion of Treated Cases")

  return(p)
}
