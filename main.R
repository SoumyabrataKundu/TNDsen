# Install and load the developed package 
library(devtools)
install_github("SoumyabrataKundu/TNDsen", subdir = "TNDsen")
library(TNDsen)

# library other package
lapply(c( "ggplot2", "knitrProgressBar", "patchwork", "extraDistr", "latex2exp", "nnet", "rlang", "viridis", "metR", "cowplot"), library, character.only = TRUE)

#### Real data application in Section 9 and Section A2.2  ####

# load data for the application from Thompson et al. (2021)
# data_vaccine is from Figure 1 of Thompson et al. (2021)
# data_confounders is from Figure 2 of Thompson et al. (2021)
load('data.rda')

# source the relevant R code
source('scripts/heatmap.R')
source('scripts/real_data_plots.R')
source('scripts/bounds_compare.R')

# Section 9, Figure 2
fig2 <- heatmap(data_confounders, grid = 10, alpha = 0.95, conf.type = 'normal')
ggsave("fig2.png", fig2, width = 12, height = 8)

# Section 9, Figure 3
fig3 <- vacccine_efficiency_comparison(data_vaccine, delta = 0.1, gamma = 3.5, xi = 3.5, alpha = 0.95, conf.type = 'quadratic')
ggsave("fig3.png", fig3, width = 12, height = 6)

# Section A2.2, Figure A3
figA3 <- heatmap_confounders(data_confounders, type='hospital', delta=0.1, gamma.range=c(1, 2.5), xi.range=c(1, 2.5), grid=10, alpha=0.95, conf.type='normal')
ggsave("figA3.png", figA3, width = 12, height = 15)

# Section A2.2, Figure A4
figA4 <- heatmap_confounders(data_confounders, type='emergency', delta=0.1, gamma.range=c(1, 3.5), xi.range=c(1, 3.5), grid=10, alpha=0.95, conf.type='normal')
ggsave("figA4.png", figA4, width = 12, height = 15)



#### Simulation in Section A2.1 ####
# source the relevant R code
source('scripts/heatmap_same_odds_ratio.R')

# Section A2.1, Figure A2(a)
figA2a <- heatmap_same_odds_ratio(0.5, delta = 0.1, gamma = 5, xi=Inf, grid=50, log.transform=TRUE)
ggsave("figA2a.png", figA2a, width = 12, height = 8)

# Section A2.1, Figure A2(b)
figA2b <- heatmap_same_odds_ratio(0.5, delta = 0.1, gamma = 5, xi=2, grid=50, log.transform=TRUE)
ggsave("figA2b.png", figA2b, width = 12, height = 8)




#### Simulation in Section A3.5 ####
# source the relevant R code
source('scripts/CI_compare.R')

# Section A3.5, Figure A5
set.seed(123)
o = c(0.1, 0.2, 0.3, 0.4)
figA5 <- CI_comparison(o, delta=0.1, gamma=5, xi=2, alpha=0.95, n.population=1000, n.sim=20)
ggsave("figA5.png", figA5, width = 12, height = 6)



#### Simulation in Section A6 ####
# source the relevant R code
source('scripts/continuous.R')

# Section A3.6, Figure A6
set.seed(123456)
figA6 <- plot_continuous_confounder_simultaion(delta = 0.1, gamma = 5, xi = 2, n.test = 5, n.population = 50000, n.sim = 10, alpha = 0.95, conf.type = 'quadratic')
ggsave("figA6.png", figA6, width = 12, height = 6)






