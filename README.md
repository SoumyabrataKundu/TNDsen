# TNDsen

Senstivity Analysis for Test Negative Design

## Software Requirements

- R         (https://cran.r-project.org/bin/windows/base/)
- R studio  (https://posit.co/download/rstudio-desktop/)
- Gurobi    (https://cran.r-project.org/web/packages/prioritizr/vignettes/gurobi_installation_guide.html)


## R package Requirements

- Matrix
- gurobi
- extraDistr
- knitrProgressBar
- ggplot2
- patchwork
- metR
- cowplot
- viridis
- plyr
- latex2exp

## TNDsen Installation

To install the package `TNDsen` clone the repository and then run the following in R console

`devtools::install_local("TNDsen", INSTALL_opts="--no-multiarch")`

To reproduce the figures in the paper run `Plots.Rmd`.

## Usage


To get the causal bounds for a `2x2` contingency table, use the following function:

```r
TND_causal_bounds(o, delta, gamma, xi, alpha, conf.type)
```
```
o         : contingency table (need not be normalized to sum one)
delta     : value of the sensitivity parameter delta, 
            should be between (0,1).
gamma     : value of the sensitivity parameter delta, 
            should be between (1, infinity), 
            default value is infinity.
xi        : value of the sensitivity parameter xi, 
            should be between (1, infinity), 
            default value is infinity.
alpha     : level of confidence interval for computing confidence interval.
            if conf.type is specified, default is 0.95
conf.type : type of confidence set to be used to compute confidence interval,
            if not specified causal bounds are computed without confidence,
            allowed values : 'normal', 'transformed' and 'quadratic.
```


