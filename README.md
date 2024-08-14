# TNDsen

This repository contains the package associated with the method described in
''Senstivity Analysis for Test Negative Design''.

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

To reproduce the figures in the paper see `Paper simulations and real data.md`.

<!-- ## Usage


To get the causal bounds for a `2x2` contingency table, use the following function:

```r
TND_causal_bounds(o, 
                  delta, 
                  gamma=Inf, 
                  xi=Inf, 
                  alpha=0.95, 
                  conf.type=c('normal', 'transformed', 'quadratic'))
```
Input to the function : 
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

Output is a list containing : 
```
upper.bound      : upper bound for causal odds ratio.
lower.bound      : lower bound for causal odds ratio.
a.upper, b.upper : probabilty distribution of exposure and outcome 
                   for the two different levels of unmeasure condounding
                   that achieves the upper bound.
a.lower, b.lower : probabilty distribution of exposure and outcome 
                   for the two different levels of unmeasure condounding
                   that achieves the lower bound.
``` -->

## Example

For a contingency table with values o_00 = 0.1, o_10 = 0.2, o_01 = 0.3 and o_11 = 0.4, the proposed causal bounds can be obtained using the following command:

```r
bounds = TND_causal_bounds(o = c(0.1, 0.2, 0.3, 0.4), delta=0.1, gamma=5, xi=2)
print(bounds$upper.bound) # Upper bound for the causal odds ratio
print(bounds$lower.bound) # Lower bound for the causal odds ratio
```

The value of `delta` should be within the range [0,1], and both `gamma` and `xi` should greater than or equal to 1 (Inf is the default value). By default, this does not generate confidence bounds. To obtain confidence bounds, include the following additional arguments:

```r
bounds = TND_causal_bounds(o = c(1000, 2000, 3000, 4000), delta=0.1, gamma=5, xi=2,
                           alpha=0.95, conf.type='normal')
```

In this example, the total population size is 10,000. The `alpha` argument determines the confidence level, and the `conf.type` specifies the type of confidence bound to apply, with options being 'normal', 'transformed', or 'quadratic'.

## Reference
Kundu, S., Ding, P., Li, X., & Wang, J. (2024). Sensitivity Analysis for the Test-Negative Design [Arxiv](https://arxiv.org/abs/2406.06980)