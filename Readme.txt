Software Requirements(installation guide):
-R         (https://cran.r-project.org/bin/windows/base/)
-R studio  (https://posit.co/download/rstudio-desktop/)
-Gurobi    (https://cran.r-project.org/web/packages/prioritizr/vignettes/gurobi_installation_guide.html)


R package Requirements:
Matrix
gurobi
extraDistr
knitrProgressBar
ggplot2
patchwork
metR
cowplot
viridis
plyr
latex2exp

To produce the plots in the paper run the following in the R console to install the package : 

devtools::install_local("TNDsen", INSTALL_opts="--no-multiarch")

and then run the Plots.Rmd file.