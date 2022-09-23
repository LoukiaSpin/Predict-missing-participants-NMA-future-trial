# Using network meta-analysis to predict the percentage of missing participants for a future trial


## Description of the repository

The repository offers the typical structure of separate folders for data, and R (scripts and .RData):
* The _data_ folder includes one text file containing the analysed dataset for chronic obstructive pulmonary disease exacerbations from the systematic review of Baker et al. contain the posterior summaries from the Bayesian analysis (via the [R2jags](https://github.com/suyusung/R2jags/issues/) package) under all pre-defined scenarios for the missingness mechanism;
* The _R_ folder includes two analysis scripts (__A.Run empirical analysis_PMA & NMA.R__ and __Β.Determine robustness_PMA & NMA.R__). The first script sources the RData files with the datasets and performs the Bayesian analyses (one-stage random-effects PMA and NMA for continuous and binary outcomes with incorporation of the pattern-mixture model). The second script calculates the robustness index and apply our proposed decision framework for robustness of the primary analysis results for each analysis. Furthermore, it produces 1) the heatmap of robustness index for all possible comparisons of the network and 2) the panel of density plots of the summary log OR under all missingness scenarios and the Kullback-Leibler divergence. The network of Liu et al. (the motivating example in our article) has been used for that purpose. Finally, the remaining R scripts refer to the necessary functions to apply the aforementioned analysis scripts.<br>

[JAGS](http://mcmc-jags.sourceforge.net/) must be installed to employ the [R2jags](https://github.com/suyusung/R2jags/issues/) package. After downloading/cloning the repo, the user can use the .Rproj file to source all code.

The next sections briefly illustrate the functions of this repository.

## Output 

Prerequisite R packages: [R2jags](https://CRAN.R-project.org/package=R2jags), [dplyr](https://CRAN.R-project.org/package=dplyr), [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html), and [reshape2](https://cran.r-project.org/web/packages/reshape2/index.html)
