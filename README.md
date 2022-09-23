# Using network meta-analysis to predict the percentage of missing participants for a future trial


## Description of the repository

The repository offers the typical structure of separate folders for data, and R (code/scripts), respectively.
* The _data_ folder includes four RData and text files. The RData files __binary_NMA__, __binary_PMA__, __continuous_NMA__, and __continuous_PMA__ refer to the datasets with the analysed pairwise meta-analyses (PMA) and network meta-analyses (NMA) on selected binary and continuous outcomes. The remaining RData files refer to the mean and standard deviation of selected empirically-based prior distributions for the between-trial variance that align with the outcome and intervention-comparison type of each analysis. Finally, the text files contain the posterior summaries from the Bayesian analysis (via the [R2jags](https://github.com/suyusung/R2jags/issues/) package) under all pre-defined scenarios for the missingness mechanism;
* The _R_ folder includes two analysis scripts (__A.Run empirical analysis_PMA & NMA.R__ and __Β.Determine robustness_PMA & NMA.R__). The first script sources the RData files with the datasets and performs the Bayesian analyses (one-stage random-effects PMA and NMA for continuous and binary outcomes with incorporation of the pattern-mixture model). The second script calculates the robustness index and apply our proposed decision framework for robustness of the primary analysis results for each analysis. Furthermore, it produces 1) the heatmap of robustness index for all possible comparisons of the network and 2) the panel of density plots of the summary log OR under all missingness scenarios and the Kullback-Leibler divergence. The network of Liu et al. (the motivating example in our article) has been used for that purpose. Finally, the remaining R scripts refer to the necessary functions to apply the aforementioned analysis scripts.<br>

[JAGS](http://mcmc-jags.sourceforge.net/) must be installed to employ the [R2jags](https://github.com/suyusung/R2jags/issues/) package. After downloading/cloning the repo, the user can use the .Rproj file to source all code.

The next sections briefly illustrate the functions of this repository.

## Output 

Prerequisite R packages: [R2jags](https://CRAN.R-project.org/package=R2jags), [dplyr](https://CRAN.R-project.org/package=dplyr), [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html), and [reshape2](https://cran.r-project.org/web/packages/reshape2/index.html)
