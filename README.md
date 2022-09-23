# Using network meta-analysis to predict the percentage of missing participants for a future trial


## Description of the repository

The repository offers the typical structure of separate folders for data, and R (scripts and .RData):
* The _data_ folder includes one text file containing the analysed dataset for chronic obstructive pulmonary disease exacerbations from the systematic review of [Baker et al.](https://pubmed.ncbi.nlm.nih.gov/19637942/);
* The _R_ folder includes two analysis scripts (__NMA & PMA for placebo.R__ and __NMA & PMA for all references.R__). The first script contains the code for the baseline and relative effects models when placebo is the reference intervention. The second script contains the code for the baseline and relative effects models when the remaining intervention are the references. The results from both scripts are found in the folder _NMA & OMA results_ and are used for other scripts to create the Figures of the manuscirpt. Note that Figures 2 and S2 that refer to the forest plots from the baseline effects models under placebo and the remaining interventions, respectively, are created using the scripts __NMA & PMA for placebo.R__ and __NMA & PMA for all references.R__. The remaining scripts aim to create the main and supplementary Figures of the manuscript.
* The _NMA & PMA results_ subfolder in the _R_ folder contains the results from the baseline effects and relative effects models, the latter under network and pairwise meta-analyses. The .RData files are called from the functions in scripts to create the main and supplementary Figures of the manuscript.<br>

[JAGS](http://mcmc-jags.sourceforge.net/) must be installed to employ the [R2jags](https://github.com/suyusung/R2jags/issues/) package. After downloading/cloning the repo, the user can use the .Rproj file to source all code.

The next sections briefly illustrate the functions of this repository.

## Output 

Prerequisite R packages: [R2jags](https://CRAN.R-project.org/package=R2jags), [dplyr](https://CRAN.R-project.org/package=dplyr), [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html), and [reshape2](https://cran.r-project.org/web/packages/reshape2/index.html)
