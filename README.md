# Using network meta-analysis to predict the percentage of missing participants for a future trial


## Description of the repository

The repository offers the typical structure of separate folders for data, and R (scripts and .RData):
* The _data_ folder includes one text file containing the analysed dataset for chronic obstructive pulmonary disease exacerbations from the systematic review of [Baker et al.](https://pubmed.ncbi.nlm.nih.gov/19637942/);
* The _R_ folder includes two analysis scripts (__NMA & PMA for placebo.R__ and __NMA & PMA for all references.R__). The first script contains the code for the baseline and relative effects models when placebo is the reference intervention. The second script contains the code for the baseline and relative effects models when the remaining interventions are the references. The results from both scripts are found in the folder _NMA & OMA results_ and are used from the other R scripts to create the Figures of the manuscirpt. 
  + Note that Figures 2 and S2 that refer to the forest plots from the baseline effects models under placebo and the remaining interventions, respectively, are created using the scripts __NMA & PMA for placebo.R__ and __NMA & PMA for all references.R__. The remaining scripts aim to create the main and supplementary Figures of the manuscript.
  + The _NMA & PMA results_ subfolder in the _R_ folder contains the results from the baseline effects and relative effects models, the latter under network and pairwise meta-analyses. The .RData files are called from the functions in scripts to create the main and supplementary Figures of the manuscript.<br>

After downloading/cloning the repo, the user can use the .Rproj file to source all code.

## Output 

Prerequisite R packages: [rnmamod](https://CRAN.R-project.org/package=rnmamod), [ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html), [ggpubr](https://CRAN.R-project.org/package=ggpubr), [ggrepel](https://CRAN.R-project.org/package=ggrepel), and [tidytext](https://CRAN.R-project.org/package=tidytext).
