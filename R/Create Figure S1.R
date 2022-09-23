#*******************************************************************************
#*
#*
#*           Inspect NMA and PMA summary log ORs for PBO-comparisons                     
#*
#*
#*******************************************************************************



## Load development version of 'rnmamod'
devtools::install_github("LoukiaSpin/rnmamod", force = TRUE)



## Load libraries ----
list.of.packages <- c("ggplot2", "rnmamod", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages) 



## Run RE-NMA ----
baseline <- baker[baker$t1 == 1, c(5, 9)]
re_nma <- run_model(data = baker,
                    measure = "OR",
                    model = "RE",
                    heter_prior = list("halfnormal", 0, 1),
                    D = 0,
                    ref = 1,
                    base_risk = baseline,
                    n_chains = 3,
                    n_iter = 50000,
                    n_burnin = 5000,
                    n_thin = 10)



## Separate PMA (shared tau) analysis ----
# Run the model
seriesmeta_res <- run_series_meta(re_nma)

# Present the results
res_series_ma <- series_meta_plot(re_nma, 
                                  seriesmeta_res,
                                  drug_names = c("PBO", "LABA", "ICS", "ICS+LABA", "tiotropium"))

tiff("./Figure S2.tiff", 
     height = 20, 
     width = 40, 
     units = 'cm', 
     compression = "lzw", 
     res = 600)
res_series_ma$forest_plots
dev.off()
