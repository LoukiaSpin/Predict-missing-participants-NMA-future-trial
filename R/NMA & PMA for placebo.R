#*******************************************************************************
#*
#*
#*      Obtained predicted % MOD in NMA & PMA and observed % MOD in trials 
#*          (Primary analysis: placebo as the reference intervention)                      
#*
#*
#*******************************************************************************



## Load development version of 'rnmamod'
devtools::install_github("LoukiaSpin/rnmamod", force = TRUE)



## Load libraries
list.of.packages <- c("rnmamod", "ggplot2")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages) 



## Load function (baseline_model)
source("./31_Functions/baseline.model_fun.R")



## Load data ----
# MOD as the outcome
baker <- as.data.frame(read.table("./30_Analysis & Results/19637942_Baker(2009).txt", 
                                  header = TRUE))[, c(1:4, 9:16)]
colnames(baker) <- c("t1", "t2", "t3", "t4", "r1", "r2", "r3", "r4", "n1", "n2", "n3", "n4")

# Keep only PBO-controlled trials 
baker_pbo <- subset(baker, t1 == "1")



## Median and range of MOD in placebo ----
# Per intervention
netplot(baker_pbo,
        drug_names = c("placebo", "LABA", "ICS", "ICS+LABA", "tiotropium"))$table_interventions

# Per comparison
netplot(baker_pbo,
        drug_names = c("placebo", "LABA", "ICS", "ICS+LABA", "tiotropium"))$table_comparison



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
mcmc_diagnostics(re_nma, par = c("tau", "EM[2,1]"))



## Save NMA results as .RData
# Only predicted risks
nma_risk <- re_nma$abs_risk
save(nma_risk, 
     file = "./30_Analysis & Results/Results R analyses/nma_results.RData")
# All results
#save(re_nma, 
#     file = "./30_Analysis & Results/Results R analyses/nma_all_results.RData")



## Plot of baseline model
base_mod <- baseline_model(base_risk = baseline,
                           n_chains = 3,
                           n_iter = 50000,
                           n_burnin = 5000,
                           n_thin = 10)
# Get the plot
tiff("./30_Analysis & Results/Figure 2.tiff", 
     height = 20, 
     width = 24, 
     units = 'cm', 
     compression = "lzw", 
     res = 600)
base_mod$figure
dev.off()



## Several PMAs ----
# Keep only PBO-controlled trials 
dataset_pbo <- data_preparation(data = baker_pbo,
                                measure = "OR")

# Function to turn wide- to long-format for an element
log_format <- function (input) {
  if (length(input[1, ]) > 2) {
    long_form0 <- apply(input, 1, function(x) {combn(na.omit(x), 2)})
    long_form <- t(do.call(cbind, long_form0))
  } else {
    long_form <- input
  }
  return(long_form)
}

# Create the dataset per observed PBO comparison
t_long_form <- log_format(dataset_pbo$t)
r_long_form <- log_format(dataset_pbo$r)
n_long_form <- log_format(dataset_pbo$N)
pairwise_data0 <- data.frame(t_long_form, r_long_form, n_long_form)
colnames(pairwise_data0) <- c("arm1", "arm2", "r1", "r2", "n1", "n2")
pairwise_data <- subset(pairwise_data0, arm1 == "1")

# Control arm
pairwise_data$t1 <- rep(1, dim(pairwise_data)[1])
# Experimental arm
pairwise_data$t2 <- rep(2, dim(pairwise_data)[1])

# Observed comparisons in the network
comp <- as.data.frame(
  table(paste0(pairwise_data$arm1, "vs", pairwise_data$arm2)))
colnames(comp) <- c("comparison", "frequency")

# Indicate all observed comparisons
obs_comp0 <- unique(pairwise_data[, 1:2])
obs_comp <- obs_comp0[order(obs_comp0$arm2), ]
n_obs_comp <- dim(obs_comp)[1]

# Run separate pairwise meta-analyses with baseline model
re_pmas <- list()
for (i in 1:n_obs_comp) {
  message(paste(i, "out of", n_obs_comp, "observed comparisons"))
  # 'D' does not matter in pairwise meta-analysis
  re_pmas[[i]] <- run_model(data = pairwise_data[pairwise_data$arm1 == obs_comp[i, 1] &
                                                   pairwise_data$arm2 == obs_comp[i, 2], ],
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
}
mcmc_diagnostics(re_pmas[[4]], par = c("tau", "EM[2,1]"))

# Obtain results in data-frame
abs_pmas0 <- list() 
for (i in 1:4) {
  abs_pmas0[[i]] <- re_pmas[[i]]$abs_risk
}
abs_pmas <- cbind(rep(1:length(obs_comp[, 1]), each = 2), 
                  matrix(t(obs_comp), ncol = 1), 
                  do.call(rbind, abs_pmas0))
colnames(abs_pmas)[1:2] <- c("comp", "treat")
rownames(abs_pmas) <- NULL



## Save PMA results as .RData
# Only predicted risks
save(abs_pmas, 
     file = "./30_Analysis & Results/Results R analyses/pma_results.RData")
# All results
#save(re_pmas, 
#     file = "./30_Analysis & Results/Results R analyses/pma_all_results.RData")



## Several trials ----
## Calculate % MOD per trial-arm 
trial_arm_mod <- transform(baker, 
                           new = round((baker[, 5:8] / baker[, 9:12]), 3))

# Indicate the PBO trials
cond <- na.omit(c(rep(ifelse(baker[, 1] == "1", 1, 0), 2),
          ifelse(baker[, 1] == "1", baker[, 1]*baker[, 3], 0*baker[, 3]),
          ifelse(baker[, 1] == "1", baker[, 1]*baker[, 4], 0*baker[, 4])))

# Create the dataset
trial_id <- c(rep(rownames(trial_arm_mod), 2),
              which(trial_arm_mod[, 3] %in% na.omit(trial_arm_mod[, 3])), 
              which(trial_arm_mod[, 4] %in% na.omit(trial_arm_mod[, 4])))
trial_t <- na.omit(unlist(trial_arm_mod[, 1:4]))
trial_m <- na.omit(unlist(trial_arm_mod[, 13:16]))
trial_arm_data0 <- cbind(trial_id, trial_t, trial_m)
colnames(trial_arm_data0) <- c("trial", "treat", "m_perc")

# Keep only trial-arms from PBO-controlled trials
trial_arm_data1 <- subset(trial_arm_data0, cond > 0)

# Turn into numeric
trial_arm_data <- data.frame(apply(trial_arm_data1, 2, function(x) as.numeric(as.character(x))))



## Save trial results as .RData
save(trial_arm_data, 
     file = "./30_Analysis & Results/Results R analyses/trial_results.RData")
