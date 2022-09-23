#*******************************************************************************
#*
#*
#*      Obtained predicted % MOD in NMA & PMA and observed % MOD in trials
#*             (Secondary analysis: All reference interventions)                              
#*
#*
#*******************************************************************************



## Load development version of 'rnmamod'
devtools::install_github("LoukiaSpin/rnmamod", force = TRUE)



## Load libraries
list.of.packages <- c("rnmamod", "ggplot2", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages) 




## Load data ----
# MOD as the outcome
baker <- as.data.frame(read.table("./data/19637942_Baker(2009).txt", 
                                  header = TRUE))[, c(1:4, 9:16)]
colnames(baker) <- c("t1", "t2", "t3", "t4", "r1", "r2", "r3", "r4", "n1", "n2", "n3", "n4")
treat_names <- c("placebo", "LABA", "ICS", "ICS+LABA", "tiotropium")



## Median and range of MOD ----
# Per intervention
netplot(baker,
        drug_names = treat_names)$table_interventions



## Turn wide- into long-format with 'unlist' (one row per trial-arm) ---
t_long <- na.omit(unlist(baker[, 1:4]))
r_long <- na.omit(unlist(baker[, 5:8]))
n_long <- na.omit(unlist(baker[, 9:12]))
baseline0 <- data.frame(t_long, r_long, n_long)
colnames(baseline0) <- c("t", "r", "n")



## Run RE-NMA ----
baseline <- re_nma <- list()
for (i in 1:length(treat_names)) {
  message(paste(i, "out of", length(treat_names), "interventions"))
  baseline[[i]] <- subset(baseline0, t == i)[, 2:3]
  re_nma[[i]] <- run_model(data = baker,
                           measure = "OR",
                           model = "RE",
                           heter_prior = list("halfnormal", 0, 1),
                           D = 0,
                           ref = i,
                           base_risk = baseline[[i]],
                           n_chains = 3,
                           n_iter = 50000,
                           n_burnin = 5000,
                           n_thin = 10)
}
names(baseline) <- names(re_nma) <- treat_names
mcmc_diagnostics(re_nma[[1]], par = c("tau", "EM[2,1]"))



## Save NMA results as .RData
# Only predicted risks
nma_risk <- list()
for (i in 1:length(treat_names)) {
  nma_risk[[i]] <- re_nma[[i]]$abs_risk
}
names(nma_risk) <- treat_names
#(nma_risk, 
#     file = "./R/Results R analyses/nma_all refers.RData")



## Plot of baseline model ----
base_mod <- list()
for (i in 1:length(treat_names)) {
  message(paste(i, "out of", length(treat_names), "interventions"))
  base_mod[[i]] <- baseline_model(base_risk = baseline[[i]],
                                  n_chains = 3,
                                  n_iter = 50000,
                                  n_burnin = 5000,
                                  n_thin = 10)
}

# Get the baseline plot 
tiff("./Figure S2.tiff", 
     height = 30, 
     width = 43, 
     units = 'cm', 
     compression = "lzw", 
     res = 600)
ggarrange(base_mod[[2]]$figure, 
          base_mod[[3]]$figure, 
          base_mod[[4]]$figure, 
          base_mod[[5]]$figure,
          common.legend = TRUE, 
          legend = "bottom", 
          labels = c("a)", "b)", "c)", "d)"))
dev.off()



## Several PMAs ----
# Function to turn wide- to long-format for an element ----
log_format <- function (input) {
  if (length(input[1, ]) > 2) {
    long_form0 <- apply(input, 1, function(x) {combn(na.omit(x), 2)})
    long_form <- t(do.call(cbind, long_form0))
  } else {
    long_form <- input
  }
  return(long_form)
}

# Re-arrange so that t1 < t2 
dataset <- data_preparation(data = baker,
                            measure = "OR")

# Create the dataset per observed comparison ----
t_long_form <- log_format(dataset$t)
r_long_form <- log_format(dataset$r)
n_long_form <- log_format(dataset$N)
pairwise_data <- data.frame(t_long_form, r_long_form, n_long_form)
colnames(pairwise_data) <- c("arm1", "arm2", "r1", "r2", "n1", "n2")

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
obs_comp <- obs_comp0[order(obs_comp0$arm2, obs_comp0$arm1), ]
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
                            base_risk = baseline[[obs_comp[i, 1]]],
                            n_chains = 3,
                            n_iter = 50000,
                            n_burnin = 5000,
                            n_thin = 10)
}

# Obtain results in data-frame
abs_pmas0 <- list() 
for (i in 1:n_obs_comp) {
  abs_pmas0[[i]] <- re_pmas[[i]]$abs_risk[2, ]
}
abs_pmas <- data.frame(treat_names[obs_comp$arm1], 
                       treat_names[obs_comp$arm2], 
                       do.call(rbind, abs_pmas0))
colnames(abs_pmas)[1:2] <- c("ref", "treat")
rownames(abs_pmas) <- NULL



## Save PMA results as .RData
# Only predicted risks
save(abs_pmas, 
     file = "./30_Analysis & Results/Results R analyses/pma_all refers.RData")
