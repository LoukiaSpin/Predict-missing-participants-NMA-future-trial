#*******************************************************************************
#*
#*
#*            Create analysis figures: for NMA, PMA and trial results                       
#*           (Primary analysis: placebo as the reference intervention)   
#*
#*
#*******************************************************************************



## Load libraries
list.of.packages <- c("ggplot2", "tidytext")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages) 



## Load RData
load("./R/NMA & PMA results/nma_results.RData") # nma_risk
load("./R/NMA & PMA results/pma_results.RData") # abs_pmas
load("./R/NMA & PMA results/trial_results.RData") # trial_arm_data



## Rename the coded interventions 
trial_arm_data$treat <- as.factor(trial_arm_data$treat)
levels(trial_arm_data$treat) <- c("placebo", "LABA", "ICS", "ICS+LABA", "tiotropium")
  


## Prepare dataset for the density plots ----
# Keep only PMA results on non-PBO interventions
abs_pmas_new <- subset(as.data.frame(abs_pmas), treat > 1)

# Turn into logits for normal approximation
point_logit <- c(log(nma_risk[, 1] / (1 - nma_risk[, 1])), 
                 log(abs_pmas_new[, 3] / (1 - abs_pmas_new[, 3])))
sd_logit <- c((nma_risk[, 2]/nma_risk[, 1] * (1 - nma_risk[, 1])), 
              (abs_pmas_new[, 4]/abs_pmas_new[, 3] * (1 - abs_pmas_new[, 3])))

# Number of unique interventions
n_treat <- length(unique(abs_pmas[, 2]))

# Create a dataframe of NMA % MOD per intervention for the density plot
time_nma <- time_nma0 <- prob_nma <- list()
for (i in 1:length(nma_risk[, 1])) {
  time_nma0[[i]] <- seq(point_logit[i] - 3.0*sd_logit[i], point_logit[i] + 3.0*sd_logit[i], 0.1) 
  time_nma[[i]] <- exp(time_nma0[[i]]) / (1 + exp(time_nma0[[i]]))
  prob_nma[[i]] <- dnorm(time_nma0[[i]], point_logit[i], sd_logit[i])
}
out_nma <- as.data.frame(cbind(unlist(time_nma), 
                               unlist(prob_nma), 
                               rep(1:n_treat, do.call(cbind, lapply(time_nma, function(x) length(x))))))
colnames(out_nma) <- c("time", "prob", "treat") 
  
# Create a dataframe of PMA % MOD per intervention for the density plot
time_pma <- time_pma0 <- prob_pma <- list()
for (i in 1:length(abs_pmas_new[, 1])) {
  time_pma0[[i]] <- seq(point_logit[(length(nma_risk[, 1]) + i)] - 3.0*sd_logit[(length(nma_risk[, 1]) + i)], 
                        point_logit[(length(nma_risk[, 1]) + i)] + 3.0*sd_logit[(length(nma_risk[, 1]) + i)], 0.1) 
  time_pma[[i]] <- exp(time_pma0[[i]]) / (1 + exp(time_pma0[[i]]))
  prob_pma[[i]] <- dnorm(time_pma0[[i]], point_logit[(length(nma_risk[, 1]) + i)], sd_logit[(length(nma_risk[, 1]) + i)])
}
time_pma_new <- append(list(time_nma[[1]]), time_pma)
prob_pma_new <- append(list(prob_nma[[1]]), prob_pma)
out_pma <- as.data.frame(cbind(c(unlist(time_pma_new)), 
                               c(unlist(prob_pma_new)), 
                               rep(1:n_treat, do.call(cbind, lapply(time_pma_new, function(x) length(x))))))
colnames(out_pma) <- c("time", "prob", "treat") 

# Bring both dataframe together
all_res <- cbind(rbind(out_nma, out_pma), 
                 rep(c("Network meta-analysis", "Pairwise meta-analysis"), c(dim(out_nma)[1], dim(out_pma)[1])))
colnames(all_res)[4] <- "analysis" 
all_res$treat <- as.factor(all_res$treat)
levels(all_res$treat) <- c("placebo", "LABA", "ICS", "ICS+LABA", "tiotropium")



## Create the density plot ----
tiff("./Figure 3.tiff", 
     height = 20, 
     width = 30, 
     units = 'cm', 
     compression = "lzw", 
     res = 600)
ggplot(data = all_res, 
       aes(x = time, 
           y = prob, 
           colour = analysis)) + 
  geom_rect(aes(xmin = 0, xmax = 0.05, ymin = 0, ymax = Inf, 
                fill = "low"), 
            alpha = 0.005, colour = "#009E73") +
  geom_rect(aes(xmin = 0.05, xmax = 0.20, ymin = 0, ymax = Inf, 
                fill = "moderate"), 
            alpha = 0.005, colour = "orange") +
  geom_rect(aes(xmin = 0.20, xmax = 0.50, ymin = 0, ymax = Inf, 
                fill = "high"), 
            alpha = 0.005, colour = "#D55E00") +
  geom_line(aes(time, prob), 
            linewidth = 1.5) +
  geom_vline(data = trial_arm_data,
             aes(xintercept = m_perc),
             colour = "black",
             linetype = 3,
             linewidth = 0.8) +
  facet_grid(treat ~.) +
  scale_color_manual(values = c("black", "royalblue")) +
  labs(x = "predicted and observed % missing participants",
       y = "density",
       colour = "Analysis") +
  scale_fill_manual(name = "Heterogeneity",
                    values = c("low" = "#009E73",
                               "moderate" = "orange",
                               "high" = "#D55E00")) +
  scale_x_continuous(breaks = seq(0, 0.50, by = 0.05)) +
  coord_cartesian(xlim = c(-0.004, 0.50), expand = FALSE) +
  guides(fill = "none") +
  theme_bw() +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12), 
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12), 
        strip.text = element_text(size = 12))
dev.off()



## Prepare data for the barplots (% difference) ----
# Percentage difference of % MOD (from each trial to NMA)
interv <- levels(trial_arm_data$treat)
perc_diff <- trial_id <- list()
for (i in 1:n_treat) {
  perc_diff[[i]] <- round(((subset(trial_arm_data, treat == interv[i])[, 3] - nma_risk[i, 1]) / nma_risk[i, 1]) * 100, 0)
  trial_id[[i]] <- subset(trial_arm_data, treat == interv[i])[, 1]
}

# Bring all together
out_diff <- data.frame(trial_id = unlist(trial_id), 
                         diff = unlist(perc_diff),
                         treat = rep(interv, do.call(cbind, lapply(perc_diff, function(x) length(x)))))
out_diff$thres <- ifelse(unlist(perc_diff) > 0, "high", "low")
  
# Specify intervention order
out_diff$treat <- factor(out_diff$treat, levels = interv)



## Create the barplots ----
tiff("./Figure 4.tiff", 
     height = 20, 
     width = 30, 
     units = 'cm', 
     compression = "lzw", 
     res = 600)
ggplot(out_diff, 
       aes(x = reorder_within(trial_id, by = diff, within = treat),
           y = diff, 
           fill = thres)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = diff, 
            vjust = ifelse(diff >= 0, -0.2, 1)),
            size = 3.6) +
  scale_fill_manual(values = c("low" = "#009E73", "high" = "#D55E00")) +
  scale_x_reordered() +
  facet_grid(. ~ treat, 
             scales = "free_x",
             space = "free_x") +
  labs(x = "Trial ID",
       y = "% difference in missing participants (from a trial to NMA)") +
  scale_y_continuous(breaks = seq(-100, 150, by = 25)) +
  guides(fill = "none") +
  theme_bw() +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12), 
        strip.text = element_text(size = 12))
dev.off()

