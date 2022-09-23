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



## Load RData ----
# RData for NMA and PMA
load("./30_Analysis & Results/Results R analyses/nma_all_results.RData") # re_nma
load("./30_Analysis & Results/Results R analyses/pma_all_results.RData") # re_pmas

# MOD as the outcome
baker <- as.data.frame(read.table("./30_Analysis & Results/19637942_Baker(2009).txt", 
                                  header = TRUE))[, c(1:4, 9:16)]
colnames(baker) <- c("t1", "t2", "t3", "t4", "r1", "r2", "r3", "r4", "n1", "n2", "n3", "n4")



## Prepare dataset for interval plot of odds ratios ----
# Interventions compared with PBO
compar <- c("LABA", "ICS", "ICS+LABA", "tiotropium")

# PMA for PBO comparisons
pma_res0 <- list()
for (i in 1:4) {
  pma_res0[[i]] <- re_pmas[[i]]$EM
}
pma_res <- do.call(rbind, pma_res0)

# Bring together
all_results <- data.frame(cbind(rbind(re_nma$EM[1:4, c(1, 3, 7)],
                                      pma_res[, c(1, 3, 7)]),
                                analysis = rep(c("Network meta-analysis", "Pairwise meta-analysis"), each = 4),
                                comparison = rep(compar, 2),
                                id = rep(1:4, 2)))
colnames(all_results)[2:3] <- c("lower", "upper")
rownames(all_results) <- NULL



## Create the interval plot of odds ratios ----
fig_or <- ggplot(all_results, 
                 aes(x = as.factor(id), 
                     y = exp(as.numeric(mean)), 
                     ymin = exp(as.numeric(lower)), 
                     ymax = exp(as.numeric(upper)),
                     colour = analysis,
                     group = analysis)) + 
  geom_hline(yintercept = 1,
             lty = 1,
             size = 1,
             col = "grey60") +
  geom_linerange(size = 2,
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 1.5,
             colour = "white",
             stroke = 0.3,
             position = position_dodge(width = 0.5)) +
  geom_text(aes(x = as.factor(id),
                y = exp(as.numeric(mean)),
                label = paste0(sprintf("%.2f", exp(as.numeric(mean))), " ", "(",
                               sprintf("%.2f", exp(as.numeric(lower))), ",", " ",
                               sprintf("%.2f", exp(as.numeric(upper))), ")"),
                hjust = 0,
                vjust = -0.5),
            color = "black",
            size = 4.0,
            position = position_dodge(width = 0.5)) +
  scale_x_discrete(breaks = as.factor(seq_len(4)),
                   labels = compar) +
  scale_color_manual(breaks = c("Network meta-analysis", "Pairwise meta-analysis"),
                     values = c("black", "royalblue")) +
  labs(x = "", y = "Odds ratio", colour = "Analysis") +
  scale_y_continuous(breaks = seq(0.15, 1.50, by = 0.2)) + #0.1
  coord_flip() +
  theme_classic() +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12), 
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 12))



## Prepare dataset for interval plot of taus ----
# PBO comparisons
compar2 <- paste(compar, "\n versus PBO")

# Posterior distribution of tau per PBO coomparison
pma_tau0 <- list()
for (i in 1:4) {
  pma_tau0[[i]] <- re_pmas[[i]]$tau
}
pma_tau <- do.call(rbind, pma_tau0)

# Bring together
all_res_taus <- data.frame(pma_tau[, c(5, 3, 7)], 
                           comparison = compar2)
colnames(all_res_taus)[1:3] <- c("median", "lower", "upper")
rownames(all_res_taus) <- NULL



## Create the interval plot of taus ----
fig_tau <- ggplot(all_res_taus, 
                  aes(x = 1:4, 
                      y = median, 
                      ymin = lower, 
                      ymax = upper)) + 
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = 0, ymax = 0.099,
                fill = "low"),
            alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = 0.1, ymax = 0.5,
                fill = "reasonable"),
            alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = 0.5, ymax = 1.0,
                fill = "fairly high"),
            alpha = 0.02) +
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = 1.0, ymax = Inf,
                fill = "fairly extreme"),
            alpha = 0.02) +
  geom_hline(yintercept = re_nma$tau[5], # re_nma$tau
             lty = 1,
             size = 1,
             col = "#006CD1") +
  geom_hline(yintercept = re_nma$tau[3],
             lty = 3,
             size = 1,
             col = "#006CD1") +
  geom_hline(yintercept = re_nma$tau[7],
             lty = 3,
             size = 1,
             col = "#006CD1") +
  geom_linerange(size = 2,
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 2.5,
             colour = "white",
             stroke = 0.3,
             position = position_dodge(width = 0.5)) +
  geom_text(aes(x = 1:4,
                y = median,
                label = paste0(sprintf("%.2f", median), " ", "(",
                               sprintf("%.2f", lower), ",", " ",
                               sprintf("%.2f", upper), ")"),
                hjust = 0,
                vjust = -0.5),
            color = "black",
            size = 4.0,
            position = position_dodge(width = 0.5)) +
  scale_x_continuous(breaks = 1:4,
                     labels = all_res_taus$comparison) +
  scale_fill_manual(name = "Heterogeneity",
                    values = c("low" = "#009E73",
                               "reasonable" = "orange",
                               "fairly high" = "#D55E00",
                               "fairly extreme" = "red")) +
  labs(x = "", y = "Between-trial standard deviation") +
  coord_flip(ylim = c(-0.01, 1.30), expand = FALSE) +
  theme_classic() +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12),
        axis.text.y = element_text(size = 12, hjust = 0),
        legend.position = "bottom",
        legend.text =  element_text(color = "black", size = 12),
        legend.title =  element_text(color = "black", face = "bold",
                                     size = 12))



## Bring both plots together ----
tiff("./30_Analysis & Results/Figure S1.tiff", 
     height = 20, 
     width = 40, 
     units = 'cm', 
     compression = "lzw", 
     res = 600)
ggarrange(fig_or, fig_tau, 
          labels = c("a)", "b)"),
          common.legend = FALSE)
dev.off()



## Node-splitting analysis ----
# Run the model
nodesplit_res <- run_nodesplit(re_nma)

# Present the results
nodesplit_plot(re_nma, 
               nodesplit_res,
               drug_names = c("PBO", "LABA", "ICS", "ICS+LABA", "tiotropium"))



## Separate PMA (shared tau) analysis ----
# Run the model
seriesmeta_res <- run_series_meta(re_nma)

# Present the results
res_series_ma <- series_meta_plot(re_nma, 
                                  seriesmeta_res,
                                  drug_names = c("PBO", "LABA", "ICS", "ICS+LABA", "tiotropium"))

tiff("./30_Analysis & Results/Figure S3.tiff", 
     height = 20, 
     width = 40, 
     units = 'cm', 
     compression = "lzw", 
     res = 600)
res_series_ma$forest_plots
dev.off()


