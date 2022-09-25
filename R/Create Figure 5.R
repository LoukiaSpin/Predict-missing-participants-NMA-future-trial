#*******************************************************************************
#*
#*
#*              Create analysis figures: for NMA, and PMA results                          
#*              (Secondary analysis: All reference interventions)    
#*
#*
#*******************************************************************************



## Load libraries
list.of.packages <- c("ggplot2", "ggrepel")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages) 



## Load RData
load("./R/NMA & PMA results/nma_all refers.RData") # nma_risk
load("./R/NMA & PMA results/pma_all refers.RData") # abs_pmas



## Rename the coded interventions 
treat_names <- c("placebo", "LABA", "ICS", "ICS+LABA", "tiotropium")
  


## Prepare dataset for the interval plots ----
# NMA results
nma_res <- data.frame(rep(treat_names, each = length(treat_names)),
                      rep(treat_names, length(treat_names)),
                      do.call(rbind, nma_risk)[, c(1, 3, 7)])
colnames(nma_res) <- c("ref", "treat", "mean", "lower", "upper")
rownames(nma_res) <- NULL; nma_res

# PMA results
pma_res <- abs_pmas[, c(1:3, 5, 9)]
colnames(pma_res)[4:5] <- c("lower", "upper"); pma_res

# Bring both dataset together
all_res <- rbind(nma_res, pma_res) 
all_res$analysis <- rep(c("Network meta-analysis", "Pairwise meta-analysis"), 
                        c(dim(nma_res)[1], dim(pma_res)[1]))
all_res$id <- c(rep(1:5, each = 5), 1, 1, 2, 1, 2, 3, 1, 2)
all_res$treat <- factor(all_res$treat, 
                        levels = c("placebo", "LABA", "ICS", "ICS+LABA", "tiotropium"))



## Create the density plot ----
tiff("./Figure 5.tiff", 
     height = 25, 
     width = 40, 
     units = 'cm', 
     compression = "lzw", 
     res = 400)
ggplot(all_res, 
       aes(x = as.factor(id), 
           y = mean, 
           ymin = lower, 
           ymax = upper,
           group = analysis,
           colour = analysis)) + 
  geom_linerange(size = 2,
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 2.5,
             colour = "white",
             stroke = 0.3,
             position = position_dodge(width = 0.5)) +
  facet_grid(. ~ treat) +
  geom_text_repel(aes(x = as.factor(id),
                      y = mean,
                      label = round(mean * 100, 0)),
                  box.padding = 0.5, 
                  min.segment.length = 0, seed = 42, 
                  color = "black",
                  size = 4,
                  position = position_dodge(width = 0.5)) +
  scale_x_discrete(breaks = as.factor(seq_len(5)),
                   labels = treat_names) +
  scale_y_continuous(labels = c("0", "25", "50", "75", "100"),
                     limits = c(0.0, 1.00)) +
  scale_color_manual(breaks = c("Network meta-analysis", "Pairwise meta-analysis"),
                     values = c("limegreen", "royalblue")) +
  labs(x = "", y = "Predicted % missing participants", colour = "Analysis") +
  theme_classic() +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text.y = element_text(size = 12, hjust = 0),
        axis.text.x = element_text(size = 12, hjust = 1, vjust = 1, angle = 45),
        legend.position = "bottom",
        legend.text =  element_text(color = "black", size = 12),
        legend.title =  element_text(color = "black", face = "bold", size = 12),
        strip.text = element_text(size = 12, face = "bold"))
dev.off()

