#*******************************************************************************
#*                                                                                  
#*                     Heatmaps of missing participants                     
#*                                                                                  
#*******************************************************************************



## Load libraries
list.of.packages <- c("rnmamod", "ggpubr")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages) 



## Load data
baker <- as.data.frame(read.table("./data/19637942_Baker(2009).txt", 
                                  header = TRUE))[, -17]



## Interventions names
baker_names <- c("placebo", "LABA", "ICS", "ICS+LABA", "tiotropium")



## Heatmap missing participants in the dataset
mp_dataset <- heatmap_missing_dataset(data = baker,
                                      trial_names = 1:21,
                                      drug_names = baker_names)
mp_network <- heatmap_missing_network(data = baker,
                                      drug_names = baker_names)



## Create the network plot
tiff("./Figure 1.tiff", 
     height = 20, 
     width = 40, 
     units = 'cm', 
     compression = "lzw", 
     res = 600)
ggarrange(mp_dataset, mp_network, 
          labels = c("a)", "b)"),
          common.legend = TRUE,
          legend = "bottom")
dev.off()

