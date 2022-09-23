#*******************************************************************************
#*                                                                                  
#*                     Network plots for the Motivating Example                     
#*                                                                                  
#*******************************************************************************



## Load libraries
list.of.packages <- c("readxl", "rnmamod")
lapply(list.of.packages, require, character.only = TRUE); rm(list.of.packages) 



## Load data
baker <- as.data.frame(read_excel("./30_Analysis & Results/Baker_Dataset.xlsx", na = "NA", sheet = "Network_plot"))[, 7:18]
colnames(baker) <- c(paste0("t..", 1:4, "."), paste0("r..", 1:4, "."), paste0("n..", 1:4, "."))



## Interventions names
baker_names <- c("placebo", "LABA", "ICS", "ICS plus \n LABA", "tiotropium")



## Create the network plot
tiff("./30_Analysis & Results/Figure 1.tiff", 
     height = 20, 
     width = 20, 
     units = 'cm', 
     compression = "lzw", 
     res = 600)
netplot(baker,
        drug_names = baker_names,
        text.cex = 1.5)
dev.off()

