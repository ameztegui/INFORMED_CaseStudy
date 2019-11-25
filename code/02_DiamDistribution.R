# Script to define the diameter distribution of all plots with pue Pinus nigra stands in Solsones

rm(list=ls())

source("./R scripts/1. Regularity.R")

library(data.table)
library(trelliscopejs)

palette(brewer.pal(n = 8, name = "Set2"))



# Extract the info in the S4 object to a list
pn_trees <- (lapply(pn_forestlist,function(l) l[[3]]))

# Transform the info in a data frame
pn_plots <- rbindlist(pn_trees, use.names=TRUE, fill=TRUE, idcol="Plot")

pn_plots$CD <- cut(pn_plots$DBH, breaks = c(0, 0.3, 1, 3, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5,
                                      37.5, 42.5, 47.5, 52.5))
pn_plots$CD <- factor(pn_plots$CD,labels = c("0.1","0.5","1.5", "5.0", "10", "15","20", "25","30", "35", "40","45","50"))
pn_plots$Species <- as.factor(pn_plots$Species)


# Create diameter distribution (without species)
   
            pn_plots %>%
            group_by (Plot, Species, CD) %>%
            summarise(n = sum(N)) %>%
            filter(CD %in% c("10":"50"))  %>%
            ggplot(aes(x=CD, y=n)) +
                  geom_col(aes(fill=Species)) +
                  scale_fill_brewer(palette="Set3") + 
                  facet_trelliscope(~ Plot , nrow = 2, ncol = 5, width = 300)
      
    
# Plot diameter distribution for SORTIE files
           bind_rows(pn_sortie_A) %>%
                  mutate(Plot= names(pn_dd)) %>%
                  gather(CD, n, -Plot) %>%
                  arrange(Plot, CD) %>%
                  ggplot(aes(x=CD, y=n)) +
                  geom_col() +
                  scale_fill_brewer(palette="Set3") + 
                  facet_trelliscope(~ Plot , nrow = 2, ncol = 5, width = 300)
            
              
                  
            
                  
            
            