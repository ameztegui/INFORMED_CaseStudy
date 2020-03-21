rm(list=ls())

library(dplyr)
library(tidyr)
# raw_data_CD.Rdata --> HECHO
# data_plot.Rdata
# data_plot_species.Rdata
# annual_climate.Rdata --> HECHO


# load("./data/pn.rdata")
# load("./data/ps.rdata")
# load("./data/mx.rdata")

load("./data/raw_data_CD.Rdata")

data_CD <- raw_data_CD %>%
    filter(Type == "Adult", `Dead Code` == "Alive") 

save(data_CD, file = "./data/data_CD.Rdata")

data_plot <- raw_data_CD %>%
    filter(Type == "Adult", `Dead Code` == "Alive") %>%
    group_by(Parcela,  Forest_Type, Climate_Model, RCP, Management,  Narrative, Recipe, Timestep) %>%
    summarise(DBH = weighted.mean(DBH, N, na.rm=T),
              Height = weighted.mean(Height, N, na.rm = T),
              N = sum(N),
              BA = sum(BA)) %>%
    ungroup() %>%
    complete(Timestep, nesting(Parcela, Forest_Type, Climate_Model, RCP, 
                               Management,  Narrative, Recipe),
             fill= list(N= 0, DBH = NA, BA=0 ))
    
save(data_plot, file = "./data/data_plot.Rdata")


data_plot_species <- raw_data_CD %>%
    filter(Type == "Adult", `Dead Code` == "Alive") %>%
    group_by(Parcela, Forest_Type, Climate_Model, RCP, 
             Management,  Narrative, Recipe, Species, Timestep) %>%
    summarise(DBH = weighted.mean(DBH, N, na.rm=T),
              Height = weighted.mean(Height, N, na.rm = T),
              N = sum(N),
              BA = sum(BA)) %>%
    ungroup() %>%
    complete( Timestep, nesting(Parcela,  Forest_Type, Climate_Model, RCP, 
                                Management,  Narrative, Recipe, Species),
              fill= list(N= 0, DBH = NA, BA=0 ))
save(data_plot_species, file = "./data/data_plot_species.Rdata")




 
 
 

 
