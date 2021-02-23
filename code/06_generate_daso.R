rm(list=ls())

library(dplyr)
library(tidyr)

load("./data/raw_data_CD.Rdata")


# Annual Dasometry by plot, scenario, species and CD class ----
   
    adult_data_CD <- raw_data_CD %>%
        filter(Type == "Adult", `Dead Code` == "Alive") 

    save(adult_data_CD, file = "./data/adult_data_CD.Rdata")


# Annual Dasometry by plot and scenario ----

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

    
# Annual Dasometry by plot, scenario and species ----

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
