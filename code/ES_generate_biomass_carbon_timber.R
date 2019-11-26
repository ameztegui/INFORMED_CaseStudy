rm(list=ls())

library(dplyr)
library(purrr)
library(ggplot2)
library(tidyr)
library(readr)
library(forcats)


load("./data/raw_data_CD.Rdata")

dendro_vol <- read_delim("./data/dendro_vol.csv", delim = ";", locale =locale(decimal_mark = "."))
dendro_dens <- read_delim("./data/dendro_dens.csv", delim = ";", locale =locale(decimal_mark = "."))

data_daso <- raw_data_CD %>%
    left_join(dendro_vol, by = c("Species", "CD")) %>%
    left_join(dendro_dens, by = c("Species"))

# densidad de madera y corteza    | kg/dm3
# concentracion de carbono        | %
# espesor de corteza              | mm
# biomasa ramas                   | kg
# biomasa hojas                   | kg

# Vamos a calcular todo en kg y para cada arbol, y lo pasaremos a toneladas por ha después

timber_arbol <- data_daso %>%
    filter(Type %in% c("Adult", "Snag"),
           `Dead Code` != "Natural") %>%
    ungroup() %>%
    mutate(Type = if_else(`Dead Code` == "Harvest", "Harvested",
                          if_else(Type== "Adult", "Alive",
                                  if_else(Type == "Snag", "Dead", "ZZ"))),
           TotalV = pi/4 * (DBH/10)^2 * Height*10 * Kfk ,
           TrunkV = pi/4 * ((DBH-(2*Btk/10))/10)^2 * Height*10 * Kfk,
           TrunkBio =  TrunkV * DensTrunk,
           BarkV = TrunkV - (pi/4 * ((DBH-4*Btk/10)/10)^2 * Height*10 * Kfk),
           BarkBio =  BarkV * DensBark,
           Biomass = TrunkBio + BarkBio + BranchBio + LeafBio,
           AboveC = (TrunkC/100) * TrunkBio + (BarkC/100) * BarkBio + (BranchC/100) * BranchBio + (LeafC/100) * LeafBio,
           TotalC = (1 + Proot) * AboveC,
           BelowC = TotalC - AboveC, 
           TotalBio = exp(BT_SEE^2/2) * exp(BT_a) * DBH^BT_b)
           
timber_plot <- timber_arbol %>%
    mutate(TotalV = TotalV * N /1000,
           TotalB = Biomass * N /1000,
           TotalC = TotalC * N/1000) %>%
    group_by(Parcela, Forest_Type, Climate_Model, RCP, Management, Narrative,
             Recipe, Timestep, Type) %>%
    summarise(TotalV = sum(TotalV, na.rm=T),
              TotalB = sum(TotalB, na.rm=T),
              TotalC = sum(TotalC, na.rm=T)) %>%
    ungroup() %>%
    complete( Timestep, nesting(Parcela,  Forest_Type, Climate_Model, RCP, Management,  Narrative, 
                                Recipe, Type),
              fill= list(TotalV= 0, TotalB = 0, TotalC=0 )) %>%
    arrange(Parcela, Narrative, Timestep) %>%
    pivot_wider(names_from = Type, 
                values_from = c(TotalV, TotalB, TotalC),
                values_fill = list(TotalV = 0,
                                   TotalB = 0,
                                   TotalC = 0)) %>%
    group_by(Parcela, Forest_Type, Climate_Model, RCP, Management, Narrative,
             Recipe) %>%
    mutate(TotalC = TotalC_Alive + TotalC_Dead,
           V_Inc = TotalV_Alive  + TotalV_Dead + TotalV_Harvested - lag(TotalV_Alive),
           B_Inc = TotalB_Alive  + TotalB_Dead + TotalB_Harvested - lag(TotalB_Alive),
           C_Inc = TotalC_Alive  + TotalC_Dead + TotalC_Harvested - lag(TotalC_Alive)) %>%
    ungroup() %>%
    mutate(Management = as.factor(Management),
           Management = fct_relevel(Management, "BAU", "BIO", "CAR", "ADA"),
           Forest_Type = as.factor(Forest_Type),
           Forest_Type = fct_relevel(Forest_Type, "Pinus nigra", "Mixed forest", "Pinus sylvestris"))

save(timber_plot, file="./data/ES_data/timber_plot.Rdata")
