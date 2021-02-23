
library(trelliscopejs)
library(here)
library(tidyverse)

load("./data/pn.rdata")
load("./data/ps.rdata")
load("./data/mx.rdata")

all_forest_list <-  c(pn_forestlist, ps_forestlist, mx_forestlist)
# all_forest_list <- all_forest_list[order(names(all_forest_list))]

load("./data/data_plot.Rdata")
load("./data/data_plot_species.Rdata")



bluewater<-function(swbres, relative = FALSE) {
    m= matrix(NA, nrow=length(swbres), ncol=100)
    for(i in 1:length(swbres)) {
        m[i,] = swbres[[i]]$Runoff + swbres[[i]]$DeepDrainage
        if(relative) m[i,] = m[i,]/swbres[[i]]$Precipitation
    }
    rownames(m) = names(swbres)
    return(as.vector(m))
}

transpiration<-function(swbres, relative = FALSE) {
    m= matrix(NA, nrow=length(swbres), ncol=100)
    for(i in 1:length(swbres)) {
        m[i,] = swbres[[i]]$Eplanttot
        if(relative) m[i,] = m[i,]/swbres[[i]]$Precipitation
    }
    rownames(m) = names(swbres)
    return(as.vector(m))
}

year = gl(100,261,labels=2001:2100)
parcela = rep(names(all_forest_list),times=100)
for_type = ifelse(parcela %in% names(pn_forestlist),"Pinus nigra", 
                  ifelse(parcela %in% names(ps_forestlist),"Pinus sylvestris",
                         ifelse(parcela %in% names(mx_forestlist),"Mixed forest", NA)))

## CCLM 4.5
load("./data/SWBOutput/CCLM/SWB_CCLM_BAU_45.Rdata")
blue_cclm_bau_45 = bluewater(swbres)
bluerel_cclm_bau_45 = bluewater(swbres, relative = TRUE)

load("./data/SWBOutput/CCLM/SWB_CCLM_CAR_45.Rdata")
blue_cclm_CAR_45 = bluewater(swbres) 
bluerel_cclm_CAR_45 = bluewater(swbres, relative = TRUE)
bluerelBAU_cclm_CAR_45 = (bluerel_cclm_CAR_45 - bluerel_cclm_bau_45)

load("./data/SWBOutput/CCLM/SWB_CCLM_BIO_45.Rdata")
blue_cclm_BIO_45 = bluewater(swbres) 
bluerel_cclm_BIO_45 = bluewater(swbres, relative = TRUE)
bluerelBAU_cclm_BIO_45 = (bluerel_cclm_BIO_45 - bluerel_cclm_bau_45)

load("./data/SWBOutput/CCLM/SWB_CCLM_ADA_45.Rdata")
blue_cclm_ADA_45 = bluewater(swbres) 
bluerel_cclm_ADA_45 = bluewater(swbres, relative = TRUE)
bluerelBAU_cclm_ADA_45 = (bluerel_cclm_ADA_45 - bluerel_cclm_bau_45)

## CCLM 8.5
load("./data/SWBOutput/CCLM/SWB_CCLM_BAU_85.Rdata")
blue_cclm_bau_85 = bluewater(swbres) 
bluerel_cclm_bau_85 = bluewater(swbres, relative = TRUE)

load("./data/SWBOutput/CCLM/SWB_CCLM_CAR_85.Rdata")
blue_cclm_CAR_85 = bluewater(swbres) 
bluerel_cclm_CAR_85 = bluewater(swbres, relative = TRUE)
bluerelBAU_cclm_CAR_85 = (bluerel_cclm_CAR_85 - bluerel_cclm_bau_85)

load("./data/SWBOutput/CCLM/SWB_CCLM_BIO_85.Rdata")
blue_cclm_BIO_85 = bluewater(swbres) 
bluerel_cclm_BIO_85 = bluewater(swbres, relative = TRUE)
bluerelBAU_cclm_BIO_85 = (bluerel_cclm_BIO_85 - bluerel_cclm_bau_85)

load("./data/SWBOutput/CCLM/SWB_CCLM_ADA_85.Rdata")
blue_cclm_ADA_85 = bluewater(swbres) 
bluerel_cclm_ADA_85 = bluewater(swbres, relative = TRUE)
bluerelBAU_cclm_ADA_85 = (bluerel_cclm_ADA_85 - bluerel_cclm_bau_85)

## RCA4 4.5
# load("./data/SWBOutput/CCLM/SWB_RCA4_BAU_45.Rdata")
# blue_rca4_bau_45 = bluewater(swbres) 
# bluerel_rca4_bau_45 = bluewater(swbres, relative = TRUE)
# 
# load("./data/SWBOutput/CCLM/SWB_RCA4_CAR_45.Rdata")
# blue_rca4_CAR_45 = bluewater(swbres) 
# bluerel_rca4_CAR_45 = bluewater(swbres, relative = TRUE)
# bluerelBAU_rca4_CAR_45 = (bluerel_rca4_CAR_45-bluerel_rca4_bau_45)
# 
# load("./data/SWBOutput/CCLM/SWB_RCA4_BIO_45.Rdata")
# blue_rca4_BIO_45 = bluewater(swbres) 
# bluerel_rca4_BIO_45 = bluewater(swbres, relative = TRUE)
# bluerelBAU_rca4_BIO_45 = (bluerel_rca4_BIO_45-bluerel_rca4_bau_45)
# 
# load("./data/SWBOutput/CCLM/SWB_RCA4_ADA_45.Rdata")
# blue_rca4_ADA_45 = bluewater(swbres) 
# bluerel_rca4_ADA_45 = bluewater(swbres, relative = TRUE)
# bluerelBAU_rca4_ADA_45 = (bluerel_rca4_ADA_45-bluerel_rca4_bau_45)
# 
# ## RCA4 8.5
# load("./data/SWBOutput/RCA4/SWB_RCA4_BAU_85.Rdata")
# blue_rca4_bau_85 = bluewater(swbres) 
# bluerel_rca4_bau_85 = bluewater(swbres, relative = TRUE)
# 
# load("./data/SWBOutput/RCA4/SWB_RCA4_CAR_85.Rdata")
# blue_rca4_CAR_85 = bluewater(swbres) 
# bluerel_rca4_CAR_85 = bluewater(swbres, relative = TRUE)
# bluerelBAU_rca4_CAR_85 = (bluerel_rca4_CAR_85-bluerel_rca4_bau_85)
# 
# load("./data/SWBOutput/RCA4/SWB_RCA4_BIO_85.Rdata")
# blue_rca4_BIO_85 = bluewater(swbres) 
# bluerel_rca4_BIO_85 = bluewater(swbres, relative = TRUE)
# bluerelBAU_rca4_BIO_85 = (bluerel_rca4_BIO_85-bluerel_rca4_bau_85)
# 
# load("./data/SWBOutput/RCA4/SWB_RCA4_ADA_85.Rdata")
# blue_rca4_ADA_85 = bluewater(swbres) 
# bluerel_rca4_ADA_85 = bluewater(swbres, relative = TRUE)
# bluerelBAU_rca4_ADA_85 = (bluerel_rca4_ADA_85-bluerel_rca4_bau_85)


## Merge all databases
bw = c(blue_cclm_bau_45, blue_cclm_CAR_45, blue_cclm_BIO_45, blue_cclm_ADA_45,
       blue_cclm_bau_85, blue_cclm_CAR_85, blue_cclm_BIO_85, blue_cclm_ADA_85)
       # blue_rca4_bau_45, blue_rca4_CAR_45, blue_rca4_BIO_45, blue_rca4_ADA_45,
       # blue_rca4_bau_85, blue_rca4_CAR_85, blue_rca4_BIO_85, blue_rca4_ADA_85)

bwrel = c(bluerel_cclm_bau_45, bluerel_cclm_CAR_45, bluerel_cclm_BIO_45, bluerel_cclm_ADA_45,
          bluerel_cclm_bau_85,bluerel_cclm_CAR_85,bluerel_cclm_BIO_85,bluerel_cclm_ADA_85)
          # bluerel_rca4_bau_45,bluerel_rca4_CAR_45,bluerel_rca4_BIO_45,bluerel_rca4_ADA_45,
          # bluerel_rca4_bau_85,bluerel_rca4_CAR_85,bluerel_rca4_BIO_85,bluerel_rca4_ADA_85)

bwbau = c(rep(NA, 26100), bluerelBAU_cclm_CAR_45, bluerelBAU_cclm_BIO_45, bluerelBAU_cclm_ADA_45,
          rep(NA, 26100),bluerelBAU_cclm_CAR_85,bluerelBAU_cclm_BIO_85,bluerelBAU_cclm_ADA_85)
          # rep(NA, 26100),bluerelBAU_rca4_CAR_45,bluerelBAU_rca4_BIO_45,bluerelBAU_rca4_ADA_45,
          # rep(NA, 26100),bluerelBAU_rca4_CAR_85,bluerelBAU_rca4_BIO_85,bluerelBAU_rca4_ADA_85)

yall = rep(year, 8)

for_all <- rep(for_type, 8)

recipe = as.character(gl(8, 26100,
                         labels=c("CCLM_45_BAU_45","CCLM_45_CAR_45","CCLM_45_BIO_45","CCLM_45_ADA_45",
                                  "CCLM_85_BAU_85", "CCLM_85_CAR_85", "CCLM_85_BIO_85", "CCLM_85_ADA_85")))
                                  # "RCA4_45_BAU_45", "RCA4_45_CAR_45", "RCA4_45_BIO_45", "RCA4_45_ADA_45",
                                  # "RCA4_85_BAU_85", "RCA4_85_CAR_85", "RCA4_85_BIO_85", "RCA4_85_ADA_85")))
RCP = substr(recipe, 6,7)
Climate_Model = substr(recipe, 1,4)
Narrative = substr(recipe, 9,14)
Management = substr(recipe, 9,11)


bw_plot = data.frame(Parcela = parcela, Recipe = recipe, RCP = RCP, Climate_Model = Climate_Model, 
                     Narrative = Narrative, Management = factor(Management), Forest_Type = for_all, 
                     Year = as.numeric(as.character(yall)), BW = bw, BWrel = bwrel, BWbau = bwbau)

bw_plot <- bw_plot %>% 
    mutate(Management = fct_relevel(Management, levels = c("BAU", "BIO","CAR","ADA")),
           Timestep = Year - 2001)

bw_species <- bw_plot %>%
    filter(Climate_Model == "CCLM") %>%
    group_by (Recipe, RCP, Climate_Model, Management, Narrative, Year, Forest_Type)  %>%
    summarize_at(vars(BW:BWbau), c(mean="mean", sd = "sd"), na.rm=T) %>%
    ungroup()

save(bw_plot, bw_species, file = "./data/ES_data/bw_plot.Rdata")

# Per plot  ----------------------------------------------------

bw_plot %>%
    ggplot(aes(x=Timestep, y=BWrel)) +
    geom_line(aes(color = RCP)) +
    facet_trelliscope(Parcela ~ Management , nrow = 2, ncol = 4, width = 300, 
                      path = here("reports/BWrel_per_plot"))

bw_plot %>%
    ggplot(aes(x=Timestep, y=BW)) +
    geom_line(aes(color = RCP)) +
    facet_trelliscope(Parcela ~ Management , nrow = 2, ncol = 4, width = 300, 
                      path = here("reports/BW_per_plot"))
