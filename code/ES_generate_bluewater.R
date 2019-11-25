load("./data/pn.rdata")
load("./data/ps.rdata")
load("./data/mx.rdata")

all_forest_list <-  c(pn_forestlist, ps_forestlist, mx_forestlist)
all_forest_list <- all_forest_list[order(names(all_forest_list))]

load("./data/data_plot.Rdata")
load("./data/data_plot_species.Rdata")


bluewater<-function(swbres, relative = FALSE) {
    m= matrix(NA, nrow=length(swbres), ncol=100)
    for(i in 1:length(swbres)) {
        m[i,] = swbres[[i]]$Runoff+swbres[[i]]$DeepDrainage
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
for_type = data_plot %>% distinct(Parcela, Forest_Type) %>%
    dplyr::select(Forest_Type) %>%
    pull()
for_type <- rep(for_type,100)

## CCLM 4.5
load("./Rdata/SWBOutput/SWB_CCLM_BAU_45.Rdata")
blue_cclm_bau_45 = bluewater(swbres)
bluerel_cclm_bau_45 = bluewater(swbres, relative = TRUE)

load("./Rdata/SWBOutput/SWB_CCLM_scA_45.Rdata")
blue_cclm_scA_45 = bluewater(swbres) 
bluerel_cclm_scA_45 = bluewater(swbres, relative = TRUE)
bluerelBAU_cclm_scA_45 = (bluerel_cclm_scA_45-bluerel_cclm_bau_45)

load("./Rdata/SWBOutput/SWB_CCLM_scB_45.Rdata")
blue_cclm_scB_45 = bluewater(swbres) 
bluerel_cclm_scB_45 = bluewater(swbres, relative = TRUE)
bluerelBAU_cclm_scB_45 = (bluerel_cclm_scB_45-bluerel_cclm_bau_45)

load("./Rdata/SWBOutput/SWB_CCLM_scC_45.Rdata")
blue_cclm_scC_45 = bluewater(swbres) 
bluerel_cclm_scC_45 = bluewater(swbres, relative = TRUE)
bluerelBAU_cclm_scC_45 = (bluerel_cclm_scC_45-bluerel_cclm_bau_45)

## CCLM 8.5
load("./Rdata/SWBOutput/SWB_CCLM_BAU_85.Rdata")
blue_cclm_bau_85 = bluewater(swbres) 
bluerel_cclm_bau_85 = bluewater(swbres, relative = TRUE)

load("./Rdata/SWBOutput/SWB_CCLM_scD_85.Rdata")
blue_cclm_scD_85 = bluewater(swbres) 
bluerel_cclm_scD_85 = bluewater(swbres, relative = TRUE)
bluerelBAU_cclm_scD_85 = (bluerel_cclm_scD_85-bluerel_cclm_bau_85)

load("./Rdata/SWBOutput/SWB_CCLM_scE_85.Rdata")
blue_cclm_scE_85 = bluewater(swbres) 
bluerel_cclm_scE_85 = bluewater(swbres, relative = TRUE)
bluerelBAU_cclm_scE_85 = (bluerel_cclm_scE_85-bluerel_cclm_bau_85)

load("./Rdata/SWBOutput/SWB_CCLM_scF_85.Rdata")
blue_cclm_scF_85 = bluewater(swbres) 
bluerel_cclm_scF_85 = bluewater(swbres, relative = TRUE)
bluerelBAU_cclm_scF_85 = (bluerel_cclm_scF_85-bluerel_cclm_bau_85)

## RCA4 4.5
load("./Rdata/SWBOutput/SWB_RCA4_BAU_45.Rdata")
blue_rca4_bau_45 = bluewater(swbres) 
bluerel_rca4_bau_45 = bluewater(swbres, relative = TRUE)

load("./Rdata/SWBOutput/SWB_RCA4_scA_45.Rdata")
blue_rca4_scA_45 = bluewater(swbres) 
bluerel_rca4_scA_45 = bluewater(swbres, relative = TRUE)
bluerelBAU_rca4_scA_45 = (bluerel_rca4_scA_45-bluerel_rca4_bau_45)

load("./Rdata/SWBOutput/SWB_RCA4_scB_45.Rdata")
blue_rca4_scB_45 = bluewater(swbres) 
bluerel_rca4_scB_45 = bluewater(swbres, relative = TRUE)
bluerelBAU_rca4_scB_45 = (bluerel_rca4_scB_45-bluerel_rca4_bau_45)

load("./Rdata/SWBOutput/SWB_RCA4_scC_45.Rdata")
blue_rca4_scC_45 = bluewater(swbres) 
bluerel_rca4_scC_45 = bluewater(swbres, relative = TRUE)
bluerelBAU_rca4_scC_45 = (bluerel_rca4_scC_45-bluerel_rca4_bau_45)

## RCA4 8.5
load("./Rdata/SWBOutput/SWB_RCA4_BAU_85.Rdata")
blue_rca4_bau_85 = bluewater(swbres) 
bluerel_rca4_bau_85 = bluewater(swbres, relative = TRUE)

load("./Rdata/SWBOutput/SWB_RCA4_scD_85.Rdata")
blue_rca4_scD_85 = bluewater(swbres) 
bluerel_rca4_scD_85 = bluewater(swbres, relative = TRUE)
bluerelBAU_rca4_scD_85 = (bluerel_rca4_scD_85-bluerel_rca4_bau_85)

load("./Rdata/SWBOutput/SWB_RCA4_scE_85.Rdata")
blue_rca4_scE_85 = bluewater(swbres) 
bluerel_rca4_scE_85 = bluewater(swbres, relative = TRUE)
bluerelBAU_rca4_scE_85 = (bluerel_rca4_scE_85-bluerel_rca4_bau_85)

load("./Rdata/SWBOutput/SWB_RCA4_scF_85.Rdata")
blue_rca4_scF_85 = bluewater(swbres) 
bluerel_rca4_scF_85 = bluewater(swbres, relative = TRUE)
bluerelBAU_rca4_scF_85 = (bluerel_rca4_scF_85-bluerel_rca4_bau_85)


## Merge all databases
bw = c(blue_cclm_bau_45, blue_cclm_scA_45, blue_cclm_scB_45, blue_cclm_scC_45,
       blue_cclm_bau_85, blue_cclm_scD_85, blue_cclm_scE_85, blue_cclm_scF_85,
       blue_rca4_bau_45, blue_rca4_scA_45, blue_rca4_scB_45, blue_rca4_scC_45,
       blue_rca4_bau_85, blue_rca4_scD_85, blue_rca4_scE_85, blue_rca4_scF_85)

bwrel = c(bluerel_cclm_bau_45, bluerel_cclm_scA_45, bluerel_cclm_scB_45, bluerel_cclm_scC_45,
          bluerel_cclm_bau_85,bluerel_cclm_scD_85,bluerel_cclm_scE_85,bluerel_cclm_scF_85,
          bluerel_rca4_bau_45,bluerel_rca4_scA_45,bluerel_rca4_scB_45,bluerel_rca4_scC_45,
          bluerel_rca4_bau_85,bluerel_rca4_scD_85,bluerel_rca4_scE_85,bluerel_rca4_scF_85)

bwbau = c(rep(NA, 26100), bluerelBAU_cclm_scA_45, bluerelBAU_cclm_scB_45, bluerelBAU_cclm_scC_45,
          rep(NA, 26100),bluerelBAU_cclm_scD_85,bluerelBAU_cclm_scE_85,bluerelBAU_cclm_scF_85,
          rep(NA, 26100),bluerelBAU_rca4_scA_45,bluerelBAU_rca4_scB_45,bluerelBAU_rca4_scC_45,
          rep(NA, 26100),bluerelBAU_rca4_scD_85,bluerelBAU_rca4_scE_85,bluerelBAU_rca4_scF_85)

yall = rep(year, 16)

for_all <- rep(for_type, 16)

recipe = as.character(gl(16, 26100,
                         labels=c("CCLM_45_BAU_45","CCLM_45_scA_45","CCLM_45_scB_45","CCLM_45_scC_45",
                                  "CCLM_85_BAU_85", "CCLM_85_scD_85", "CCLM_85_scE_85", "CCLM_85_scF_85", 
                                  "RCA4_45_BAU_45", "RCA4_45_scA_45", "RCA4_45_scB_45", "RCA4_45_scC_45",
                                  "RCA4_85_BAU_85", "RCA4_85_scD_85", "RCA4_85_scE_85", "RCA4_85_scF_85")))
RCP = substr(recipe, 6,7)
Climate_Model = substr(recipe, 1,4)
Narrative = substr(recipe, 9,14)
Management = substr(recipe, 9,11)
Management[Management=="BAU"] = "BAU"
Management[Management=="scA"] = "CAR"
Management[Management=="scD"] = "CAR"
Management[Management=="scB"] = "BIO"
Management[Management=="scE"] = "BIO"
Management[Management=="scC"] = "ADA"
Management[Management=="scF"] = "ADA"

bw_plot = data.frame(Parcela = parcela, Recipe = recipe, RCP = RCP, Climate_Model = Climate_Model, 
                     Narrative = narrative, Management = management, Forest_Type = for_all, 
                     Year = as.numeric(as.character(yall)), BW = bw, BWrel = bwrel, BWbau = bwbau)

bw_plot <- bw_plot %>% 
    mutate(Management = fct_relevel(Management, levels = c("BAU", "BIO","CAR","ADA")),
           Timestep = Year - 2001)

bw_species <- bw_plot %>%
    filter(Climate_Model == "CCLM") %>%
    group_by (Recipe, RCP, Climate_Model, Management, Narrative, Year, Forest_Type)  %>%
    summarize_at(vars(BW:BWbau), c(mean="mean", sd = "sd"), na.rm=T) %>%
    ungroup()

