#################################################
#  RUNS SWB MEDFATE INPUT
#################################################
library(meteoland)
library(tidyverse)

source("code/medfate_0_inputs.R")


hist <- readRDS("data/Climate/new_clima/all_hist_daily.rds")
proj_cclm45 <- readRDS("data/Climate/new_clima/all_cnrm_cclm_rcp45_daily.rds")
proj_cclm85 <- readRDS("data/Climate/new_clima/all_cnrm_cclm_rcp85_daily.rds")
proj_rca445 <- readRDS("data/Climate/new_clima/all_cnrm_rca4_rcp45_daily.rds")
proj_rca485 <- readRDS("data/Climate/new_clima/all_cnrm_rca4_rcp85_daily.rds")

sp_wgs84 <- SpatialPoints(rbind(pn_sp_wgs84@coords, ps_sp_wgs84@coords, mx_sp_wgs84@coords), pn_sp_wgs84@proj4string)

topodata <- rbind(pn_topo, ps_topo, mx_topo)
spt <- SpatialPointsTopography(sp_wgs84,
                              elevation = topodata$elevation,
                              slope=topodata$slope)

codes <- c(pn_codes, ps_codes, mx_codes)

#Transform to Longitude/Latitude
sp_latlon = spTransform(spt, sp::CRS("+proj=longlat +datum=WGS84"))

#Retrieve soil parameter list
# IFN_soilgrids_params_mod = readRDS("Z:/Datasets/Soil/Products/IFN_SoilGrids/IFN3_soilgrids_params_rfcmod.rds")
IFN_soilgrids_params_mod = readRDS("data/IFN3_soilgrids_params.rds")
soil_list_params = IFN_soilgrids_params_mod[codes]

#Load SORTIE OUTPUT
load("data/raw_data_CD.Rdata")

data_CD_alive <- raw_data_CD %>%
  filter(Type %in% c("Adult","Sapling"),
         `Dead Code` == "Alive")


# data = data_CD_alive
# proj <- proj_cclm45
# codes
# climate_model <- "CCLM"
# rcp <- "45"
# narrative <-"BAU_45"

simScenario<-function(data, hist, proj, codes, climate_model, rcp, narrative) {
  
    datascen <- data %>%
      filter(RCP == rcp,
             Climate_Model == climate_model,
             Narrative == narrative)
  
  
  #Initialize output list
    swbres = vector("list",length(codes))
    names(swbres) = codes
  
  #Simulation over plots
  for(i in 1:length(codes)) {
    

    code = codes[i]
    cat(paste0("\nProcessing plot ", code, " (",i,"/", length(codes),")...\n"))
    metproj = proj[[i]]
    methist = hist[[i]]
    met = rbind(methist[as.character(seq(as.Date("2001-01-01"), as.Date("2005-12-31"), by="day")),],
                metproj[as.character(seq(as.Date("2006-01-01"), as.Date("2100-12-30"), by="day")),])
    
    
    # create plots for 100 years
    if(code %in% pn_codes) {
      fs = buildPlotForestList_PN(datascen, code)
    } else if(code %in% ps_codes) {
      fs = buildPlotForestList_PS(datascen, code)
    } else if(code %in% mx_codes) {
      fs = buildPlotForestList_MX(datascen, code)
    }
    
    s = soil(soil_list_params[[i]])   ## soils
    years = 2001:2100
    control = defaultControl()
    control$verbose = FALSE
    swbout = NULL
    pb = txtProgressBar(1,100, style = 3)
    for(step in 1:100) {
        setTxtProgressBar(pb, step)
        dyear = seq(as.Date(paste0(years[step],"-01-01")), as.Date(paste0(years[step],"-12-31")), by="day")
        if(step==100)  dyear = seq(as.Date(paste0(years[step],"-01-01")), as.Date(paste0(years[step],"-12-30")), by="day")
        metyear = met[as.character(dyear),]
        fsyear = fs[[step]]
        if(nrow(fsyear$treeData) > 0) {
          fsyear$treeData$Z50 = 200
          fsyear$treeData$Z95 = 1000
        }
        if(nrow(fsyear$shrubData) > 0) {
          fsyear$shrubData$Z50 = 400
          fsyear$shrubData$Z95 = 800
        }
        xyear = forest2spwbInput(fsyear, s, SpParamsMED, control)
        
        ## Run the simu
        swbyear = spwb(xyear, s, metyear, elevation = spt[code,]$elevation, latitude = sp_latlon@coords[code,2])
        swbsum = as.data.frame(summary(swbyear))
        swbstand = as.data.frame(summary(swbyear, output = "Stand"))
        swbstress = as.data.frame(summary(swbyear, output = "PlantStress"))
        
        swbsum <- cbind(swbsum,swbstand, swbstress) %>%
          mutate(LAI = LAI/365,
                 Cm = Cm/365,
                 LgroundSWR = LgroundSWR/365,
                 FCC = 100 * (1-LgroundPAR/365))
        
        if(is.null(swbout)) swbout = swbsum else swbout = dplyr::bind_rows(swbout, swbsum)
    }
    swbres[[i]] = swbout
  }
  
  save(swbres, file = paste0("./data/SWBOutput/",climate_model,"/","SWB_", climate_model, "_", narrative, ".Rdata"))
  }



## CCLM

simScenario(data_CD_alive, hist, proj_cclm45, codes, "CCLM","45", "BAU_45")
simScenario(data_CD_alive, hist, proj_cclm45, codes, "CCLM","45", "CAR_45")
simScenario(data_CD_alive, hist, proj_cclm45, codes, "CCLM","45", "BIO_45")
simScenario(data_CD_alive, hist, proj_cclm45, codes, "CCLM","45", "ADA_45")

simScenario(data_CD_alive, hist, proj_cclm85, codes, "CCLM","85", "BAU_85")
simScenario(data_CD_alive, hist, proj_cclm85, codes, "CCLM","85", "CAR_85")
simScenario(data_CD_alive, hist, proj_cclm85, codes, "CCLM","85", "BIO_85")
simScenario(data_CD_alive, hist, proj_cclm85, codes, "CCLM","85", "ADA_85")


## RCA4

simScenario(data_CD_alive, hist, proj_rca445, codes, "RCA4","45", "BAU_45")
simScenario(data_CD_alive, hist, proj_rca445, codes, "RCA4","45", "CAR_45")
simScenario(data_CD_alive, hist, proj_rca445, codes, "RCA4","45", "BIO_45")
simScenario(data_CD_alive, hist, proj_rca445, codes, "RCA4","45", "ADA_45")

simScenario(data_CD_alive, hist, proj_rca485, codes, "RCA4","85", "BAU_85")
simScenario(data_CD_alive, hist, proj_rca485, codes, "RCA4","85", "CAR_85")
simScenario(data_CD_alive, hist, proj_rca485, codes, "RCA4","85", "BIO_85")
simScenario(data_CD_alive, hist, proj_rca485, codes, "RCA4","85", "ADA_85")


  
  
  
  
  
#   
#   
#   
#   
#   
#   
# #Simulate all scenarios (climate model / climate scenario / management)
# # for(climmod in c("RCA4")) {
# for(climmod in c("CCLM4-8-17","RCA4")) {
#   modeldata = ifelse(climmod=="CCLM4-8-17", "CCLM", "RCA4")
#   # for(climsce in c("rcp4.5")) {
#   for(climsce in c("rcp4.5","rcp8.5")) {
#     climdata = ifelse(climsce=="rcp4.5", "45", "85")
#     scendir = paste0(climmod, "/", climsce)
#     if(climsce=="rcp4.5") {
#       for(narrativedata in c("BAU_45", "CAR_45", "BIO_45", "ADA_45")) {
#         scen = paste0(modeldata,"_", narrativedata)
#         cat(paste0("\n\nRunning scenario: ", scen,":\n"))
#         #Select sortie data corresponding to scenario
#         datascen = data_CD[(data_CD$RCP == climdata) & (data_CD$Climate_Model == modeldata) & (data_CD$Narrative == narrativedata),]
#         simScenario(datascen, codes, scendir, paste0("data/SWBOutput/SWB_",scen,".Rdata"))
#       }
#     } else {
#       for(narrativedata in c("BAU_85", "CAR_85", "BIO_85", "ADA_85")) {
#         scen = paste0(modeldata,"_", narrativedata)
#         cat(paste0("\n\nRunning scenario: ", scen,":\n"))
#         #Select sortie data corresponding to scenario
#         datascen = data_CD[(data_CD$RCP==climdata) & (data_CD$Climate_Model == modeldata) & (data_CD$Narrative == narrativedata),]
#         simScenario(datascen, codes, scendir, paste0("data/SWBOutput/SWB_",scen,".Rdata"))
#       }
#     }
#   }
# }

