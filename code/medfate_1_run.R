#################################################
#  RUNS SWB MEDFATE INPUT
#################################################
library(meteoland)

source("code/medfate_0_inputs.R")


sp_wgs84 = SpatialPoints(rbind(pn_sp_wgs84@coords, ps_sp_wgs84@coords, mx_sp_wgs84@coords), pn_sp_wgs84@proj4string)

topodata = rbind(pn_topo, ps_topo, mx_topo)

spt = SpatialPointsTopography(sp_wgs84,
                              elevation = topodata$elevation,
                              slope=topodata$slope,
                              aspect =topodata$aspect)

codes = c(pn_codes, ps_codes, mx_codes)

#Transform to Longitude/Latitude
sp_latlon = spTransform(spt, sp::CRS("+proj=longlat +datum=WGS84"))

#Retrieve soil parameter list
# IFN_soilgrids_params_mod = readRDS("Z:/Datasets/Soil/Products/IFN_SoilGrids/IFN3_soilgrids_params_rfcmod.rds")
IFN_soilgrids_params_mod = readRDS("data/IFN3_soilgrids_params.rds")
soil_list_params = IFN_soilgrids_params_mod[codes]

#Load SORTIE OUTPUT
load(paste0("data/data_CD.Rdata"))


simScenario<-function(datascen, codes, scendir, outfile) {
  #Initialize output list
  swbres = vector("list",length(codes))
  names(swbres) = codes
  #Simulation over plots
  for(i in 1:length(codes)) {
    code = codes[i]
    cat(paste0("\nProcessing plot ", code, " (",i,"/", length(codes),")...\n"))
    metproj = readmeteorologypoint(paste0("Climate/BiasCorrectedMeteoIFN/",scendir,"/",code,".rds"), format="meteoland/rds")
    methist = readmeteorologypoint(paste0("Climate/InterpolatedMeteoIFN/",code,".rds"), format="meteoland/rds")
    met = rbind(methist[as.character(seq(as.Date("2001-01-01"), as.Date("2005-12-31"), by="day")),],
                metproj[as.character(seq(as.Date("2006-01-01"), as.Date("2100-12-30"), by="day")),])
    if(code %in% pn_codes) {
      fs = buildPlotForestList_PN(datascen, code)
    } else if(code %in% ps_codes) {
      fs = buildPlotForestList_PS(datascen, code)
    } else if(code %in% mx_codes) {
      fs = buildPlotForestList_MX(datascen, code)
    }
    s = soil(soil_list_params[[i]])
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
      if(nrow(fsyear$treeData)>0) {
        fsyear$treeData$Z50 = 200
        fsyear$treeData$Z95 = 1000
      }
      if(nrow(fsyear$shrubData)>0) {
        fsyear$shrubData$Z50 = 400
        fsyear$shrubData$Z95 = 800
        fsyear$shrubData$Z = NULL
      }
      xyear = forest2spwbInput(fsyear, s, SpParamsMED, control)
      swbyear =spwb(xyear,s,metyear, elevation = spt[code,]$elevation)
      swbsum = summary(swbyear)
      swbsum[,"LAIcell"] = swbsum[,"LAIcell"]/365
      swbsum[,"Cm"] = swbsum[,"Cm"]/365
      swbsum[,"Lground"] = swbsum[,"Lground"]/365
      if(is.null(swbout)) swbout = swbsum
      else swbout = rbind(swbout, summary(swbyear))
    }
    swbres[[i]] = swbout
  }
  save(swbres, file=outfile)
}



#Simulate all scenarios (climate model / climate scenario / management)
# for(climmod in c("RCA4")) {
for(climmod in c("CCLM4-8-17","RCA4")) {
  modeldata = ifelse(climmod=="CCLM4-8-17", "CCLM", "RCA4")
  # for(climsce in c("rcp4.5")) {
  for(climsce in c("rcp4.5","rcp8.5")) {
    climdata = ifelse(climsce=="rcp4.5", "45", "85")
    scendir = paste0(climmod, "/", climsce)
    if(climsce=="rcp4.5") {
      for(narrativedata in c("BAU_45", "CAR_45", "BIO_45", "ADA_45")) {
        scen = paste0(modeldata,"_", narrativedata)
        cat(paste0("\n\nRunning scenario: ", scen,":\n"))
        #Select sortie data corresponding to scenario
        datascen = data_CD[(data_CD$RCP == climdata) & (data_CD$Climate_Model == modeldata) & (data_CD$Narrative == narrativedata),]
        simScenario(datascen, codes, scendir, paste0("data/SWBOutput/SWB_",scen,".Rdata"))
      }
    } else {
      for(narrativedata in c("BAU_85", "CAR_85", "BIO_85", "ADA_85")) {
        scen = paste0(modeldata,"_", narrativedata)
        cat(paste0("\n\nRunning scenario: ", scen,":\n"))
        #Select sortie data corresponding to scenario
        datascen = data_CD[(data_CD$RCP==climdata) & (data_CD$Climate_Model == modeldata) & (data_CD$Narrative == narrativedata),]
        simScenario(datascen, codes, scendir, paste0("Rdata/SWBOutput/SWB_",scen,".Rdata"))
      }
    }
  }
}


