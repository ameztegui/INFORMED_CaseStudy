library(meteoland)
library(tidyverse)
library(raster)

load("data/pn.rdata")
load("data/ps.rdata")
load("data/mx.rdata")

points <- rbind(pn_sp_longlat, ps_sp_longlat, mx_sp_longlat)


# Rainfall erosivity (R) --------------

hist_daily <- readRDS("./data/Climate/new_clima/all_hist_daily.rds")
cclm_45_daily <- readRDS("./data/Climate/new_clima/all_cnrm_cclm_rcp45_daily.rds")
cclm_85_daily <- readRDS("./data/Climate/new_clima/all_cnrm_cclm_rcp85_daily.rds")
rca4_45_daily <- readRDS("./data/Climate/new_clima/all_cnrm_rca4_rcp45_daily.rds")
rca4_85_daily <- readRDS("./data/Climate/new_clima/all_cnrm_rca4_rcp85_daily.rds")

precipitation_rainfallErosivity(hist_daily@data[[1]], points@coords[1,1], scale = "year", average = FALSE)

eros_hist    <- map2_df(hist_daily@data, points@coords[,1], 
                        precipitation_rainfallErosivity, 
                        scale = "year", average = FALSE) %>% dplyr::select(`2001-01-01`:`2005-01-01`)
eros_cclm_45 <- map2_df(cclm_45_daily@data, points@coords[,1], 
                        precipitation_rainfallErosivity, 
                        scale = "year", average = FALSE) %>% dplyr::select(`2006-01-01`:`2100-01-01`)
eros_cclm_85 <- map2_df(cclm_85_daily@data, points@coords[,1], 
                        precipitation_rainfallErosivity, 
                        scale = "year", average = FALSE) %>% dplyr::select(`2006-01-01`:`2100-01-01`)
eros_rca4_45 <- map2_df(rca4_45_daily@data, points@coords[,1], 
                        precipitation_rainfallErosivity, 
                        scale = "year", average = FALSE) %>% dplyr::select(`2006-01-01`:`2100-01-01`)
eros_rca4_85 <- map2_df(rca4_85_daily@data, points@coords[,1], 
                        precipitation_rainfallErosivity, 
                        scale = "year", average = FALSE) %>% dplyr::select(`2006-01-01`:`2100-01-01`)


r_cclm_45 <- bind_cols(eros_hist, eros_cclm_45) 
row.names(r_cclm_45) <- row.names(points@coords)
r_cclm_85 <- bind_cols(eros_hist, eros_cclm_85)
row.names(r_cclm_85) <- row.names(points@coords)
r_rca4_45 <- bind_cols(eros_hist, eros_rca4_45)
row.names(r_rca4_45) <- row.names(points@coords)
r_rca4_85 <- bind_cols(eros_hist, eros_rca4_85)
row.names(r_rca4_85) <- row.names(points@coords)

save(r_cclm_45, r_cclm_85, r_rca4_45, r_rca4_85, file = "./data/erosion/erosion_R.rdata")


# Canopy cover ----------------------------------------------------------------------------------------------------

## CCLM
load("./data/SWBOutput/CCLM/SWB_CCLM_BAU_45.Rdata")
par <- map_df(swbres, "LgroundPAR") 
fcc_cclm_bau_45 <- as.data.frame(1 - t(as.matrix(par))/365)
fcc_cclm_bau_45[fcc_cclm_bau_45 < 0] <- 0

load("./data/SWBOutput/CCLM/SWB_CCLM_BIO_45.Rdata")
par <- map_df(swbres, "LgroundPAR") 
fcc_cclm_bio_45 <- as.data.frame(1 - t(as.matrix(par))/365)
fcc_cclm_bio_45[fcc_cclm_bio_45 < 0] <- 0

load("./data/SWBOutput/CCLM/SWB_CCLM_CAR_45.Rdata")
par <- map_df(swbres, "LgroundPAR") 
fcc_cclm_car_45 <- as.data.frame(1 - t(as.matrix(par))/365)
fcc_cclm_car_45[fcc_cclm_car_45 < 0] <- 0

load("./data/SWBOutput/CCLM/SWB_CCLM_ADA_45.Rdata")
par <- map_df(swbres, "LgroundPAR") 
fcc_cclm_ada_45 <- as.data.frame(1 - t(as.matrix(par))/365)
fcc_cclm_ada_45[fcc_cclm_ada_45 < 0] <- 0

load("./data/SWBOutput/CCLM/SWB_CCLM_BAU_85.Rdata")
par <- map_df(swbres, "LgroundPAR") 
fcc_cclm_bau_85 <- as.data.frame(1 - t(as.matrix(par))/365)
fcc_cclm_bau_85[fcc_cclm_bau_85 < 0] <- 0

load("./data/SWBOutput/CCLM/SWB_CCLM_BIO_85.Rdata")
par <- map_df(swbres, "LgroundPAR") 
fcc_cclm_bio_85 <- as.data.frame(1 - t(as.matrix(par))/365)
fcc_cclm_bio_85[fcc_cclm_bio_85 <0] <- 0

load("./data/SWBOutput/CCLM/SWB_CCLM_CAR_85.Rdata")
par <- map_df(swbres, "LgroundPAR") 
fcc_cclm_car_85 <- as.data.frame(1 - t(as.matrix(par))/365)
fcc_cclm_car_85[fcc_cclm_car_85 < 0] <- 0

load("./data/SWBOutput/CCLM/SWB_CCLM_ADA_85.Rdata")
par <- map_df(swbres, "LgroundPAR") 
fcc_cclm_ada_85 <- as.data.frame(1 - t(as.matrix(par))/365)
fcc_cclm_ada_85[fcc_cclm_ada_85 < 0] <- 0

save(fcc_cclm_bau_45, fcc_cclm_bio_45, fcc_cclm_car_45, fcc_cclm_ada_45,
     fcc_cclm_bau_85, fcc_cclm_bio_85, fcc_cclm_car_85, fcc_cclm_ada_85,
     file = "./data/erosion/canopy_cover.Rdata")
