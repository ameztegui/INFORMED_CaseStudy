
rm(list=ls())

library(stringr)
library(purrr)
library(meteoland)
library(tidyverse)

load("data/pn.rdata")
load("data/ps.rdata")
load("data/mx.rdata")

points <- rbind(mx_sp_longlat, pn_sp_longlat, ps_sp_longlat)

join_climate_files <- function (clim_dir, pattern) {
  mx_files <- readRDS(dir(clim_dir, pattern = paste("mx", pattern, sep = "_"), full.names = T))
  pn_files <- readRDS(dir(clim_dir, pattern = paste("pn", pattern, sep = "_"), full.names = T))
  ps_files <- readRDS(dir(clim_dir, pattern = paste("ps", pattern, sep = "_"), full.names = T))

  data <- c(mx_files@data, pn_files@data, ps_files@data)
  foo <- SpatialPointsMeteorology(points, data , dates = mx_files@dates, dataByDate = F)
  saveRDS(foo, file = paste0(clim_dir, "all_", pattern, "_daily.rds"))
}

 join_climate_files("./data/Climate/new_clima/", "hist") 
 join_climate_files("./data/Climate/new_clima/", "cnrm_cclm_rcp45") 
 join_climate_files("./data/Climate/new_clima/", "cnrm_cclm_rcp85") 
 join_climate_files("./data/Climate/new_clima/", "cnrm_rca4_rcp45") 
 join_climate_files("./data/Climate/new_clima/", "cnrm_rca4_rcp85") 
 

create_climate_files <- function (clim_dir, plots, codes, model, rcp) {
  
  hist_files <- dir(clim_dir, pattern = paste(plots,"hist", sep = "_"), full.names = T)
  clim_hist_object <- readRDS(hist_files)
  dates_hist = seq(as.Date("2001-01-01"), as.Date("2005-12-31"), by="day")
  prec_month_hist = summarypoints(clim_hist_object, "Precipitation", fun=sum, freq="month", na.rm=TRUE, dates=dates_hist)
  meantemp_month_hist = summarypoints(clim_hist_object, "MeanTemperature", fun=mean, freq="month", na.rm=TRUE, dates=dates_hist)
  
  ### scenario
  futu_files <- dir(clim_dir, pattern = paste(plots, "cnrm", model, rcp, sep = "_"), full.names = T)
  clim_futu_object <- readRDS(futu_files)
  prec_month_futu = summarypoints(clim_futu_object, "Precipitation", fun=sum, freq="month", na.rm=TRUE)
  meantemp_month_futu = summarypoints(clim_futu_object, "MeanTemperature", fun=mean, freq="month", na.rm=TRUE)

  pb = txtProgressBar(0, nrow(meantemp_month_futu), style=3)
  for(i in 1:nrow(prec_month_futu)) {
      setTxtProgressBar(pb, i)
      
      pr = rbind(matrix(as.vector(as.matrix(prec_month_hist@data[i,])), nrow=5, ncol =12, byrow = TRUE),
                 matrix(as.vector(as.matrix(prec_month_futu@data[i,])), nrow=95, ncol =12, byrow = TRUE))
      rownames(pr)<-2001:2100
      colnames(pr)<-1:12
      write.table(pr, file=paste("SORTIE_files/Climatic_Data/Prec/", model, "/", rcp, "/", codes[i],".txt",sep=""), sep="\t", quote=FALSE, row.names = F)
  
    mt = rbind(matrix(as.vector(as.matrix(meantemp_month_hist@data[i,])), nrow=5, ncol =12, byrow = TRUE),
               matrix(as.vector(as.matrix(meantemp_month_futu@data[i,])), nrow=95, ncol =12, byrow = TRUE))
    rownames(mt)<-2001:2100
    colnames(mt)<-1:12
    write.table(mt, file=paste("SORTIE_files/Climatic_Data/Temp/", model, "/", rcp, "/", codes[i],".txt",sep=""), sep="\t", quote=FALSE, row.names = F)
  }
}

create_climate_files("./data/Climate/new_clima/", "mx", mx_codes, "cclm", "rcp45") 
create_climate_files("./data/Climate/new_clima/", "pn", pn_codes,  "cclm", "rcp45") 
create_climate_files("./data/Climate/new_clima/", "ps", ps_codes, "cclm", "rcp45") 

create_climate_files("./data/Climate/new_clima/", "mx", mx_codes, "cclm", "rcp85") 
create_climate_files("./data/Climate/new_clima/", "pn", pn_codes,  "cclm", "rcp85") 
create_climate_files("./data/Climate/new_clima/", "ps", ps_codes, "cclm", "rcp85") 

create_climate_files("./data/Climate/new_clima/", "mx", mx_codes, "rca4", "rcp45") 
create_climate_files("./data/Climate/new_clima/", "pn", pn_codes,  "rca4", "rcp45") 
create_climate_files("./data/Climate/new_clima/", "ps", ps_codes, "rca4", "rcp45") 

create_climate_files("./data/Climate/new_clima/", "mx", mx_codes, "rca4", "rcp85") 
create_climate_files("./data/Climate/new_clima/", "pn", pn_codes,  "rca4", "rcp85") 
create_climate_files("./data/Climate/new_clima/", "ps", ps_codes, "rca4", "rcp85") 




# Import data into Rdata ------------------------------------------------------------------------------------------


# CCLM --------------------------------------------------------------------

temp_CCLM_4.5_files <- list.files(path="./SORTIE_files/Climatic_Data/Temp/cclm/rcp45", full.names = T)
prec_CCLM_4.5_files <- list.files(path="./SORTIE_files/Climatic_Data/Prec/cclm/rcp45/", full.names = T)

temp_CCLM_4.5 =  map(temp_CCLM_4.5_files, read.table,header=TRUE,se="\t")
prec_CCLM_4.5 = map(prec_CCLM_4.5_files, read.table,header=TRUE,se="\t")
names(temp_CCLM_4.5) = str_sub(temp_CCLM_4.5_files,start=-10,-5)
names(prec_CCLM_4.5) = str_sub(prec_CCLM_4.5_files,start=-10,-5)

CCLM_4.5 <- mapply(list, temp = temp_CCLM_4.5, prec=prec_CCLM_4.5, SIMPLIFY = FALSE)
      

temp_CCLM_8.5_files <- list.files(path="./SORTIE_files/Climatic_Data/Temp/cclm/rcp85", full.names = T)
prec_CCLM_8.5_files <- list.files(path="./SORTIE_files/Climatic_Data/Prec/cclm/rcp85", full.names = T)

temp_CCLM_8.5 =  map(temp_CCLM_8.5_files, read.table,header=TRUE,se="\t")
prec_CCLM_8.5 = map(prec_CCLM_8.5_files, read.table,header=TRUE,se="\t")
names(temp_CCLM_8.5) = str_sub(temp_CCLM_8.5_files,start=-10,-5)
names(prec_CCLM_8.5) = str_sub(prec_CCLM_8.5_files,start=-10,-5)

CCLM_8.5 <- mapply(list, temp=temp_CCLM_8.5, prec=prec_CCLM_8.5, SIMPLIFY = FALSE)



      
# RCA4 --------------------------------------------------------------------

temp_RCA4_4.5_files <- list.files(path="./SORTIE_files/Climatic_Data/Temp/rca4/rcp45", full.names = T)
prec_RCA4_4.5_files <- list.files(path="./SORTIE_files/Climatic_Data/Prec/rca4/rcp45", full.names = T)

temp_RCA4_4.5 = map(temp_RCA4_4.5_files, read.table,header=TRUE,se="\t")
prec_RCA4_4.5 = map(prec_RCA4_4.5_files, read.table,header=TRUE,se="\t")
names(temp_RCA4_4.5) = str_sub(temp_RCA4_4.5_files,start=-10,-5)
names(prec_RCA4_4.5) = str_sub(prec_RCA4_4.5_files,start=-10,-5)

RCA4_4.5 <- mapply(list, temp=temp_RCA4_4.5, prec=prec_RCA4_4.5, SIMPLIFY = FALSE)


temp_RCA4_8.5_files <- list.files(path="./SORTIE_files/Climatic_Data/Temp/rca4/rcp85", full.names = T)
prec_RCA4_8.5_files <- list.files(path="./SORTIE_files/Climatic_Data/Prec/rca4/rcp85", full.names = T)

temp_RCA4_8.5 = map(temp_RCA4_8.5_files, read.table,header=TRUE,se="\t")
prec_RCA4_8.5 = map(prec_RCA4_8.5_files, read.table,header=TRUE,se="\t")
names(temp_RCA4_8.5) = str_sub(temp_RCA4_8.5_files,start=-10,-5)
names(prec_RCA4_8.5) = str_sub(prec_RCA4_8.5_files,start=-10,-5)

RCA4_8.5 <- mapply(list, temp=temp_RCA4_8.5, prec=prec_RCA4_8.5, SIMPLIFY = FALSE)


save (CCLM_4.5,CCLM_8.5,
      RCA4_4.5, RCA4_8.5,
      file="./data/climatic_data_new.Rdata")

