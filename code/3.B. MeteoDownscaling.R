library(meteoland)

load("data/pn.rdata")
load("data/ps.rdata")
load("data/mx.rdata")

sp_wgs84 = SpatialPoints(rbind(pn_sp_wgs84@coords, ps_sp_wgs84@coords, mx_sp_wgs84@coords), pn_sp_wgs84@proj4string)

spt = SpatialPointsTopography(sp_wgs84, 
                              elevation = c(pn_topo$elevation, ps_topo$elevation,mx_topo$elevation),
                              slope=c(pn_topo$slope, ps_topo$slope,mx_topo$slope),
                              aspect = c(pn_topo$aspect, ps_topo$aspect,mx_topo$aspect))



#Read table containing the target coordinates and filenames of observed meteorology
ObservedMP = read.table("data/Climate/InterpolatedMeteoIFN/MP.txt", sep="\t", header=TRUE)
ObsMP = SpatialPointsDataFrame(sp_wgs84, ObservedMP[,c("dir","filename")])

dates = seq(as.Date("2006-01-01"),as.Date("2100-12-31"), by="day")

###############
#    CCLM4-8-17
###############


#Read table containining RCM historical predictions
RCMHisMP = read.table("EUR-11/CCLM4-8-17/historical/MP_Solsones.txt", sep="\t", header=TRUE)
RCMHisMP$dir = "EUR-11/CCLM4-8-17/historical"
sp = SpatialPoints(RCMHisMP[,c("lon","lat")], CRS("+proj=longlat +datum=WGS84"))

#Read table containing rcp4.5 future predictions
RCM45MP = read.table("EUR-11/CCLM4-8-17/rcp4.5/MP_Solsones.txt", sep="\t", header=TRUE)
RCM45MP$dir = "EUR-11/CCLM4-8-17/rcp4.5"

#Read table containing rcp8.5 future predictions
RCM85MP = read.table("EUR-11/CCLM4-8-17/rcp8.5/MP_Solsones.txt", sep="\t", header=TRUE)
RCM85MP$dir = "EUR-11/CCLM4-8-17/rcp8.5"

mdd4.5 = MeteorologyUncorrectedData(sp,RCMHisMP[,c("dir","filename")],RCM45MP[,c("dir","filename")], dates)
mdd8.5 = MeteorologyUncorrectedData(sp,RCMHisMP[,c("dir","filename")],RCM85MP[,c("dir","filename")], dates)

#Perform bias correction of each target point in 'ObsMP'
correctionpoints(mdd4.5, ObsMP, topodata = pn_topo, export=TRUE,
                  exportDir="BiasCorrectedMeteoIFN/CCLM4-8-17/rcp4.5")
correctionpoints(mdd8.5, ObsMP, topodata = pn_topo, export=TRUE,
                  exportDir="BiasCorrectedMeteoIFN/CCLM4-8-17/rcp8.5")




###############
#    RCA4
###############

#Read table containining RCM historical predictions
RCMHisMP = read.table("EUR-11/RCA4/historical/MP_Solsones.txt", sep="\t", header=TRUE)
RCMHisMP$dir = "EUR-11/RCA4/historical"
sp = SpatialPoints(RCMHisMP[,c("lon","lat")], CRS("+proj=longlat +datum=WGS84"))

#Read table containing rcp4.5 future predictions
RCM45MP = read.table("EUR-11/RCA4/rcp4.5/MP_Solsones.txt", sep="\t", header=TRUE)
RCM45MP$dir = "EUR-11/RCA4/rcp4.5"

#Read table containing rcp8.5 future predictions
RCM85MP = read.table("EUR-11/RCA4/rcp8.5/MP_Solsones.txt", sep="\t", header=TRUE)
RCM85MP$dir = "EUR-11/RCA4/rcp8.5"

mdd4.5 = MeteorologyUncorrectedData(sp,RCMHisMP[,c("dir","filename")],RCM45MP[,c("dir","filename")], dates)
mdd8.5 = MeteorologyUncorrectedData(sp,RCMHisMP[,c("dir","filename")],RCM85MP[,c("dir","filename")], dates)

#Perform bias correction of each target point in 'ObsMP'
correctionpoints(mdd4.5, ObsMP, topodata = pn_topo, export=TRUE,
                  exportDir="BiasCorrectedMeteoIFN/RCA4/rcp4.5")
correctionpoints(mdd8.5, ObsMP, topodata = pn_topo, export=TRUE,
                  exportDir="BiasCorrectedMeteoIFN/RCA4/rcp8.5")


