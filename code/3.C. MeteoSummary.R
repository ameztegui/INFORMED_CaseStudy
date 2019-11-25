library(meteoland)

load("Rdata/pn.rdata")
load("Rdata/ps.rdata")
load("Rdata/mx.rdata")

sp_wgs84 = SpatialPoints(rbind(pn_sp_wgs84@coords, ps_sp_wgs84@coords, mx_sp_wgs84@coords), pn_sp_wgs84@proj4string)

spt = SpatialPointsTopography(sp_wgs84, 
                              elevation = c(pn_topo$elevation, ps_topo$elevation,mx_topo$elevation),
                              slope=c(pn_topo$slope, ps_topo$slope,mx_topo$slope),
                              aspect = c(pn_topo$aspect, ps_topo$aspect,mx_topo$aspect))
codes = c(pn_codes, ps_codes, mx_codes)
setwd("../CaseStudy_INFORMED_RARS")


###############
#    HISTORICAL
###############

mp = read.table("InterpolatedMeteoIFN/MP.txt", header=TRUE, sep="\t")
spdf = SpatialPointsDataFrame(sp_wgs84@coords, mp, proj4string=sp_wgs84@proj4string)
dates_hist = seq(as.Date("2001-01-01"), as.Date("2005-12-31"), by="day")
prec_month_hist = summarypoints(spdf, "Precipitation", fun=sum, freq="month", na.rm=TRUE, dates=dates_hist)
meantemp_month_hist = summarypoints(spdf, "MeanTemperature", fun=mean, freq="month", na.rm=TRUE, dates=dates_hist)


###############
#    CCLM4-8-17
###############

mp = read.table("BiasCorrectedMeteoIFN/CCLM4-8-17/rcp4.5/MP.txt", header=TRUE, sep="\t")
spdf = SpatialPointsDataFrame(sp_wgs84@coords, mp, proj4string=sp_wgs84@proj4string)
prec_month = summarypoints(spdf, "Precipitation", fun=sum, freq="month", na.rm=TRUE)
meantemp_month = summarypoints(spdf, "MeanTemperature", fun=mean, freq="month", na.rm=TRUE)
pb = txtProgressBar(0, nrow(prec_month), style=3)
for(i in 1:nrow(prec_month)) {
  setTxtProgressBar(pb, i)
  pr = rbind(matrix(as.vector(as.matrix(prec_month_hist@data[i,])), nrow=5, ncol =12, byrow = TRUE),
            matrix(as.vector(as.matrix(prec_month@data[i,-1])), nrow=95, ncol =12, byrow = TRUE))
  rownames(pr)<-2001:2100
  colnames(pr)<-1:12
  write.table(pr, file=paste("PrecipitationSortieIFN/CCLM4-8-17/rcp4.5/", codes[i],".txt",sep=""),sep="\t", quote=FALSE)
  mt = rbind(matrix(as.vector(as.matrix(meantemp_month_hist@data[i,])), nrow=5, ncol =12, byrow = TRUE),
            matrix(as.vector(as.matrix(meantemp_month@data[i,-1])), nrow=95, ncol =12, byrow = TRUE))
  rownames(mt)<-2001:2100
  colnames(mt)<-1:12
  write.table(mt, file=paste("MeanTemperatureSortieIFN/CCLM4-8-17/rcp4.5/",codes[i],".txt",sep=""),sep="\t", quote=FALSE)
}


mp = read.table("BiasCorrectedMeteoIFN/CCLM4-8-17/rcp8.5/MP.txt", header=TRUE, sep="\t")
spdf = SpatialPointsDataFrame(sp_wgs84@coords, mp, proj4string=sp_wgs84@proj4string)
prec_month = summarypoints(spdf, "Precipitation", fun=sum, freq="month", na.rm=TRUE)
meantemp_month = summarypoints(spdf, "MeanTemperature", fun=sum, freq="month", na.rm=TRUE)
pb = txtProgressBar(0, nrow(prec_month), style=3)
for(i in 1:nrow(prec_month)) {
  setTxtProgressBar(pb, i)
  pr = rbind(matrix(as.vector(as.matrix(prec_month_hist@data[i,])), nrow=5, ncol =12, byrow = TRUE),
             matrix(as.vector(as.matrix(prec_month@data[i,-1])), nrow=95, ncol =12, byrow = TRUE))
  rownames(pr)<-2001:2100
  colnames(pr)<-1:12
  write.table(pr, file=paste("PrecipitationSortieIFN/CCLM4-8-17/rcp8.5/",codes[i],".txt",sep=""),sep="\t", quote=FALSE)
  mt = rbind(matrix(as.vector(as.matrix(meantemp_month_hist@data[i,])), nrow=5, ncol =12, byrow = TRUE),
             matrix(as.vector(as.matrix(meantemp_month@data[i,-1])), nrow=95, ncol =12, byrow = TRUE))
  rownames(mt)<-2001:2100
  colnames(mt)<-1:12
  write.table(mt, file=paste("MeanTemperatureSortieIFN/CCLM4-8-17/rcp8.5/",codes[i],".txt",sep=""),sep="\t", quote=FALSE)
}

###############
#    RCA4
###############

mp = read.table("BiasCorrectedMeteoIFN/RCA4/rcp4.5/MP.txt", header=TRUE, sep="\t")
spdf = SpatialPointsDataFrame(sp_wgs84@coords, mp, proj4string=sp_wgs84@proj4string)
prec_month = summarypoints(spdf, "Precipitation", fun=sum, freq="month", na.rm=TRUE)
meantemp_month = summarypoints(spdf, "MeanTemperature", fun=sum, freq="month", na.rm=TRUE)
pb = txtProgressBar(0, nrow(prec_month), style=3)
for(i in 1:nrow(prec_month)) {
  setTxtProgressBar(pb, i)
  pr = rbind(matrix(as.vector(as.matrix(prec_month_hist@data[i,])), nrow=5, ncol =12, byrow = TRUE),
             matrix(as.vector(as.matrix(prec_month@data[i,-1])), nrow=95, ncol =12, byrow = TRUE))
  rownames(pr)<-2001:2100
  colnames(pr)<-1:12
  write.table(pr, file=paste("PrecipitationSortieIFN/RCA4/rcp4.5/",codes[i],".txt",sep=""),sep="\t", quote=FALSE)
  mt = rbind(matrix(as.vector(as.matrix(meantemp_month_hist@data[i,])), nrow=5, ncol =12, byrow = TRUE),
             matrix(as.vector(as.matrix(meantemp_month@data[i,-1])), nrow=95, ncol =12, byrow = TRUE))
  rownames(mt)<-2001:2100
  colnames(mt)<-1:12
  write.table(mt, file=paste("MeanTemperatureSortieIFN/RCA4/rcp4.5/",codes[i],".txt",sep=""),sep="\t", quote=FALSE)
}


mp = read.table("BiasCorrectedMeteoIFN/RCA4/rcp8.5/MP.txt", header=TRUE, sep="\t")
spdf = SpatialPointsDataFrame(sp_wgs84@coords, mp, proj4string=sp_wgs84@proj4string)
prec_month = summarypoints(spdf, "Precipitation", fun=sum, freq="month", na.rm=TRUE)
meantemp_month = summarypoints(spdf, "MeanTemperature", fun=sum, freq="month", na.rm=TRUE)
pb = txtProgressBar(0, nrow(prec_month), style=3)
for(i in 1:nrow(prec_month)) {
  setTxtProgressBar(pb, i)
  pr = rbind(matrix(as.vector(as.matrix(prec_month_hist@data[i,])), nrow=5, ncol =12, byrow = TRUE),
             matrix(as.vector(as.matrix(prec_month@data[i,-1])), nrow=95, ncol =12, byrow = TRUE))
  rownames(pr)<-2001:2100
  colnames(pr)<-1:12
  write.table(pr, file=paste("PrecipitationSortieIFN/RCA4/rcp8.5/",codes[i],".txt",sep=""),sep="\t", quote=FALSE)
  mt = rbind(matrix(as.vector(as.matrix(meantemp_month_hist@data[i,])), nrow=5, ncol =12, byrow = TRUE),
             matrix(as.vector(as.matrix(meantemp_month@data[i,-1])), nrow=95, ncol =12, byrow = TRUE))
  rownames(mt)<-2001:2100
  colnames(mt)<-1:12
  write.table(mt, file=paste("MeanTemperatureSortieIFN/RCA4/rcp8.5/",codes[i],".txt",sep=""),sep="\t", quote=FALSE)
}
