library(meteoland)


load("Rdata/pn.rdata")
load("Rdata/ps.rdata")
load("Rdata/mx.rdata")

sp_wgs84 = SpatialPoints(rbind(pn_sp_wgs84@coords, ps_sp_wgs84@coords, mx_sp_wgs84@coords), pn_sp_wgs84@proj4string)

spt = SpatialPointsTopography(sp_wgs84, 
                              elevation = c(pn_topo$elevation, ps_topo$elevation,mx_topo$elevation),
                              slope=c(pn_topo$slope, ps_topo$slope,mx_topo$slope),
                              aspect = c(pn_topo$aspect, ps_topo$aspect,mx_topo$aspect))


load("D:/Recerca/Datasets/Climate/MeteorologyInterpolationData/Interpolator_Mother.rda")

hist_dates = seq.Date(as.Date("1990-01-01"),as.Date("2005-12-31"), by="day")

solsones_interpolator = subsample(interpolator, bbox=spt@bbox, dates=hist_dates, buffer=30000)

spt@proj4string = solsones_interpolator@proj4string
mp = interpolationpoints(solsones_interpolator, spt,
                         export=TRUE, exportDir = "../CaseStudy_INFORMED_RARS/InterpolatedMeteoIFN",
                         dates = hist_dates)
