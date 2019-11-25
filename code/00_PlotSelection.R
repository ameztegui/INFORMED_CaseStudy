
# Script to select the plots with pure Pinus nigra stands (>80% BA) within Solson?s

library(medfate)

#setwd("D:/Recerca/Lab/CaseStudy_INFORMED/")


      # Load coordinates
      ifn3_xy <- read.delim("D:/Recerca/Datasets/IFN/IFN3/ifn3_xy_cat_unique.txt", row.names=1, header=TRUE)
      coords = ifn3_xy[,-1]
      rownames(coords) = ifn3_xy[,1]
      #coords (in datum ED50!)
      ifn3_sp = SpatialPoints(coords, CRS("+proj=utm +zone=31 +ellps=intl +units=m +no_defs"))
      ifn3_sp_wgs84 = spTransform(ifn3_sp,CRS("+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
      ifn3_sp_longlat = spTransform(ifn3_sp,CRS("+proj=longlat +datum=WGS84"))
      # Read topography (TO DO: check the projection of topographic data!)
      ifn3_topo = read.table("D:/Recerca/Datasets/IFN/IFN3/ifn3_topo.txt", sep="\t", header=TRUE)

      # Load counties
      load("D:/Recerca/Datasets/Limits/comarques.rdata")
      load("D:/Recerca/Datasets/Limits/catalonia.rdata")
      comarques = spTransform(comarques, CRS("+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
      cat.contour = spTransform(cat.contour, CRS("+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
      solsones = comarques[11,]
      par(mar=c(0,0,0,0))
      plot(comarques)
      plot(cat.contour, col="red", lwd=2, add=TRUE)
      plot(solsones, col="gray", add=TRUE)


      # select IFN plots in Solsones
      sel = over(ifn3_sp_wgs84, as(solsones,"SpatialPolygons"))
      sel[is.na(sel)] = 0
      sel = (sel==1)
      ifn3_sp  = ifn3_sp[sel]
      ifn3_sp_wgs84  = ifn3_sp_wgs84[sel]
      ifn3_sp_longlat  = ifn3_sp_longlat[sel]
      ifn3_topo =ifn3_topo[sel,]
      codes = row.names(ifn3_topo)

      # Load IFN data
      load("./Rdata/IFN3_exist_SpatialForestPoints.Rdata")
      solsones_forestlist = y_ifn3_exist@forestlist[codes]
      solsones_soillist = y_ifn3_exist@soillist[codes]

#Ancillary functions
adult_ba<-function(x, minDBH) {
  a = plant.BasalArea(x)
  a = a[!is.na(a)]
  a = a[x$treeData$DBH>=minDBH]
  return(sum(a, na.rm=TRUE))
}
sp_ba<-function(x, sp, minDBH) {
  a = plant.BasalArea(x)
  b = plant.Species(x)==sp
  b = b[!is.na(a)]
  a = a[!is.na(a)]
  b = b[x$treeData$DBH>=minDBH]
  a = a[x$treeData$DBH>=minDBH]
  return(sum(a[b]))
}
density_spp<-function(x, spp) {
  dens = x$treeData$N
  sp = plant.Species(x)
  sp = sp[!is.na(dens)]
  dens = dens[!is.na(dens)]
  sp = sp[x$treeData$DBH>=2.5]
  dens = dens[x$treeData$DBH>=2.5]
  sel = sp %in% spp
  return(sum(dens[sel], na.rm=TRUE))
}


#Selection by plot characteristics
ab_total = unlist(lapply(solsones_forestlist, adult_ba, 7.5))
ab_pn = unlist(lapply(solsones_forestlist, sp_ba, 55, 7.5)) #BA P. nigra
ab_ps = unlist(lapply(solsones_forestlist, sp_ba, 59, 7.5)) #BA P. nigra

pn_pbas = ab_pn/ab_total *100
sel_pn = pn_pbas>80
sel_pn[is.na(pn_pbas)] = FALSE

ps_pbas = ab_ps/ab_total *100
sel_ps = ps_pbas>80
sel_ps[is.na(ps_pbas)] = FALSE

sel_mx = ((pn_pbas+ps_pbas)>80)
sel_mx[is.na(sel_mx)] = FALSE
sel_mx = sel_mx & (!(sel_pn | sel_ps))
plot(solsones)
points(ifn3_sp_wgs84[sel_pn,], col="black", pch=20)
points(ifn3_sp_wgs84[sel_ps,], col="red", pch=20)
points(ifn3_sp_wgs84[sel_mx,], col="blue", pch=20)


#Densitat de frondoses
spp = 0:88
spp = spp[-c(0, 54:60)]
dens_broadleaf = unlist(lapply(solsones_forestlist, density_spp, spp))
sel_pn = sel_pn & dens_broadleaf<=500
sel_ps = sel_ps & dens_broadleaf<=500
sel_mx = sel_mx & dens_broadleaf<=500
plot(solsones)
points(ifn3_sp_wgs84[sel_pn,], col="black", pch=20)
points(ifn3_sp_wgs84[sel_ps,], col="red", pch=20)
points(ifn3_sp_wgs84[sel_mx,], col="blue", pch=20)

sum(sel_pn)
sum(sel_ps)
sum(sel_mx)

#Subset data P. nigra
pn_forestlist = solsones_forestlist[sel_pn]
pn_sp_wgs84 = ifn3_sp_wgs84[sel_pn,]
pn_sp_longlat = ifn3_sp_longlat[sel_pn,]
pn_soillist = solsones_soillist[sel_pn]
pn_topo = ifn3_topo[sel_pn,]
pn_codes = codes[sel_pn]


#Save
save(pn_codes, pn_topo, pn_forestlist, pn_soillist, pn_sp_wgs84, pn_sp_longlat, solsones, cat.contour, comarques, file="Rdata/pn.rdata")


#Subset data P. sylvestris
ps_forestlist = solsones_forestlist[sel_ps]
ps_sp_wgs84 = ifn3_sp_wgs84[sel_ps,]
ps_sp_longlat = ifn3_sp_longlat[sel_ps,]
ps_soillist = solsones_soillist[sel_ps]
ps_topo = ifn3_topo[sel_ps,]
ps_codes = codes[sel_ps]

#Save
save(ps_codes, ps_topo, ps_forestlist, ps_soillist, ps_sp_wgs84, ps_sp_longlat, solsones, cat.contour, comarques, file="Rdata/ps.rdata")


#Subset data Mixed plots
mx_forestlist = solsones_forestlist[sel_mx]
mx_sp_wgs84 = ifn3_sp_wgs84[sel_mx,]
mx_sp_longlat = ifn3_sp_longlat[sel_mx,]
mx_soillist = solsones_soillist[sel_mx]
mx_topo = ifn3_topo[sel_mx,]
mx_codes = codes[sel_mx]

#Save
save(mx_codes, mx_topo, mx_forestlist, mx_soillist, mx_sp_wgs84, mx_sp_longlat, solsones, cat.contour, comarques, file="Rdata/mx.rdata")
