
# Script to select the plots with pure Pinus nigra stands (>80% BA) within Solson?s

library(medfate)
library(here)


# Load plots and soil information -----------------------------------------

      # Load coordinates
      ifn3_xy <- read.delim(here("data/ifn3_xy_cat_unique.txt"), header=TRUE)
      coords <- ifn3_xy[,-1]
      rownames(coords) <- ifn3_xy[,1]
      
      ifn3_sp <- SpatialPoints(coords, CRS("+proj=utm +zone=31 +ellps=intl +units=m +no_defs"))
      ifn3_sp_wgs84 <- spTransform(ifn3_sp, CRS("+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
      ifn3_sp_longlat <-  spTransform(ifn3_sp, CRS("+proj=longlat +datum=WGS84"))
      
      # Read topography
      ifn3_topo <- read.table(here("data/ifn3_topo.txt"), sep="\t", header=TRUE)

      # Load county limits
      load("data/carto/solsones.rdata")

      # select IFN plots in Solsones
      sel <- over(ifn3_sp_wgs84, as(solsones,"SpatialPolygons"))
      sel[is.na(sel)] <- 0
      sel <- (sel==1)
      ifn3_sp <- ifn3_sp[sel]
      ifn3_sp_wgs84 <- ifn3_sp_wgs84[sel]
      ifn3_sp_longlat <- ifn3_sp_longlat[sel]
      ifn3_topo <- ifn3_topo[sel,]
      codes <- row.names(ifn3_sp)

      # Load IFN data and select those from Solsonès
      load(here("data/IFN3_exist_SpatialForestPoints.Rdata"))
      solsones_forestlist <- y_ifn3_exist@forestlist[codes]
      solsones_soillist <- y_ifn3_exist@soillist[codes]

      
      
# Select those plost that meet given criteria  -----------------

# We are gonna select those plots that meet the following criteria:
      # - Basal area of Pinus nigra or Pinus sylvestris is >= 80%
      # - Density of broadleaves is <= 500 individuals /ha
      
# Ancillary functions
  adult_ba <- function(x, minDBH) {
    a = plant_basalArea(x)
    a = a[!is.na(a)]
    a = a[x$treeData$DBH >= minDBH]
    return(sum(a, na.rm=TRUE))
  }
      
  sp_ba <- function(x, sp, minDBH) {
    a = plant_basalArea(x)
    b = plant_species(x) == sp
    b = b[!is.na(a)]
    a = a[!is.na(a)]
    b = b[x$treeData$DBH >= minDBH]
    a = a[x$treeData$DBH >= minDBH]
    return(sum(a[b]))
  }
  
  density_spp<-function(x, spp) {
    dens = x$treeData$N
    sp = plant_species(x)
    sp = sp[!is.na(dens)]
    dens = dens[!is.na(dens)]
    sp = sp[x$treeData$DBH >= 2.5]
    dens = dens[x$treeData$DBH >= 2.5]
    sel = sp %in% spp
    return(sum(dens[sel], na.rm=TRUE))
  }


# Selection by plot characteristics
ab_total = map_dbl(solsones_forestlist, adult_ba, 7.5)  # BA total
ab_pn = map_dbl(solsones_forestlist, sp_ba, 55, 7.5)    # BA P. nigra
ab_ps = map_dbl(solsones_forestlist, sp_ba, 59, 7.5)    # BA P. sylvestris

pn_pbas = ab_pn/ab_total *100
sel_pn = pn_pbas > 80
sel_pn[is.na(pn_pbas)] = FALSE

ps_pbas = ab_ps/ab_total *100
sel_ps = ps_pbas > 80
sel_ps[is.na(ps_pbas)] = FALSE

sel_mx = (pn_pbas + ps_pbas) > 80
sel_mx[is.na(sel_mx)] = FALSE
sel_mx = sel_mx & (!(sel_pn | sel_ps))


## Visualize Plots selected so far
plot(solsones)
points(ifn3_sp_wgs84[sel_pn,], col="black", pch=20)
points(ifn3_sp_wgs84[sel_ps,], col="red", pch=20)
points(ifn3_sp_wgs84[sel_mx,], col="blue", pch=20)


# Densitat de frondoses
spp = 0:88
spp = spp[-c(0, 54:60)]
dens_broadleaf = map_dbl(solsones_forestlist, density_spp, spp)
sel_pn = sel_pn & dens_broadleaf <= 500
sel_ps = sel_ps & dens_broadleaf <= 500
sel_mx = sel_mx & dens_broadleaf <= 500


plot(solsones)
points(ifn3_sp_wgs84[sel_pn,], col="black", pch=20)
points(ifn3_sp_wgs84[sel_ps,], col="red", pch=20)
points(ifn3_sp_wgs84[sel_mx,], col="blue", pch=20)

sum(sel_pn)
sum(sel_ps)
sum(sel_mx)



# Subset and save data ----------------------------------------------------

# Subset data P. nigra
pn_forestlist = solsones_forestlist[sel_pn]
pn_sp_wgs84 = ifn3_sp_wgs84[sel_pn,]
pn_sp_longlat = ifn3_sp_longlat[sel_pn,]
pn_soillist = solsones_soillist[sel_pn]
pn_topo = ifn3_topo[sel_pn,]
pn_codes = codes[sel_pn]

save(pn_codes, pn_topo, pn_forestlist, pn_soillist, pn_sp_wgs84, pn_sp_longlat, solsones, 
     file="data/pn.rdata")


# Subset data P. sylvestris
ps_forestlist = solsones_forestlist[sel_ps]
ps_sp_wgs84 = ifn3_sp_wgs84[sel_ps,]
ps_sp_longlat = ifn3_sp_longlat[sel_ps,]
ps_soillist = solsones_soillist[sel_ps]
ps_topo = ifn3_topo[sel_ps,]
ps_codes = codes[sel_ps]

save(ps_codes, ps_topo, ps_forestlist, ps_soillist, ps_sp_wgs84, ps_sp_longlat, solsones,  
     file="data/ps.rdata")


# Subset data Mixed plots
mx_forestlist = solsones_forestlist[sel_mx]
mx_sp_wgs84 = ifn3_sp_wgs84[sel_mx,]
mx_sp_longlat = ifn3_sp_longlat[sel_mx,]
mx_soillist = solsones_soillist[sel_mx]
mx_topo = ifn3_topo[sel_mx,]
mx_codes = codes[sel_mx]

save(mx_codes, mx_topo, mx_forestlist, mx_soillist, mx_sp_wgs84, mx_sp_longlat, solsones, 
     file="data/mx.rdata")
