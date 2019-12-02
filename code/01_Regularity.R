#
# Script to define the regularity of IFN plots 
# based on the repartition on size classes.
#


rm(list=ls())

load("data/pn.rdata")
load("data/ps.rdata")
load("data/mx.rdata")

library(medfate)
library(tidyverse)
data("SpParamsMED")

# Calculate, for each plot, Dg, N, BA and SDI -----------------------------

      # Average quadratic diameter (Dg)
      averageQuadraticDiameter<-function(x, minDBH = 7.5) {
            ba<-plant_basalArea(x)
            N<-x$treeData$N
            dbh = x$treeData$DBH
            ba<-ba[1:length(dbh)]
            N <-N[1:length(dbh)]
            Nsum = sum(N[dbh>=minDBH])
            BAsum = sum(ba[dbh>=minDBH])
            return(200*sqrt((BAsum/Nsum)/pi))
      }
      
      qmd_pn <- unlist(lapply(pn_forestlist,averageQuadraticDiameter))
      qmd_ps <- unlist(lapply(ps_forestlist,averageQuadraticDiameter))
      qmd_mx <- unlist(lapply(mx_forestlist,averageQuadraticDiameter))
      
      # Density (N)
      currentDensity<-function(x, minDBH = 7.5){
            N<-x$treeData$N
            dbh = x$treeData$DBH
            N <-N[1:length(dbh)]
            return(sum(N[dbh>=minDBH]))
      }
      
      N_pn<-unlist(lapply(pn_forestlist,currentDensity))
      N_ps<-unlist(lapply(ps_forestlist,currentDensity))
      N_mx<-unlist(lapply(mx_forestlist,currentDensity))
      
      # Basal Area & SDI Reineke
      
      ba_pn<-unlist(lapply(pn_forestlist,forest_basalArea))
      SDI_pn <- (N_pn * (qmd_pn/25)^1.896)
      SDIrel_pn <- SDI_pn/1287
      
      ba_ps<-unlist(lapply(ps_forestlist,forest_basalArea))
      SDI_ps <- (N_ps * (qmd_ps/25)^1.896)
      SDIrel_ps <- SDI_ps/1287

      ba_mx<-unlist(lapply(mx_forestlist,forest_basalArea))
      SDI_mx <- (N_mx * (qmd_mx/25)^1.896)
      SDIrel_mx <- SDI_mx/1287
      

# Determine proportion of BA by size classes ------------------------------
      
# Per a aquesta classificació s’han d,agrupar les classes diamètriques en tres grups: 
#   petit (CDs 10, 15 i 20) mitjà (CDs 25 i 30) i gran (CDs 35 i superiors). 
# Una parcel·la es considera regular o regularitzada quan més del 80% de l’AB correspon a un sol grup; 
# es considera semiregular quan dos grups no contigus sumen més del 80% de l’AB total; 
# i es considera irregular o irregularitzada quan la suma de l’AB de dos grups no supera el 80% de l’AB total.

      diameterClasses<-function(x, breaks, percentage = TRUE) {
            ba<-plant_basalArea(x)
            f<-cut(x$treeData$DBH,breaks)
            ba<-ba[1:length(f)]
            a = tapply(ba, f, "sum", na.rm=TRUE)
            a[is.na(a)]=0
            if(percentage) a = a/sum(a, na.rm=TRUE)
            return(a)
      }

      breaks=c(7.5,22.5,32.5,100)
      b_pn = lapply(pn_forestlist,diameterClasses,breaks) 
      b_ps = lapply(ps_forestlist,diameterClasses,breaks) 
      b_mx = lapply(mx_forestlist,diameterClasses,breaks) 
      
      # Forma principal de masa
      formaPrincipal<-function(percClass) {
            regular = sum(percClass>0.8)==1
            regularitzada = ((percClass[1]+percClass[2]>0.8)| (percClass[2]+percClass[3]>0.8)) & !regular
            semirregular = ((percClass[1]+percClass[3]>0.8)) & !regular
            forma = "irregular"
            forma[regular] = "regular"
            forma[regularitzada] ="regularitzada"
            forma[semirregular] ="semirregular"
            return(forma)
      }
      
      forma_pn<-unlist(lapply(b_pn,formaPrincipal))
      forma_ps<-unlist(lapply(b_ps,formaPrincipal))
      forma_mx<-unlist(lapply(b_mx,formaPrincipal))
      
      

# plot(solsones)
# 
# points(pn_sp_wgs84[res$forma=="regular",],pch =20, col="black")
# points(pn_sp_wgs84[res$forma=="regularitzada",],pch =20, col="gray")
# points(pn_sp_wgs84[res$forma=="semirregular",],pch =20, col=" green")
# points(pn_sp_wgs84[res$forma=="irregular",],pch =20, col="red")
