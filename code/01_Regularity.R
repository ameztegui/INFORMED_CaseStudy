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
      averageQuadraticDiameter <- function(x, minDBH = 7.5) {
            ba <- plant_basalArea(x)
            N <- x$treeData$N
            dbh <- x$treeData$DBH
            ba <- ba[1:length(dbh)]
            N <-N[1:length(dbh)]
            Nsum <- sum(N[dbh>=minDBH])
            BAsum <- sum(ba[dbh>=minDBH])
            return(200*sqrt((BAsum/Nsum)/pi))
      }
      
      qmd_pn <- map_dbl(pn_forestlist,averageQuadraticDiameter)
      qmd_ps <- map_dbl(ps_forestlist,averageQuadraticDiameter)
      qmd_mx <- map_dbl(mx_forestlist,averageQuadraticDiameter)
      
      # Density (N)
      currentDensity <- function(x, minDBH = 7.5){
            N <- x$treeData$N
            dbh <- x$treeData$DBH
            N <- N[1:length(dbh)]
            return(sum(N[dbh>=minDBH]))
      }
      
      N_pn <- map_dbl(pn_forestlist,currentDensity)
      N_mx <- map_dbl(mx_forestlist,currentDensity)
      N_ps <- map_dbl(ps_forestlist,currentDensity)
      
      # Basal Area & SDI Reineke
      
      ba_pn <- map_dbl(pn_forestlist,forest_basalArea)
      SDI_pn <- N_pn * (qmd_pn/25)^1.896
      SDIrel_pn <- SDI_pn/1287
      
      ba_ps <- map_dbl(ps_forestlist,forest_basalArea)
      SDI_ps <- N_ps * (qmd_ps/25)^1.896
      SDIrel_ps <- SDI_ps/1287

      ba_mx <- map_dbl(mx_forestlist,forest_basalArea)
      SDI_mx <- (N_mx * (qmd_mx/25)^1.896)
      SDIrel_mx <- SDI_mx/1287
      

# Determine proportion of BA by size classes ------------------------------
      
# Per a aquesta classificació s’han d,agrupar les classes diamètriques en tres grups: 
#   petit (CDs 10, 15 i 20) mitjà (CDs 25 i 30) i gran (CDs 35 i superiors). 
# Una parcel·la es considera regular o regularitzada quan més del 80% de l’AB correspon a un sol grup; 
# es considera semiregular quan dos grups no contigus sumen més del 80% de l’AB total; 
# i es considera irregular o irregularitzada quan la suma de l’AB de dos grups no supera el 80% de l’AB total.

      diameterClasses <- function(x, breaks, percentage = TRUE) {
            ba <- plant_basalArea(x)
            f <- cut(x$treeData$DBH, breaks)
            ba <- ba[1:length(f)]
            a <- tapply(ba, f, "sum", na.rm=TRUE)
            a[is.na(a)]=0
            if(percentage) a = a/sum(a, na.rm=TRUE)
            return(a)
      }

      breaks = c(7.5, 22.5, 32.5, 100)
      b_pn = map(pn_forestlist,diameterClasses,breaks) 
      b_ps = map(ps_forestlist,diameterClasses,breaks) 
      b_mx = map(mx_forestlist,diameterClasses,breaks) 
      
      # Forma principal de masa
      formaPrincipal <- function(percClass) {
            regular = sum(percClass > 0.8) == 1
            regularitzada = ((percClass[1] + percClass[2] > 0.8)| (percClass[2] + percClass[3] > 0.8)) & !regular
            semirregular = ((percClass[1] + percClass[3] > 0.8)) & !regular
            forma = "irregular"
            forma[regular] = "regular"
            forma[regularitzada] ="regularitzada"
            forma[semirregular] ="semirregular"
            return(forma)
      }
      
      forma_pn <- map_chr(b_pn,formaPrincipal)
      forma_ps <- map_chr(b_ps,formaPrincipal)
      forma_mx <- map_chr(b_mx,formaPrincipal)
      
      

