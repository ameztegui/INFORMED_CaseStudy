#################################################
#  TRANSLATE SORTIE OUTPUTS INTO MEDFATE INPUT
#################################################
# TO UPDATE METEOLAND FROM MEDFATE
# library(devtools)
# install_github("miquelcaceres/medfate", ref="devel")

library(medfate)
data(SpParamsMED)

load("data/pn.rdata")
load("data/ps.rdata")
load("data/mx.rdata")

MD_codes = c(55,59)
names(MD_codes) = c("PINI","PISY")


buildPlotForestList_PS<-function(data, id) {
  dataplot = data[data$Parcela == id, ]
  forest_list = vector("list", 100)
  for(i in 1:100) {
    forest_list[[i]] = ps_forestlist[[id]]
    forest_list[[i]]$seedBank = NULL
    dataplottime = dataplot[dataplot$Timestep == (i-1),]
    td = data.frame(Species=MD_codes[dataplottime$Species], N = dataplottime$N,
                    DBH = as.numeric(substr(dataplottime$CD,3,4)),
                    Height = dataplottime$Height*100,
                    Z50 = rep(NA, nrow(dataplottime)), Z95=rep(NA, nrow(dataplottime)))
    forest_list[[i]]$treeData = td
  }
  return(forest_list)
}

buildPlotForestList_PN <- function(data, id) {
  dataplot = data[data$Parcela == id, ]
  forest_list = vector("list", 100)
  for(i in 1:100) {
    forest_list[[i]] = pn_forestlist[[id]]
    forest_list[[i]]$seedBank = NULL
    dataplottime = dataplot[dataplot$Timestep == (i-1),]
    td = data.frame(Species=MD_codes[dataplottime$Species], N = dataplottime$N,
                    DBH = as.numeric(substr(dataplottime$CD,3,4)),
                    Height = dataplottime$Height*100,
                    Z50 = rep(NA, nrow(dataplottime)), Z95=rep(NA, nrow(dataplottime)))
    forest_list[[i]]$treeData = td
  }
  return(forest_list)
}
buildPlotForestList_MX<-function(data, id) {
  dataplot = data[data$Parcela == id, ]
  forest_list = vector("list", 100)
  for(i in 1:100) {
    forest_list[[i]] = mx_forestlist[[id]]
    forest_list[[i]]$seedBank = NULL
    dataplottime = dataplot[dataplot$Timestep == (i-1),]
    td = data.frame(Species=MD_codes[dataplottime$Species], N = dataplottime$N,
                    DBH = as.numeric(substr(dataplottime$CD,3,4)),
                    Height = dataplottime$Height*100,
                    Z50 = rep(NA, nrow(dataplottime)), Z95=rep(NA, nrow(dataplottime)))
    forest_list[[i]]$treeData = td
  }
  return(forest_list)
}
getCanopyCoverFromLightExtinction<-function(forest_list) {
  cc = numeric(length(forest_list))
  for(i in 1:length(forest_list)) {
    if((nrow(forest_list[[i]]$treeData)>0) || (nrow(forest_list[[i]]$shrubData)>0)) {
      cc[i]=100-vprofile.PARExtinction(forest_list[[i]], SpParamsMED, draw=FALSE)[1]
    }
  }
  return(cc)
}


