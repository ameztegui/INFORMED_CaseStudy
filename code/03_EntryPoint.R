rm(list=ls())

source("./R scripts/1. Regularity.R")

library(truncnorm)

# Functions to generate diameter distribution ------------------------------

      # function to generate  diameter distribution for ORGEST models
      getdiamdist <- function (x) {
            distr <- data.frame(CD=cut(x, breaks = c(0, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5,
                                            37.5, 42.5, 47.5, 52.5),
                                       labels = c( "5", "10", "15","20", "25","30",
                                                   "35", "40","45","50"))) %>%
                  group_by(CD) %>%
                  count() %>%
                  complete(CD) %>%
                  #filter(!CD =="5" ) %>%
                  replace_na(list(n= 0)) %>%
                  spread(CD,n)
            }

      # function to generate  diameter distribution for IFN plots
      getdiamdist_IFN <- function (x) {
            dbh <- unlist(x["DBH"])
            n <- unlist(x["N"])
            
            distr <- data.frame(CD=cut(dbh, breaks = c(0, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5,
                                                     37.5, 42.5, 47.5, 52.5),
                                       labels = c( "5", "10", "15","20", "25","30",
                                                   "35", "40","45","50")),
                                N = n) %>%
                  group_by(CD) %>%
                  summarise (n = sum(N)) %>%
                  complete(CD) %>%
                  filter(!CD =="5" ) %>%
                  replace_na(list(n= 0)) %>%
                  spread(CD,n)
      }

# Determine stand characteristics in the silvicultural models (ORGEST) ------------------------------
      
#### Narrativa A #### 
      
      # Pinus nigra regular (Model PN06)
            set.seed(42)
            pn_modelA <- data.frame(N=c(3400, 1700,1100,700,450,300,160,80),
                                    QMD = c(7,15,22,27,30,32,34,34)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^1.896)) %>% 
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-12, b =QMD +12, QMD,0.17*QMD)))

            
      # Pinus sylvestris regular (Model Ps08)
            set.seed(42)
            ps_modelA <- data.frame(N=c(3000,1500,1050,600,300,165,75), 
                                    QMD = c(8,15,23,31,38,39,40)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^2.071)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-12, b =QMD+12, QMD,0.17*QMD)))
            
            
      # Pinus nigra irregular (Model Pn07)
      # This model is defined by a reference structure and a rotation (how often we intervene). In each intervention,
      # the structure is modified to the reference structure.
            pn_modelA_irreg <- data.frame(N=c(379,229,42),
                                          QMD=c(11.9,24.2,34.8),
                                          BA= c(4.2,10.5,4))
      
#### Narrativa B ####
      
      # Pinus nigra (model PN06 modificado)
            set.seed(42)
            pn_modelB <- data.frame(N =c(3500,1500,800,450,230),
                                    QMD = c(8,13,22,24,25)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^1.896)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-10, b =QMD +10, QMD,0.15*QMD)))
      
      # Pinus sylvestris (model PS08 modificado)
            set.seed(42)
            ps_modelB <- data.frame(N =c(300,1500,800,400,200),
                                    QMD = c(8,15,23,25,27)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^2.071)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-10, b =QMD +10, QMD,0.15*QMD)))
            
            
#### Narrativa C ####

      # Pinus nigra (Model PN10)
            set.seed(42)
            pn_modelC <- data.frame(N = c(3500,1600,850,425,175),
                                    QMD = c(6,13,29,29,30)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^1.896)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-8, b =QMD +8, QMD,0.12*QMD)))
            
            
      # Pinus sylvestris (Model PS15)
            ps_modelC <- data.frame(N = c(2500,1800,1125,700,500,350,150),
                                    QMD = c(7,12,17,23,29,30,31)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^2.071)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-8, b =QMD +8, QMD,0.12*QMD))) 
            

#### Narrativa D #### 

      # Pinus nigra (PN09 modificado)
            set.seed(42)
            pn_modelD <- data.frame (N = c(3500,1500,800,400,200),
                                     QMD = c(6,15,18,20,21)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^1.896)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-7, b =QMD +7,QMD,0.15*QMD)))
            
            
      # Pinus sylvestris (PS15 modificado)
            set.seed(42)
            ps_modelD <- data.frame (N = c(2500,1300,800,380,130),
                                     QMD = c(7,12,17,20,22)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^2.071)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-7, b =QMD +7,QMD,0.15*QMD)))
            

# Generate diameter distribution for ORGEST e IFN plots ----------------------------
            
## Pinus nigra
      pn_dd <- map(pn_forestlist,"treeData") %>%
            map(getdiamdist_IFN)
   
      pn_modelA$dd <- map(pn_modelA$dbhs,getdiamdist)
      pn_modelB$dd <- map(pn_modelB$dbhs,getdiamdist)
      pn_modelC$dd <- map(pn_modelC$dbhs,getdiamdist)
      pn_modelD$dd <- map(pn_modelD$dbhs,getdiamdist)
      
## Pinus sylvestris
      ps_dd <- map(ps_forestlist,"treeData") %>%
            map(getdiamdist_IFN)
      ps_modelA$dd <- map(ps_modelA$dbhs,getdiamdist)
      ps_modelB$dd <- map(ps_modelB$dbhs,getdiamdist)
      ps_modelC$dd <- map(ps_modelC$dbhs,getdiamdist)
      ps_modelD$dd <- map(ps_modelD$dbhs,getdiamdist)

## Mixed forests
      mx_dd<- map(mx_forestlist,"treeData") %>%
            map(getdiamdist_IFN)
      # mx_modelA$dd <- map(mx_modelA$dbhs,getdiamdist)
      # mx_modelB$dd <- map(mx_modelB$dbhs,getdiamdist)
      # mx_modelC$dd <- map(mx_modelC$dbhs,getdiamdist)
      # mx_modelD$dd <- map(mx_modelD$dbhs,getdiamdist)

      
# Structural distances between plots and models ---------------------------

library(vegclust)

initial_stands <- function (ifn, model) {
      distance <- vegdiststruct(model, ifn, method="manhattan")
      entrypoint <- apply(distance, 2, which.min)
      dd <- model[entrypoint]
      setNames(object = dd,nm = names(ifn))
}
   
# Pinus nigra
      class(pn_modelA$dd)<-c("stratifiedvegdata","list")
      class(pn_modelB$dd)<-c("stratifiedvegdata","list")
      class(pn_modelC$dd)<-c("stratifiedvegdata","list")
      class(pn_modelD$dd)<-c("stratifiedvegdata","list")
      class(pn_dd)<-c("stratifiedvegdata","list")

      pn_sortie_A <- initial_stands(pn_dd, pn_modelA$dd)
      pn_sortie_B <- initial_stands(pn_dd, pn_modelB$dd)
      pn_sortie_C <- initial_stands(pn_dd, pn_modelC$dd)
      pn_sortie_D <- initial_stands(pn_dd, pn_modelD$dd)
      
      
# Pinus sylvestris
      class(ps_modelA$dd)<-c("stratifiedvegdata","list")
      class(ps_modelB$dd)<-c("stratifiedvegdata","list")
      class(ps_modelC$dd)<-c("stratifiedvegdata","list")
      class(ps_modelD$dd)<-c("stratifiedvegdata","list")
      class(ps_dd)<-c("stratifiedvegdata","list")
      
      ps_sortie_A <- initial_stands(ps_dd, ps_modelA$dd)
      ps_sortie_B <- initial_stands(ps_dd, ps_modelB$dd)
      ps_sortie_C <- initial_stands(ps_dd, ps_modelC$dd)
      ps_sortie_D <- initial_stands(ps_dd, ps_modelD$dd)
      
      
# Mixed forests
      class(mx_modelA$dd)<-c("stratifiedvegdata","list")
      class(mx_modelB$dd)<-c("stratifiedvegdata","list")
      class(mx_modelC$dd)<-c("stratifiedvegdata","list")
      class(mx_modelD$dd)<-c("stratifiedvegdata","list")
      class(mx_dd)<-c("stratifiedvegdata","list")

      mx_sortie_A <- initial_stands(mx_dd, mx_modelA$dd)
      mx_sortie_B <- initial_stands(mx_dd, mx_modelB$dd)
      mx_sortie_C <- initial_stands(mx_dd, mx_modelC$dd)
      mx_sortie_D <- initial_stands(mx_dd, mx_modelD$dd)
      
      
      # save(pn_sortie_A, pn_sortie_B, pn_sortie_C, pn_sortie_D,
      #      ps_sortie_A, ps_sortie_B, ps_sortie_C, ps_sortie_D,
      #      file="./Rdata/sortie_plots.Rdata")
      