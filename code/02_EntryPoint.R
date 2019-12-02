rm(list=ls())

source("./code/01_Regularity.R")

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
      
#### Narrativa A: Carbon storage; RCP 4.5 #### 
      
      # Pinus nigra regular (Model PN06)
            set.seed(42)
            pn_model_scA <- data.frame(N=c(3400, 1700,1100,700,450,300,160,80),
                                    QMD = c(7,15,22,27,30,32,34,34)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^1.896)) %>% 
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-12, b =QMD +12, QMD,0.17*QMD)))

            
      # Pinus sylvestris regular (Model Ps08)
            set.seed(42)
            ps_model_scA <- data.frame(N=c(3000,1500,1050,600,300,165,75), 
                                    QMD = c(8,15,23,31,38,39,40)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^2.071)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-12, b =QMD+12, QMD,0.17*QMD)))
            
      
#### Narrativa B: Biomass ####
      # Pinus nigra (model PN06 modificado)
            set.seed(42)
            pn_model_scB <- data.frame(N =c(3500,1500,800,450,230),
                                    QMD = c(8,13,22,24,25)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^1.896)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-10, b =QMD +10, QMD,0.15*QMD)))
      
      # Pinus sylvestris (model PS08 modificado)
            set.seed(42)
            ps_model_scB <- data.frame(N =c(3000,1500,800,400,200),
                                    QMD = c(8,15,23,25,27)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^2.071)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-10, b =QMD +10, QMD,0.15*QMD)))
            
            
#### Narrativa C: Adaptation, RCP4.5 ####
        # Pinus nigra (model PN06 modificado)
        set.seed(42)
        pn_model_scC <- data.frame(N =c(3400,1700,935,514,283,141),
                                   QMD = c(5,12.5,20,25,27,32)) %>%
            mutate(BA = (pi/4)*N*(QMD/100)^2,
                   SDI = (N * (QMD/25)^1.896)) %>%
            rowwise() %>%
            mutate(dbhs = list(rtruncnorm(N, a= QMD-10, b =QMD +10, QMD,0.15*QMD)))
        
        # Pinus sylvestris (model PS02 modificado)
        set.seed(42)
        ps_model_scC <- data.frame(N =c(3000,1500,825,454,250,150,60),
                                   QMD = c(6,14,20,26,29,31,35)) %>%
            mutate(BA = (pi/4)*N*(QMD/100)^2,
                   SDI = (N * (QMD/25)^2.071)) %>%
            rowwise() %>%
            mutate(dbhs = list(rtruncnorm(N, a= QMD-10, b =QMD +10, QMD,0.15*QMD)))
        
            
#### Narrativa D: Carbon storage; RCP 8.5 ####

      # Pinus nigra (Model PN10)
            set.seed(42)
            pn_model_scD <- data.frame(N = c(3500,1600,850,425,175),
                                    QMD = c(6,13,29,29,30)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^1.896)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-8, b =QMD +8, QMD,0.12*QMD)))
            
            
      # Pinus sylvestris (Model PS16)
            ps_model_scD <- data.frame(N = c(2500,1800,1125,700,500,350,150),
                                    QMD = c(7,12,17,23,29,30,31)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^2.071)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-8, b =QMD +8, QMD,0.12*QMD))) 
            

#### Narrativa E: Biomass; RCP 8.5 #### 

      # Pinus nigra (PN09 modificado)
            set.seed(42)
            pn_model_scE <- data.frame (N = c(3500,1500,800,400,200),
                                     QMD = c(6,15,18,20,21)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^1.896)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-7, b =QMD +7,QMD,0.15*QMD)))
            
            
      # Pinus sylvestris (PS15 modificado)
            set.seed(42)
            ps_model_scE <- data.frame (N = c(2500,1300,800,380,130),
                                     QMD = c(7,12,17,20,22)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^2.071)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-7, b =QMD +7,QMD,0.15*QMD)))

#### Narrativa F: Adaptation; RCP 8.5 #### 

        # Pinus nigra (model PN06 modificado)
        set.seed(42)
        pn_model_scF <- data.frame(N =c(3000,1500,675,304,167,84),
                                   QMD = c(5,11,16,19,22,27)) %>%
            mutate(BA = (pi/4)*N*(QMD/100)^2,
                   SDI = (N * (QMD/25)^1.896)) %>%
            rowwise() %>%
            mutate(dbhs = list(rtruncnorm(N, a= QMD-10, b =QMD +10, QMD,0.15*QMD)))
        
        # Pinus sylvestris (model PS02 modificado)
        set.seed(42)
        ps_model_scF <- data.frame(N =c(2500,1500,675,304,182,73),
                                   QMD = c(6,12.5,17,22.5,25,28)) %>%
            mutate(BA = (pi/4)*N*(QMD/100)^2,
                   SDI = (N * (QMD/25)^2.071)) %>%
            rowwise() %>%
            mutate(dbhs = list(rtruncnorm(N, a= QMD-10, b =QMD +10, QMD,0.15*QMD)))
        
# Generate diameter distribution for ORGEST e IFN plots ----------------------------
            
## Pinus nigra
      pn_dd <- map(pn_forestlist,"treeData") %>%
            map(getdiamdist_IFN)
   
      pn_model_scA$dd <- map(pn_model_scA$dbhs,getdiamdist)
      pn_model_scB$dd <- map(pn_model_scB$dbhs,getdiamdist)
      pn_model_scC$dd <- map(pn_model_scC$dbhs,getdiamdist)
      pn_model_scD$dd <- map(pn_model_scD$dbhs,getdiamdist)
      pn_model_scE$dd <- map(pn_model_scE$dbhs,getdiamdist)
      pn_model_scF$dd <- map(pn_model_scF$dbhs,getdiamdist)
      
## Pinus sylvestris
      ps_dd <- map(ps_forestlist,"treeData") %>%
            map(getdiamdist_IFN)
      ps_model_scA$dd <- map(ps_model_scA$dbhs,getdiamdist)
      ps_model_scB$dd <- map(ps_model_scB$dbhs,getdiamdist)
      ps_model_scC$dd <- map(ps_model_scC$dbhs,getdiamdist)
      ps_model_scD$dd <- map(ps_model_scD$dbhs,getdiamdist)

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
      class(pn_model_sc$dd)<-c("stratifiedvegdata","list")
      class(pn_model_sc$dd)<-c("stratifiedvegdata","list")
      class(pn_model_sc$dd)<-c("stratifiedvegdata","list")
      class(pn_model_sc$dd)<-c("stratifiedvegdata","list")
      class(pn_dd)<-c("stratifiedvegdata","list")

      pn_sortie_scA <- initial_stands(pn_dd, pn_model_scA$dd)
      pn_sortie_scB <- initial_stands(pn_dd, pn_model_scB$dd)
      pn_sortie_scC <- initial_stands(pn_dd, pn_model_scC$dd)
      pn_sortie_scD <- initial_stands(pn_dd, pn_model_scD$dd)
      sc
      
# Pinus sylvestris
      class(ps_model_scA$dd)<-c("stratifiedvegdata","list")
      class(ps_model_scB$dd)<-c("stratifiedvegdata","list")
      class(ps_model_scC$dd)<-c("stratifiedvegdata","list")
      class(ps_model_scD$dd)<-c("stratifiedvegdata","list")
      class(ps_dd)<-c("stratifiedvegdata","list")
      
      ps_sortie_scA <- initial_stands(ps_dd, ps_model_scA$dd)
      ps_sortie_scB <- initial_stands(ps_dd, ps_model_scB$dd)
      ps_sortie_scC <- initial_stands(ps_dd, ps_model_scC$dd)
      ps_sortie_scD <- initial_stands(ps_dd, ps_model_scD$dd)
      
      
# Mixed forests
      class(mx_model_scA$dd)<-c("stratifiedvegdata","list")
      class(mx_model_scB$dd)<-c("stratifiedvegdata","list")
      class(mx_model_scC$dd)<-c("stratifiedvegdata","list")
      class(mx_model_scD$dd)<-c("stratifiedvegdata","list")
      class(mx_dd)<-c("stratifiedvegdata","list")

      mx_sortie_scA <- initial_stands(mx_dd, mx_model_scA$dd)
      mx_sortie_scB <- initial_stands(mx_dd, mx_model_scB$dd)
      mx_sortie_scC <- initial_stands(mx_dd, mx_model_scC$dd)
      mx_sortie_scD <- initial_stands(mx_dd, mx_model_scD$dd)
      
      
      # save(pn_sortie_A, pn_sortie_B, pn_sortie_C, pn_sortie_D,
      #      ps_sortie_A, ps_sortie_B, ps_sortie_C, ps_sortie_D,
      #      file="./Rdata/sortie_plots.Rdata")
      