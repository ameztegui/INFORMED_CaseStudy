rm(list=ls())

source("./code/01_Regularity.R")

library(truncnorm)
library(vegclust)
library(tidyverse)

# Functions to generate diameter distribution ------------------------------

      # function to generate  diameter distribution for ORGEST models
      getdiamdist <- function (x) {
            distr <- data.frame(CD=cut(x, breaks = c(0, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5,
                                            37.5, 42.5, 47.5, 52.5),
                                       labels = c( "5", "10", "15","20", "25","30",
                                                   "35", "40","45","50"))) %>%
                  group_by(CD) %>%
                  count() %>%
                  complete(CD, fill = list(n = 0)) %>%
                  unique() %>%
                  #filter(!CD =="5" ) %>%
                  # replace_na(list(n= 0)) %>%
                  pivot_wider(names_from = CD, values_from = n)
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
                  complete(CD, fill = list(n = 0)) %>%
                  filter(!CD =="5" ) %>%
                  spread(CD,n)
      }

# Determine stand characteristics in the silvicultural models (ORGEST) ------------------------------
      
#### Narrativa A: Carbon storage; RCP 4.5 #### 
      
      # Pinus nigra regular (Model PN06)
            set.seed(42)
            pn_orgest_CAR_45 <- data.frame(N=c(3400, 1700,1100,700,450,300,160,80),
                                    QMD = c(7,15,22,27,30,32,34,34)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^1.896)) %>% 
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-12, b =QMD +12, QMD,0.17*QMD)))

            
      # Pinus sylvestris regular (Model Ps08)
            set.seed(42)
            ps_orgest_CAR_45 <- data.frame(N=c(3000,1500,1050,600,300,165,75), 
                                    QMD = c(8,15,23,31,38,39,40)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^2.071)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-12, b =QMD+12, QMD,0.17*QMD)))
            
      
#### Narrativa B: WOOD ENERGY ####
      # Pinus nigra (model PN06 modificado)
            set.seed(42)
            pn_orgest_BIO_45 <- data.frame(N =c(3500,1500,800,450,230),
                                    QMD = c(8,13,22,24,25)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^1.896)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-10, b =QMD +10, QMD,0.15*QMD)))
      
      # Pinus sylvestris (model PS08 modificado)
            set.seed(42)
            ps_orgest_BIO_45 <- data.frame(N =c(3000,1500,800,400,200),
                                    QMD = c(8,15,23,25,27)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^2.071)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-10, b =QMD +10, QMD,0.15*QMD)))
            
            
#### Narrative C: VULNERABILITY REDUCTION, RCP4.5 ####
        # Pinus nigra (model PN06 modificado)
        set.seed(42)
        pn_orgest_VUL_45 <- data.frame(N =c(3400,1700,935,514,283,141),
                                   QMD = c(5,12.5,20,25,27,32)) %>%
            mutate(BA = (pi/4)*N*(QMD/100)^2,
                   SDI = (N * (QMD/25)^1.896)) %>%
            rowwise() %>%
            mutate(dbhs = list(rtruncnorm(N, a= QMD-10, b =QMD +10, QMD,0.15*QMD)))
        
        # Pinus sylvestris (model PS02 modificado)
        set.seed(42)
        ps_orgest_VUL_45 <- data.frame(N =c(3000,1500,825,454,250,150,60),
                                   QMD = c(6,14,20,26,29,31,35)) %>%
            mutate(BA = (pi/4)*N*(QMD/100)^2,
                   SDI = (N * (QMD/25)^2.071)) %>%
            rowwise() %>%
            mutate(dbhs = list(rtruncnorm(N, a= QMD-10, b =QMD +10, QMD,0.15*QMD)))
        
            
#### Narrativa D: Carbon storage; RCP 8.5 ####

      # Pinus nigra (Model PN10)
            set.seed(42)
            pn_orgest_CAR_85 <- data.frame(N = c(3500,1600,850,425,175),
                                    QMD = c(6,13,29,29,30)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^1.896)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-8, b =QMD +8, QMD,0.12*QMD)))
            
            
      # Pinus sylvestris (Model PS16)
            ps_orgest_CAR_85 <- data.frame(N = c(2500,1800,1125,700,500,350,150),
                                    QMD = c(7,12,17,23,29,30,31)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^2.071)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-8, b =QMD +8, QMD,0.12*QMD))) 
            

#### Narrativa E: WOODE ENERGY; RCP 8.5 #### 

      # Pinus nigra (PN09 modificado)
            set.seed(42)
            pn_orgest_BIO_85 <- data.frame (N = c(3500,1500,800,400,200),
                                     QMD = c(6,15,18,20,21)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^1.896)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-7, b =QMD +7,QMD,0.15*QMD)))
            
            
      # Pinus sylvestris (PS15 modificado)
            set.seed(42)
            ps_orgest_BIO_85 <- data.frame (N = c(2500,1300,800,380,130),
                                     QMD = c(7,12,17,20,22)) %>%
                  mutate(BA = (pi/4)*N*(QMD/100)^2,
                         SDI = (N * (QMD/25)^2.071)) %>%
                  rowwise() %>%
                  mutate(dbhs = list(rtruncnorm(N, a= QMD-7, b =QMD +7,QMD,0.15*QMD)))

#### Narrativa F: VULNERABILITY REDUCTION; RCP 8.5 #### 

        # Pinus nigra (model PN06 modificado)
          set.seed(42)
          pn_orgest_VUL_85 <- data.frame(N =c(3000,1500,675,304,167,84),
                                   QMD = c(5,11,16,19,22,27)) %>%
              mutate(BA = (pi/4)*N*(QMD/100)^2,
                   SDI = (N * (QMD/25)^1.896)) %>%
              rowwise() %>%
              mutate(dbhs = list(rtruncnorm(N, a= QMD-10, b =QMD +10, QMD,0.15*QMD)))
        
        # Pinus sylvestris (model PS02 modificado)
          set.seed(42)
          ps_orgest_VUL_85 <- data.frame(N =c(2500,1500,675,304,182,73),
                                   QMD = c(6,12.5,17,22.5,25,28)) %>%
              mutate(BA = (pi/4)*N*(QMD/100)^2,
                   SDI = (N * (QMD/25)^2.071)) %>%
              rowwise() %>%
              mutate(dbhs = list(rtruncnorm(N, a= QMD-10, b =QMD +10, QMD,0.15*QMD)))
        
# Generate diameter distribution for ORGEST e IFN plots ----------------------------
            
## Pinus nigra
      pn_dd <- map(pn_forestlist,"treeData") %>%
            map(getdiamdist_IFN)
   
      pn_orgest_CAR_45$dd <- map(pn_orgest_CAR_45$dbhs,getdiamdist)
      pn_orgest_BIO_45$dd <- map(pn_orgest_BIO_45$dbhs,getdiamdist)
      pn_orgest_VUL_45$dd <- map(pn_orgest_VUL_45$dbhs,getdiamdist)
      pn_orgest_CAR_85$dd <- map(pn_orgest_CAR_85$dbhs,getdiamdist)
      pn_orgest_BIO_85$dd <- map(pn_orgest_BIO_85$dbhs,getdiamdist)
      pn_orgest_VUL_85$dd <- map(pn_orgest_VUL_85$dbhs,getdiamdist)

## Pinus sylvestris
      ps_dd <- map(ps_forestlist,"treeData") %>%
            map(getdiamdist_IFN)
      ps_orgest_CAR_45$dd <- map(ps_orgest_CAR_45$dbhs,getdiamdist)
      ps_orgest_BIO_45$dd <- map(ps_orgest_BIO_45$dbhs,getdiamdist)
      ps_orgest_VUL_45$dd <- map(ps_orgest_VUL_45$dbhs,getdiamdist)
      ps_orgest_CAR_85$dd <- map(ps_orgest_CAR_85$dbhs,getdiamdist)
      ps_orgest_BIO_85$dd <- map(ps_orgest_BIO_85$dbhs,getdiamdist)
      ps_orgest_VUL_85$dd <- map(ps_orgest_VUL_85$dbhs,getdiamdist)

## Mixed forests
      mx_dd<- map(mx_forestlist,"treeData") %>%
            map(getdiamdist_IFN)
      mx_orgest_CAR_45$dd <- map(mx_orgest_CAR_45$dbhs,getdiamdist)
      mx_orgest_BIO_45$dd <- map(mx_orgest_BIO_45$dbhs,getdiamdist)
      mx_orgest_VUL_45$dd <- map(mx_orgest_VUL_45$dbhs,getdiamdist)
      mx_orgest_CAR_85$dd <- map(mx_orgest_CAR_85$dbhs,getdiamdist)
      mx_orgest_BIO_85$dd <- map(mx_orgest_BIO_85$dbhs,getdiamdist)
      mx_orgest_VUL_85$dd <- map(mx_orgest_VUL_85$dbhs,getdiamdist)

# Structural distances between plots and models ---------------------------

initial_stands <- function (ifn, model) {
      distance <- vegdiststruct(model, ifn, method="manhattan")
      entrypoint <- apply(distance, 2, which.min)
      dd <- model[entrypoint]
      setNames(object = dd,nm = names(ifn))
}
   
# Pinus nigra
      class(pn_orgest_CAR_45$dd)<-c("stratifiedvegdata","list")
      class(pn_orgest_BIO_45$dd)<-c("stratifiedvegdata","list")
      class(pn_orgest_VUL_45$dd)<-c("stratifiedvegdata","list")
      class(pn_orgest_CAR_85$dd)<-c("stratifiedvegdata","list")
      class(pn_orgest_BIO_85$dd)<-c("stratifiedvegdata","list")
      class(pn_orgest_VUL_85$dd)<-c("stratifiedvegdata","list")
      class(pn_dd)<-c("stratifiedvegdata","list")

      pn_sortie_A <- initial_stands(pn_dd, pn_orgest_CAR_45$dd)
      pn_sortie_B <- initial_stands(pn_dd, pn_orgest_BIO_45$dd)
      pn_sortie_C <- initial_stands(pn_dd, pn_orgest_VUL_45$dd)
      pn_sortie_D <- initial_stands(pn_dd, pn_orgest_CAR_85$dd)
      pn_sortie_E <- initial_stands(pn_dd, pn_orgest_BIO_85$dd)
      pn_sortie_F <- initial_stands(pn_dd, pn_orgest_VUL_85$dd)
      
      
# Pinus sylvestris
      class(ps_orgest_CAR_45$dd)<-c("stratifiedvegdata","list")
      class(ps_orgest_BIO_45$dd)<-c("stratifiedvegdata","list")
      class(ps_orgest_VUL_45$dd)<-c("stratifiedvegdata","list")
      class(ps_orgest_CAR_85$dd)<-c("stratifiedvegdata","list")
      class(ps_orgest_BIO_85$dd)<-c("stratifiedvegdata","list")
      class(ps_orgest_VUL_85$dd)<-c("stratifiedvegdata","list")
      class(ps_dd)<-c("stratifiedvegdata","list")
      
      ps_sortie_A <- initial_stands(ps_dd, ps_orgest_CAR_45$dd)
      ps_sortie_B <- initial_stands(ps_dd, ps_orgest_BIO_45$dd)
      ps_sortie_C <- initial_stands(ps_dd, ps_orgest_VUL_45$dd)
      ps_sortie_D <- initial_stands(ps_dd, ps_orgest_CAR_85$dd)
      ps_sortie_E <- initial_stands(ps_dd, ps_orgest_BIO_85$dd)
      ps_sortie_F <- initial_stands(ps_dd, ps_orgest_VUL_85$dd)
      
# Mixed forests
      class(mx_orgest_CAR_45$dd)<-c("stratifiedvegdata","list")
      class(mx_orgest_BIO_45$dd)<-c("stratifiedvegdata","list")
      class(mx_orgest_VUL_45$dd)<-c("stratifiedvegdata","list")
      class(mx_orgest_CAR_85$dd)<-c("stratifiedvegdata","list")
      class(mx_orgest_BIO_85$dd)<-c("stratifiedvegdata","list")
      class(mx_orgest_VUL_85$dd)<-c("stratifiedvegdata","list")
      class(mx_dd)<-c("stratifiedvegdata","list")

      mx_sortie_A <- initial_stands(mx_dd, mx_orgest_CAR_45$dd)
      mx_sortie_B <- initial_stands(mx_dd, mx_orgest_BIO_45$dd)
      mx_sortie_C <- initial_stands(mx_dd, mx_orgest_VUL_45$dd)
      mx_sortie_D <- initial_stands(mx_dd, mx_orgest_CAR_85$dd)
      mx_sortie_E <- initial_stands(mx_dd, mx_orgest_BIO_85$dd)
      mx_sortie_F <- initial_stands(mx_dd, mx_orgest_VUL_85$dd)
      
      
      # save(pn_sortie_A, pn_sortie_B, pn_sortie_C, pn_sortie_D, pn_sortie_E, pn_sortie_F pn_dd,
      #      ps_sortie_A, ps_sortie_B, ps_sortie_C, ps_sortie_D, pn_sortie_E, pn_sortie_F, ps_dd, 
      #      mx_sortie_A, mx_sortie_B, mx_sortie_C, mx_sortie_D, mx_sortie_E, mx_sortie_F, mx_dd, 
      #      file="./Rdata/sortie_plots.Rdata")
      