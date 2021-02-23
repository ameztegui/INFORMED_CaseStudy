rm(list = ls())


# Requirements ----------------------------------------------------------------------------------------------------

# The code to calculate some of the parameters needed to get erosion values (rainfall erosivity, canopy cover...) can be
# found in the script "./code/meteoland_erosion.R". In principle, we don't need to load the script, since the
# objects generated there are saved as Rdata files in "./data/erosion/"

library(meteoland)
library(tidyverse)
library(raster)
library(trelliscopejs)
library(here)


load("./data/pn.rdata")
load("./data/ps.rdata")
load("./data/mx.rdata")

all_forest_list <-  c(pn_forestlist, ps_forestlist, mx_forestlist)

# Load data frames and data
load("./data/erosion/erosion_R.rdata") # rainfall erosivity (R)
load("./data/erosion/canopy_cover.Rdata")  # canopy cover data
load("./data/erosion/coords_KLSfactors.rdata")  # KLS factor (one value per plot)

# Calculate structural impact per plot (previous "A" factor)
SI_cclm_45 <- r_cclm_45 * points_wgs84$Kfactor *points_wgs84$LSfactor
SI_cclm_85 <- r_cclm_85 * points_wgs84$Kfactor *points_wgs84$LSfactor
SI_rca4_45 <- r_rca4_45 * points_wgs84$Kfactor *points_wgs84$LSfactor
SI_rca4_85 <- r_rca4_85 * points_wgs84$Kfactor *points_wgs84$LSfactor

# Soil loss (cantidad de suelo que se pierde teniendo en cuenta la fracción de cabida cubierta C) - previous "B" factor
USLE_cclm_bau_45 <- SI_cclm_45 * fcc_cclm_bau_45
USLE_cclm_bio_45 <- SI_cclm_45 * fcc_cclm_bio_45
USLE_cclm_car_45 <- SI_cclm_45 * fcc_cclm_car_45
USLE_cclm_ada_45 <- SI_cclm_45 * fcc_cclm_ada_45

USLE_cclm_bau_85 <- SI_cclm_85 * fcc_cclm_bau_85
USLE_cclm_bio_85 <- SI_cclm_85 * fcc_cclm_bio_85
USLE_cclm_car_85 <- SI_cclm_85 * fcc_cclm_car_85
USLE_cclm_ada_85 <- SI_cclm_85 * fcc_cclm_ada_85


# Erosion service -------------------------------------------------------------------------------------------------

# Erosion service as ratio
er_cclm_bau_45 <- ((SI_cclm_45 - USLE_cclm_bau_45)/SI_cclm_45) %>% pivot_longer(1:100) %>% pull(value)
er_cclm_bio_45 <- ((SI_cclm_45 - USLE_cclm_bio_45)/SI_cclm_45) %>% pivot_longer(1:100) %>% pull(value)
er_cclm_car_45 <- ((SI_cclm_45 - USLE_cclm_car_45)/SI_cclm_45) %>% pivot_longer(1:100) %>% pull(value)
er_cclm_ada_45 <- ((SI_cclm_45 - USLE_cclm_ada_45)/SI_cclm_45) %>% pivot_longer(1:100) %>% pull(value)

er_cclm_bau_85 <- ((SI_cclm_85 - USLE_cclm_bau_85)/SI_cclm_85) %>% pivot_longer(1:100) %>% pull(value)
er_cclm_bio_85 <- ((SI_cclm_85 - USLE_cclm_bio_85)/SI_cclm_85) %>% pivot_longer(1:100) %>% pull(value)
er_cclm_car_85 <- ((SI_cclm_85 - USLE_cclm_car_85)/SI_cclm_85) %>% pivot_longer(1:100) %>% pull(value)
er_cclm_ada_85 <- ((SI_cclm_85 - USLE_cclm_ada_85)/SI_cclm_85) %>% pivot_longer(1:100) %>% pull(value)

er_ratio <- c(er_cclm_bau_45,er_cclm_bio_45,er_cclm_car_45,er_cclm_ada_45,
              er_cclm_bau_85,er_cclm_bio_85,er_cclm_car_85,er_cclm_ada_85)

# Erosion service as difference

er_dif_cclm_bau_45 <- (SI_cclm_45 - USLE_cclm_bau_45) %>% pivot_longer(1:100) %>% pull(value) #toneladas
er_dif_cclm_bio_45 <- (SI_cclm_45 - USLE_cclm_bio_45) %>% pivot_longer(1:100) %>% pull(value) #toneladas
er_dif_cclm_car_45 <- (SI_cclm_45 - USLE_cclm_car_45) %>% pivot_longer(1:100) %>% pull(value) #toneladas 
er_dif_cclm_ada_45 <- (SI_cclm_45 - USLE_cclm_ada_45) %>% pivot_longer(1:100) %>% pull(value) #toneladas

er_dif_cclm_bau_85 <- (SI_cclm_85 - USLE_cclm_bau_85) %>% pivot_longer(1:100) %>% pull(value) #toneladas
er_dif_cclm_bio_85 <- (SI_cclm_85 - USLE_cclm_bio_85) %>% pivot_longer(1:100) %>% pull(value) #toneladas
er_dif_cclm_car_85 <- (SI_cclm_85 - USLE_cclm_car_85) %>% pivot_longer(1:100) %>% pull(value) #toneladas
er_dif_cclm_ada_85 <- (SI_cclm_85 - USLE_cclm_ada_85) %>% pivot_longer(1:100) %>% pull(value) #toneladas

er_dif <- c(er_dif_cclm_bau_45,er_dif_cclm_bio_45,er_dif_cclm_car_45,er_dif_cclm_ada_45,
            er_dif_cclm_bau_85,er_dif_cclm_bio_85,er_dif_cclm_car_85,er_dif_cclm_ada_85)

# Erosion service as difference relative to BAU

er_rel_cclm_bio_45 <- er_dif_cclm_bio_45 - er_dif_cclm_bau_45
er_rel_cclm_car_45 <- er_dif_cclm_car_45 - er_dif_cclm_bau_45
er_rel_cclm_ada_45 <- er_dif_cclm_ada_45 - er_dif_cclm_bau_45

er_rel_cclm_bio_85 <- er_dif_cclm_bio_85 - er_dif_cclm_bau_85
er_rel_cclm_car_85 <- er_dif_cclm_car_85 - er_dif_cclm_bau_85
er_rel_cclm_ada_85 <- er_dif_cclm_ada_85 - er_dif_cclm_bau_85

er_rel <- c(rep(NA, 26100), er_rel_cclm_bio_45,er_rel_cclm_car_45,er_rel_cclm_ada_45,
            rep(NA, 26100), er_rel_cclm_bio_85,er_rel_cclm_car_85,er_rel_cclm_ada_85)


# Canopy cover, K, LS, SI and USLE (to check)

cc_cclm_bau_45 <- fcc_cclm_bau_45 %>% pivot_longer(1:100) %>% pull(value)
cc_cclm_bio_45 <- fcc_cclm_bio_45 %>% pivot_longer(1:100) %>% pull(value)
cc_cclm_car_45 <- fcc_cclm_car_45 %>% pivot_longer(1:100) %>% pull(value)
cc_cclm_ada_45 <- fcc_cclm_ada_45 %>% pivot_longer(1:100) %>% pull(value)

cc_cclm_bau_85 <- fcc_cclm_bau_85 %>% pivot_longer(1:100) %>% pull(value)
cc_cclm_bio_85 <- fcc_cclm_bio_85 %>% pivot_longer(1:100) %>% pull(value)
cc_cclm_car_85 <- fcc_cclm_car_85 %>% pivot_longer(1:100) %>% pull(value)
cc_cclm_ada_85 <- fcc_cclm_ada_85 %>% pivot_longer(1:100) %>% pull(value)

fcc <- c(cc_cclm_bau_45, cc_cclm_bio_45, cc_cclm_car_45, cc_cclm_ada_45,
         cc_cclm_bau_85, cc_cclm_bio_85, cc_cclm_car_85, cc_cclm_ada_85)

K <- rep(rep(points_wgs84$Kfactor, each = 100), 8)
LS <- rep(rep(points_wgs84$LSfactor, each = 100), 8)

R_cclm_45 <- r_cclm_45 %>% pivot_longer(1:100) %>% pull(value)
R_cclm_85 <- r_cclm_85 %>% pivot_longer(1:100) %>% pull(value)

R <- c(rep(R_cclm_45, 4), rep(R_cclm_85,4))


si_cclm_45 <- SI_cclm_45 %>% pivot_longer(1:100) %>% pull(value)
si_cclm_85 <- SI_cclm_85 %>% pivot_longer(1:100) %>% pull(value)

SI <- c(rep(si_cclm_45, 4), rep(si_cclm_85,4))

usle_cclm_bau_45 <- USLE_cclm_bau_45 %>% pivot_longer(1:100) %>% pull(value)
usle_cclm_bio_45 <- USLE_cclm_bio_45 %>% pivot_longer(1:100) %>% pull(value)
usle_cclm_car_45 <- USLE_cclm_car_45 %>% pivot_longer(1:100) %>% pull(value)
usle_cclm_ada_45 <- USLE_cclm_ada_45 %>% pivot_longer(1:100) %>% pull(value)

usle_cclm_bau_85 <- USLE_cclm_bau_85 %>% pivot_longer(1:100) %>% pull(value)
usle_cclm_bio_85 <- USLE_cclm_bio_85 %>% pivot_longer(1:100) %>% pull(value)
usle_cclm_car_85 <- USLE_cclm_car_85 %>% pivot_longer(1:100) %>% pull(value)
usle_cclm_ada_85 <- USLE_cclm_ada_85 %>% pivot_longer(1:100) %>% pull(value)

usle <- c(usle_cclm_bau_45, usle_cclm_bio_45, usle_cclm_car_45, usle_cclm_ada_45,
          usle_cclm_bau_85, usle_cclm_bio_85, usle_cclm_car_85, usle_cclm_ada_85)


# Add plot information --------------------------------------------------------------------------------------------

year = rep(2001:2100, times = 261)
parcela = rep(names(all_forest_list),each = 100 )
for_type = ifelse(parcela %in% names(pn_forestlist),"Pinus nigra", 
                  ifelse(parcela %in% names(ps_forestlist),"Pinus sylvestris",
                         ifelse(parcela %in% names(mx_forestlist),"Mixed forest", NA)))

yall = rep(year, 8)
for_all <- rep(for_type, 8)
recipe = as.character(gl(8, 26100,
                         labels=c("CCLM_45_BAU_45", "CCLM_45_BIO_45", "CCLM_45_CAR_45", "CCLM_45_ADA_45",
                                  "CCLM_85_BAU_85", "CCLM_85_BIO_85", "CCLM_85_CAR_85", "CCLM_85_ADA_85")))

RCP = substr(recipe, 6,7)
Climate_Model = substr(recipe, 1,4)
Narrative = substr(recipe, 9,14)
Management = substr(recipe, 9,11)


er_plot = data.frame(Parcela = parcela, Recipe = recipe, RCP = RCP, Climate_Model = Climate_Model, 
                     Narrative = Narrative, Management = factor(Management), Forest_Type = for_all, 
                     Year = yall, ER = er_ratio, ERrel = er_dif, ERbau = er_rel,
                     R = R, FCC = fcc, K = K, LS = LS, SI = SI, USLE = usle)

er_plot <- er_plot %>% 
    mutate(Management = fct_relevel(Management, levels = c("BAU", "BIO","CAR","ADA")),
           Timestep = Year - 2001)


er_species <- er_plot %>%
    filter(Climate_Model == "CCLM") %>%
    group_by (Recipe, RCP, Climate_Model, Management, Narrative, Year, Forest_Type)  %>%
    summarize_at(vars(ER:ERbau), c(mean="mean", sd = "sd"), na.rm=T) %>%
    ungroup()

save(er_plot, er_species, file = "./data/ES_data/er_plot.Rdata")

# Per plot (trelliscope) ----------------------------------------------------
# # 
# er_plot %>%
#     ggplot(aes(x=Timestep, y=ER)) +
#     geom_line(aes(color = RCP)) +
#     facet_trelliscope(Parcela ~ Management , nrow = 2, ncol = 4, width = 300,
#                       path = here("reports/ER_per_plot"))
# 
# er_plot %>%
#     ggplot(aes(x=Timestep, y=ERrel)) +
#     geom_line(aes(color = RCP)) +
#     facet_trelliscope(Parcela ~ Management , nrow = 2, ncol = 4, width = 300,
#                       path = here("reports/ERrel_per_plot"))
# 
# er_plot %>%
#     ggplot(aes(x=Timestep, y=ERbau)) +
#     geom_line(aes(color = RCP)) +
#     facet_trelliscope(Parcela ~ Management , nrow = 2, ncol = 4, width = 300,
#                       path = here("reports/ERbau_per_plot"))
# 
# er_plot %>%
#     ggplot(aes(x=Timestep, y=FCC)) +
#     geom_line(aes(color = RCP)) +
#     facet_trelliscope(Parcela ~ Management , nrow = 2, ncol = 4, width = 300,
#                       path = here("reports/FCC_per_plot"))
# 
