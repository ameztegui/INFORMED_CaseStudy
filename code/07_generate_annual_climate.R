rm(list =ls())


library(tidyverse)
library(patchwork)
library(trelliscopejs)
library(here)

load("./data/climatic_data_new.Rdata")

load("./data/pn.rdata")
load("./data/ps.rdata")
load("./data/mx.rdata")


# Prepare climatic datafiles --------------------------------------------------------------------------------------

names <- c("tm01", "tm02", "tm03", "tm04", "tm05", "tm06", "tm07", "tm08", "tm09", "tm10", "tm11", "tm12",
           "pr01", "pr02", "pr03", "pr04", "pr05", "pr06", "pr07", "pr08", "pr09", "pr10", "pr11", "pr12")
           
cclm_45 <- map_df(CCLM_4.5, "temp", .id = "Parcela") %>% 
    bind_cols(map_df(CCLM_4.5, "prec")) %>%
    setNames(., c("Parcela", names)) %>%
    mutate(Climate_Model = "CCLM",
           RCP = "RCP 4.5",
           Timestep = rep(seq(0,99), 261))

cclm_85 <- map_df(CCLM_8.5, "temp", .id = "Parcela") %>% 
    bind_cols(map_df(CCLM_8.5, "prec")) %>%
    setNames(., c("Parcela", names)) %>%
    mutate(Climate_Model = "CCLM",
           RCP = "RCP 8.5",
           Timestep = rep(seq(0,99), 261))

rca4_45 <- map_df(RCA4_4.5, "temp", .id = "Parcela") %>% 
    bind_cols(map_df(RCA4_4.5, "prec")) %>%
    setNames(., c("Parcela", names)) %>%
    mutate(Climate_Model = "RCA4",
           RCP = "RCP 4.5",
           Timestep = rep(seq(0,99), 261))

rca4_85 <- map_df(RCA4_8.5, "temp", .id = "Parcela") %>% 
    bind_cols(map_df(RCA4_8.5, "prec")) %>%
    setNames(., c("Parcela", names)) %>%
    mutate(Climate_Model = "RCA4",
           RCP = "RCP 8.5",
           Timestep = rep(seq(0,99), 261))

annual_climate_new <- bind_rows(cclm_45, cclm_85, rca4_45, rca4_85) %>%
    mutate(Scen_Clima = paste(Climate_Model, RCP, sep = "_"),
           Year = Timestep + 2001,
           Forest_Type = if_else(Parcela %in% names(pn_forestlist), "Pinus nigra",
                                 if_else(Parcela %in% names(ps_forestlist), "Pinus sylvestris",
                                         if_else(Parcela %in% names(mx_forestlist), "Mixed forests", "")))) %>%
    mutate(temp = (tm01 + tm02 + tm03 + tm04 + tm05 + tm06 + tm07 + tm08 + tm09 + tm10 + tm11 + tm12)/12,
           prec = pr01 + pr02 +pr03 + pr04 + pr05 + pr06 + pr07 + pr08 + pr09 + pr10 + pr11 + pr12)


save(annual_climate_new, file= "./data/annual_climate_new.Rdata")

foo <- left_join(annual_climate, annual_climate_new, by =  c("Parcela", "Forest_Type",  "Year", "Climate_Model", "RCP", "Timestep")) %>%
    filter(Climate_Model == "CCLM")

temp_plot <- ggplot(foo,aes(temp.x, temp.y, color = Forest_Type)) +
    geom_point(alpha = 0.2, size = 0.5) +
    geom_smooth(method = "lm", size = 2) +
    geom_abline(slope = 1) +
    xlab("Old temp") + ylab("New temp") +
    xlim(5,20) + ylim(5,20)

prec_plot <- ggplot(foo,aes(prec.x, prec.y, color = Forest_Type)) +
    geom_point(alpha = 0.2, size = 0.5) +
    geom_smooth(method = "lm", size = 2) + 
    geom_abline(slope = 1) +
    xlab("Old prec") + ylab("New prec.") +
    xlim(200,1800) + ylim(200,1800)


temp_plot + prec_plot +  plot_layout(guides = "collect") & theme(legend.position = 'bottom')

foo %>%
    dplyr::filter(Climate_Model == "CCLM") %>%
    ggplot() +
    geom_line(aes(x=Timestep, y=temp.x), color = "red") +
    geom_line(aes(x=Timestep, y=temp.y), color = "steelblue") +
    facet_trelliscope(Parcela ~ RCP , nrow = 2, ncol = 2, width = 300, 
                      path = here("reports/temp_diffs_per_plot/"))


foo %>%
    dplyr::filter(Climate_Model == "CCLM") %>%
    ggplot() +
    geom_line(aes(x=Timestep, y=prec.x), color = "red") +
    geom_line(aes(x=Timestep, y=prec.y), color = "steelblue") +
    facet_trelliscope(Parcela ~ RCP , nrow = 2, ncol = 2, width = 300, 
                      path = here("reports/precdiffs_per_plot/"))



