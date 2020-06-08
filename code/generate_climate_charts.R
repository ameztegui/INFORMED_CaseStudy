load("./data/annual_climate.Rdata")

library(tidyverse)
library(patchwork)
library(zoo)


management_colors <- c("#5F978E","#5B4887", "#9B2F5D", "#E7BC66" )
rcp_colors <- c("#0072B2", "#D55E00")
species_colors <- c("#0072B2","#E69F00","#A14063" )
okabe_ito <- c( "#D55E00", "#009E73","#E69F00", "#56B4E9", "#F0E442", "#0072B2", "#CC79A7","#999999")

newtheme <- theme_minimal() +
    theme(legend.position = "bottom",
          strip.text = element_text(size = 12, face = "italic"),
          axis.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 11),
          legend.title = element_text(size = 12, face = "bold"),
          panel.spacing.x = unit(5, "mm"),
          panel.grid.minor = element_blank())



# Differences between climatic models -------------------------------------


clima_mean <-annual_climate %>%
    group_by(Forest_Type, Scen_Clima,Climate_Model, RCP, Timestep) %>%
    summarise(Temp = mean (temp),
              sd_Temp = sd(temp),
              Prec = mean (prec),
              sd_Prec = sd(prec)) %>%
    ungroup()


mean_temp_plot <- clima_mean%>%
    ggplot() +
    geom_ribbon(aes(x= 2001+Timestep, fill= RCP,
                    ymin=Temp - sd_Temp, ymax = Temp + sd_Temp), 
                alpha = 0.7, color = NA) +
    geom_smooth(aes(x= 2001+Timestep,y = Temp, color = RCP), method = "loess", se = F) +
    facet_grid(~ Climate_Model) +
    labs(x = "Year", y = "Mean annual temperature (ºC)") +
    scale_fill_manual(values = rcp_colors)+
    scale_color_manual(values = rcp_colors)+
    newtheme


mean_prec_plot <- clima_mean%>%
    ggplot() +
    geom_ribbon(aes(x= 2001+Timestep,  fill= RCP,
                    ymin=Prec - sd_Prec, ymax = Prec + sd_Prec), 
                alpha = 0.7, color =NA)+
    geom_smooth(aes(x= 2001+Timestep,y = Prec, color = RCP), method = "loess", se = F) +
    facet_grid(  ~ Climate_Model)+
    labs(x = "Year", y = "Annual precipitation (mm)") +
    scale_fill_manual(values = rcp_colors)+
    scale_color_manual(values = rcp_colors)+
    newtheme

mean_temp_plot + theme(legend.position= "n") +
    mean_prec_plot + plot_layout(ncol = 1)
ggsave("./figs/clima_regional_model.jpg", height = 7)

# Differences between forest types and RCP scenarios -------------------------------------


mean_temp_plot <- clima_mean%>%
    ggplot() +
    geom_ribbon(aes(x= 2001+Timestep, fill= RCP,
                    ymin=Temp - sd_Temp, ymax = Temp + sd_Temp), 
                alpha = 0.7, color = NA)+
    geom_smooth(aes(x= 2001+Timestep,y = Temp, color = RCP), method = "loess", se = F) +
    facet_grid(~ Forest_Type)+
    labs(x = "Year", y = "Mean annual temperature (ºC)") +
    scale_fill_manual(values = rcp_colors)+
    scale_color_manual(values = rcp_colors)+
    newtheme


mean_prec_plot <- clima_mean%>%
    ggplot() +
    geom_ribbon(aes(x= 2001+Timestep,  fill= RCP,
                    ymin=Prec - sd_Prec, ymax = Prec + sd_Prec), 
                alpha = 0.7, color =NA)+
    
    geom_smooth(aes(x= 2001+Timestep,y = Prec, color = RCP), method = "loess", se = F) +
    facet_grid(  ~ Forest_Type)+
    labs(x = "Year", y = "Annual precipitation (mm)") +
    scale_fill_manual(values = rcp_colors)+
    scale_color_manual(values = rcp_colors)+
    newtheme

mean_temp_plot + theme(legend.position= "n") +
    mean_prec_plot + plot_layout(ncol = 1)
ggsave("./figs/clima_forest_rcp.jpg", height = 7)


