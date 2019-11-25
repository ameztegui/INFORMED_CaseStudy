library(dplyr)
library(zoo)
library(ggplot2)
library(forcats)
library(patchwork)


narrative_colors <- c("#5F978E","#5B4887", "#9B2F5D", "#E7BC66" )
climate_colors <- c("#0072B2", "#D55E00")
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



load("./data/ZZ_structure_data_from_mushrooms.Rdata")

data <- daso 
data <- data_plot

filtered_data <- data %>%
    filter(Management != "ORG", Climate_Model == "CCLM") %>%
    group_by(Forest_Type, Recipe, Management, RCP, Scenario, Year) %>%
    summarise_at(vars (BA, N, DBH),
                .funs= list(~mean(.),~sd(.)))




BA_plot <- filtered_data %>%
    group_by(Forest_Type, Management, RCP, Year) %>%
    summarise (n=n(),
               BA = mean(BA_mean),
               sd = mean(BA_sd))%>%
    ggplot(aes(Year, y=zoo:::rollmean(BA, 4, na.pad=TRUE), color = Management)) +
    geom_ribbon(aes(ymin = zoo::rollmean(BA-sd,4,na.pad=T),
                    ymax = zoo::rollmean(BA+sd,4,na.pad=T),
                    fill = Management), alpha=0.2, color =NA) +
    labs(x = "Year", y = expression(bold(paste('Basal area ('~ m^2~ha^-1,')')))) +
    facet_grid(RCP ~ Forest_Type) +
    geom_line(size = 1.5) +
    xlim(2002, 2098) +
    scale_color_manual("Scenario",values = narrative_colors, labels = c("Business as usual", "Wood energy", "Carbon storage", "Adaptation")) +
    scale_fill_manual("Scenario", values = narrative_colors, labels = c("Business as usual", "Wood energy", "Carbon storage", "Adaptation")) +    
    newtheme 

N_plot <-  filtered_data %>%
    group_by(Forest_Type, Management, RCP, Year) %>%
    summarise (n=n(),
               N = mean(N_mean),
               sd= mean(N_sd)/sqrt(n())) %>%
    ggplot(aes(Year, zoo:::rollmean(N,4,na.pad=T), color = Management)) +
    geom_ribbon(aes(ymin = zoo::rollmean(N-sd,4,na.pad=T),
                    ymax = zoo::rollmean(N+sd, 4, na.pad=T),
                    fill = Management), alpha=0.2, color =NA) + 
    labs(x = "Year", y = "Stem density") +
    facet_grid(RCP ~ Forest_Type) +
    geom_line(size = 1.5) +
    xlim(2002, 2098) +
    scale_color_manual("Scenario",values = narrative_colors, labels = c("Business as usual", "Wood energy", "Carbon storage", "Adaptation")) +
    scale_fill_manual("Scenario", values = narrative_colors, labels = c("Business as usual", "Wood energy", "Carbon storage", "Adaptation")) +    
    newtheme


DBH_plot <-  filtered_data %>%
    group_by(Forest_Type, Management, RCP, Year) %>%
    summarise (n=n(),
               DBH = mean(DBH_mean),
               sd= mean(DBH_sd)) %>%
    ggplot(aes(Year, zoo:::rollmean(DBH,4,na.pad=T), color = Management)) +
    geom_ribbon(aes(ymin = zoo::rollmean(DBH-sd,4,na.pad=T),
                    ymax = zoo::rollmean(DBH+sd, 4, na.pad=T),
                    fill = Management), alpha=0.2, color =NA) + 
    labs(x = "Year", y = "Mean tree diameter (cm)") +
    facet_grid(RCP ~ Forest_Type) +
    geom_line(size = 1.5) +
    xlim(2002, 2098) +
    scale_y_continuous(breaks = seq(0, 50, by = 10)) +
    scale_color_manual("Scenario",values = narrative_colors, labels = c("Business as usual", "Wood energy", "Carbon storage", "Adaptation")) +
    scale_fill_manual("Scenario", values = narrative_colors, labels = c("Business as usual", "Wood energy", "Carbon storage", "Adaptation")) +    
    newtheme 


pdf("./figs/daso_3panels.pdf", width = 8, height = 14)
    BA_plot +  theme(legend.position = "n") +
    N_plot + theme(legend.position = "n") +
    DBH_plot + 
    plot_layout(ncol = 1) 
dev.off()

pdf("./figs/daso_2panels.pdf", width = 8, height = 11)
    N_plot + theme(legend.position = "n") +
    DBH_plot + 
    plot_layout(ncol = 1) 
dev.off()