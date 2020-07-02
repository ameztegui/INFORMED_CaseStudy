library(tidyverse)
library(trelliscopejs)

load("./data/data_plot.Rdata")
load("./data/data_plot_species.Rdata")

# Per plot ----------------------------------------------------

data_plot %>%
    ggplot(aes(x=Timestep, y=BA)) +
    geom_line(aes(color = RCP)) +
    facet_trelliscope(Parcela ~ Management , nrow = 2, ncol = 4, width = 300, 
                        path = here("reports/BA_per_plot"))

data_plot %>%
    ggplot(aes(x=Timestep, y=N)) +
    geom_line(aes(color = RCP)) +
    facet_trelliscope(Parcela ~ Management , nrow = 2, ncol = 4, width = 300, 
                      path = here("/reports/N_per_plot"))

data_plot %>%
    ggplot(aes(x=Timestep, y=DBH)) +
    geom_line(aes(color = RCP)) +
    facet_trelliscope(Parcela ~ Management , nrow = 2, ncol = 4, width = 300, 
                      path = here("reports/DBH_per_plot/"))



# Per plot and species ----------------------------------------------------

data_plot_species %>%
    ggplot(aes(x=Timestep, y=BA)) +
    geom_line(aes(color = Species)) +
    facet_trelliscope(Parcela ~ Management + RCP , nrow = 2, ncol = 4, width = 300, 
                      path = here("reports/BA_per_plotspecies"))

data_plot_species %>%
    ggplot(aes(x=Timestep, y=N)) +
    geom_line(aes(color = Species)) +
    facet_trelliscope(Parcela ~ Management + RCP , nrow = 2, ncol = 4, width = 300, 
                      path = here("reports/N_per_plotspecies"))

data_plot_species %>%
    ggplot(aes(x=Timestep, y=DBH)) +
    geom_line(aes(color = Species)) +
    facet_trelliscope(Parcela ~ Management + RCP , nrow = 2, ncol = 4, width = 300, 
                      path = here("reports/DBH_per_plotspecies/"))
