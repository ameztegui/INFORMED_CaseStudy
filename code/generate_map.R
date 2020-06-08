library(tidyverse)
library(sf)
library(raster)
library(rasterVis)
library(ggspatial)
library(ggnewscale)

load("./data/pn.rdata")
load("./data/ps.rdata")
load("./data/mx.rdata")

pn_sf <- st_as_sf(pn_sp_wgs84) %>% mutate(Sp = "Pinus nigra")
ps_sf <- st_as_sf(ps_sp_wgs84) %>% mutate(Sp = "Pinus sylvestris" )
mx_sf <- st_as_sf(mx_sp_wgs84) %>% mutate(Sp = "Mixed pine forests")
other_sf <- st_as_sf(mx_sp_wgs84) %>% mutate(Sp = "Other forests") %>% slice(1)

plots <- rbind(pn_sf, ps_sf,mx_sf, other_sf) %>%
    mutate(Sp = factor(Sp, levels = c("Pinus nigra", "Pinus sylvestris", "Mixed pine forests", "Other forests")))


mdt <- raster("./data/carto/SOL_MDT.tif")
slope <- terrain(mdt, opt='slope')
aspect <- terrain(mdt, opt='aspect')
hill <- hillShade(slope, aspect, angle=40, direction=270)

pn_mfe <- read_sf("data/carto/MFE50_solsones.shp") %>%
    filter(SP1 == 25, O1 >= 7) %>% mutate(Sp = "Pinus nigra")
ps_mfe <- read_sf("data/carto/MFE50_solsones.shp") %>%
    filter(SP1 == 21, O1 >= 7)%>% mutate(Sp = "Pinus sylvestris")
mx_mfe <- read_sf("data/carto/MFE50_solsones.shp") %>%
    filter(SP1 %in% c(21, 25), SP2 %in% c(21,25), O1 + O2 > 5)%>% mutate(Sp = "Mixed pine forests")
other_mfe <- read_sf("data/carto/MFE50_solsones.shp") %>%
    filter(SP1 != 21, SP1 != 25, SP2 != 21, SP2 != 25,
           DEFINICION == "Bosque") %>% mutate(Sp = "Other forests")

mfe <- rbind(pn_mfe, ps_mfe,mx_mfe, other_mfe) %>%
    mutate(Sp = factor(Sp, levels = c("Pinus nigra", "Pinus sylvestris", "Mixed pine forests", "Other forests")))

mdt_df <- raster::as.data.frame(mdt, xy = TRUE) %>% filter(!is.na(SOL_MDT))
hill_df <- raster::as.data.frame(hill, xy = TRUE) %>% filter(!is.na(layer))

#	Create vectors for colour breaks
b.hs <- seq(min(hill_df$layer),max(hill_df$layer), length.out=100)
b.dem <- seq(min(mdt_df$SOL_MDT),max(mdt_df$SOL_MDT), length.out=10)

species_colors <- c( "#E69F00", "#A14063", "#0072B2","navajowhite4")
species_symbols <- c( 5,2,4, 1)

pdf("figs/mapa_sps2.pdf")
ggplot() +
    geom_raster(data = hill_df, aes(x=x, y = y, fill = layer)) +
    scale_fill_gradientn(colours=grey(1:100/100),breaks=b.hs,guide="none") +
    new_scale_fill() +
    geom_sf(data = mfe, aes(fill = Sp), color = NA, alpha = 0.5) +
    geom_sf(data = plots, aes(shape = Sp, size = Sp)) +
    labs(x = "", y = "", fill = "", shape = "", size = "")+
    scale_fill_manual(values = species_colors) +
    scale_shape_manual(values = species_symbols) +
    scale_size_manual(values = c(2,2,2,0)) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank()
          ) +
    annotation_scale(location = "bl", width_hint = 0.25) +         #scale
    annotation_north_arrow(location = "bl", which_north = "true", #north
                           pad_x = unit(0.25, "in"), 
                           pad_y = unit(0.25, "in"),
                           style = north_arrow_fancy_orienteering)
 dev.off()
 
 