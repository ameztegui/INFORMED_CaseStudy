library(readr)
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)


load("./data/pn.rdata")
load("./data/ps.rdata")
load("./data/mx.rdata")

all_plots <- list.files("Y:/INFORMED/SORTIE_files/Results/TreeMaps/", pattern = "*.txt", full.names = T ) # stored in the lab's NAS

import_data_sortie <- function (sortie_path_files) {
    sortie_files <- basename(sortie_path_files)
    
    file <- read_delim(sortie_path_files, delim = "\t", skip = 1, na = "--",
                       col_types = cols(
                           dead = col_character(),
                           Diam10 = col_double(),
                           DBH = col_double(),
                           SnagDecayClass = col_character()))
        file$Parcela <- str_sub(sortie_files,10,15)
        file$Climate_Model <- str_sub(sortie_files,17,20)
        file$RCP <- str_sub(sortie_files,22,23)
        file$Management <- str_sub(sortie_files,25,27)
        file$Repetition <- as.numeric(gsub("\\D+", "",  str_sub(sortie_files,29,30)))
        file$Timestep <-  as.numeric(gsub("\\D+", "", str_sub(sortie_files,-12,-10)))

        file %>%
            filter(Type != "Seedling", dead != "2") %>%
            mutate(Year = Timestep + 2001,
                   Forest_Type = if_else(Parcela %in% names(pn_forestlist), "Pinus nigra",
                                         if_else(Parcela %in% names(ps_forestlist), "Pinus sylvestris",
                                                 if_else(Parcela %in% names(mx_forestlist), "Mixed forest", ""))),
                   Management = if_else(Management %in% c("scA", "scD"), "CAR",
                                         if_else(Management %in% c("scB", "scE"), "BIO",
                                                 if_else(Management %in% c("scC", "scF"), "ADA",
                                                         "BAU"))),
                   Narrative = paste(Management, RCP, sep="_"),
                   Recipe = paste(Climate_Model, RCP, Management, sep = "_"),
                   CD = cut(DBH, c(0,2.5, 7.5, 12.5, 17.5, 22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 
                                   52.5, 57.5, 62.5, 67.5, 200),
                            labels = c("CD0", "CD5", "CD10", "CD15", "CD20", "CD25", "CD30", "CD35", "CD40", "CD45",
                                       "CD50", "CD55", "CD60", "CD65", "CD70")),
                   BA = (pi/4)*(DBH/100)^2) %>%
            group_by(Parcela, Forest_Type, Climate_Model, RCP, Management, Narrative, Recipe, Timestep, Species, CD, Type, dead, `Dead Code`, SnagDecayClass) %>%
            summarise(N = n(),
                      DBH = mean(DBH, na.rm=T),
                      Height = mean(Height, na.rm = T),
                      BA= sum(BA)) %>%
            arrange(Parcela, Timestep)
        
        
}

raw_data_CD <-  suppressWarnings(map_df(all_plots, import_data_sortie))



 save(raw_data_CD, file = "./data/raw_data_CD.Rdata")
 
