library(dplyr)
library(forcats)
library(tidyr)

load("./data/ES_data/MushroomYieldPred5_withnewdataandmixedmodels.RData")

# Generate dasometry ------------------------------------------------------

daso <- bind_rows(mushrooms_CCLM_45, mushrooms_CCLM_85, mushrooms_RCA4_45, mushrooms_RCA4_85) %>%
    mutate(DBH = if_else(N != 0, sqrt((4*BA)/(N*3.141512))*100, 0)) %>%
    rename(Climate_Model = Scenario,
           RCP = Climate,
           Scenario = Management,
           Management = Narrative) %>%
    mutate(Year = Timestep + 2001,
           Forest_Type = fct_recode(Forest_Type,
                                    "Pinus nigra" = "PN",
                                    "Pinus sylvestris" = "PS",
                                    "Mixed forests" = "MX"),
           RCP = fct_recode(RCP,
                            "RCP 4.5" = "45",
                            "RCP 8.5" = "85")) 


save(daso, file = "./data/ZZ_structure_data_from_mushrroms.Rdata")

# Generate climatic data ------------------------------------------------------

annual_climate <- daso %>%
    rowwise() %>%
    mutate(prec = sum(c(pr01, pr02, pr03, pr04, pr05, pr06,
                        pr07, pr08, pr09, pr10, pr11,pr12)),
           temp = mean(c(tm01, tm02, tm03, tm04, tm05, tm06,
                         tm07, tm08, tm09, tm10, tm11, tm12))) %>%
    select(Parcela, Forest_Type, Scen_Clima, Climate_Model, RCP, Timestep, Year, tm01:tm12, temp, pr01:pr12, prec) %>%
    distinct()

save(annual_climate, file = "./data/annual_climate.Rdata")

