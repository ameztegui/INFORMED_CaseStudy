rm(list=ls())

library (stringr)
library(tidyverse)
library(purrr)
library(xml2)

source("./R scripts/02_EntryPoint.R")

# Initial densities -------------------------------------------------------

# This function takes the diameter distribution of each plot and parses it into a text string, so that it can be 
# inserted into the xml file
# The arguments are a dataframe, and the name of the species

define_densities <- function (df, species)  {
      
      opening <- paste0('tr_initialDensities><tr_idVals whatSpecies="',
                        species,'">')
      
      dens0 <- c("<tr_initialDensity sizeClass=\"s7.5\">0</tr_initialDensity>",
            '<tr_initialDensity sizeClass="s12.5">0</tr_initialDensity>',
            '<tr_initialDensity sizeClass="s17.5">0</tr_initialDensity>',
            '<tr_initialDensity sizeClass="s22.5">0</tr_initialDensity>',
            '<tr_initialDensity sizeClass="s27.5">0</tr_initialDensity>',
            '<tr_initialDensity sizeClass="s32.5">0</tr_initialDensity>',
            '<tr_initialDensity sizeClass="s37.5">0</tr_initialDensity>',
            '<tr_initialDensity sizeClass="s42.5">0</tr_initialDensity>',
            '<tr_initialDensity sizeClass="s47.5">0</tr_initialDensity>',
            '<tr_initialDensity sizeClass="s52.5">0</tr_initialDensity>')
            
      closing <- '</tr_idVals></tr_initialDensities>'
      
      dens_numbers <- as.numeric(df[1,])
      dens_string <- str_replace_all(dens0,'0',dens_numbers)
      dens_char <- paste(dens_string, sep="", collapse="") 
      complete_char <- paste0(opening, dens_char, closing,collapse ="" )
}

pn_xml_dd_A <- map(pn_sortie_A, define_densities, "PINI")
pn_xml_dd_B <- map(pn_sortie_B, define_densities, "PINI")
pn_xml_dd_C <- map(pn_sortie_C, define_densities, "PINI")
pn_xml_dd_D <- map(pn_sortie_D, define_densities, "PINI")

ps_xml_dd_A <- map(ps_sortie_A, define_densities, "PISY")
ps_xml_dd_B <- map(ps_sortie_B, define_densities, "PISY")
ps_xml_dd_C <- map(ps_sortie_C, define_densities, "PISY")
ps_xml_dd_D <- map(ps_sortie_D, define_densities, "PISY")

densities_A <- c(pn_xml_dd_A, ps_xml_dd_A)
densities_A <- densities_A[order(names(densities_A))]

densities_B <- c(pn_xml_dd_B, ps_xml_dd_B)
densities_B <- densities_B[order(names(densities_B))]

densities_C <- c(pn_xml_dd_C, ps_xml_dd_C)
densities_C <- densities_C[order(names(densities_C))]

densities_D <- c(pn_xml_dd_D, ps_xml_dd_D)
densities_D <- densities_D[order(names(densities_D))]


# Latitude
pn_sp_latitude <- as.list(as.character(pn_sp_longlat@coords[,2]))
names(pn_sp_latitude) <- names(pn_dd)
ps_sp_latitude <- as.list(as.character(ps_sp_longlat@coords[,2]))
names(ps_sp_latitude) <- names(ps_dd)

latitude <- c(pn_sp_latitude, ps_sp_latitude )
latitude <- latitude[order(names(latitude))]


# Harvest Regimes ---------------------------------------------------------

# In this case, we read the harvest regimes from external files, and we store them as objects

pn_harvest_A <- list.files(path="./SORTIE_files/Harvest_Regimes/",  pattern="pn_A",full.names = T) %>%
      set_names(.,basename(.)) %>%  map(read_xml)

pn_harvest_B <- list.files(path="./SORTIE_files/Harvest_Regimes/",  pattern="pn_B",full.names = T) %>%
      set_names(.,basename(.)) %>%  map(read_xml)

pn_harvest_C <- list.files(path="./SORTIE_files/Harvest_Regimes/",  pattern="pn_C",full.names = T) %>%
      set_names(.,basename(.)) %>%  map(read_xml)

pn_harvest_D <- list.files(path="./SORTIE_files/Harvest_Regimes/",  pattern="pn_D",full.names = T) %>%
      set_names(.,basename(.)) %>%  map(read_xml)

ps_harvest_A <- list.files(path="./SORTIE_files/Harvest_Regimes/",  pattern="ps_A",full.names = T) %>%
      set_names(.,basename(.)) %>%  map(read_xml)

ps_harvest_B <- list.files(path="./SORTIE_files/Harvest_Regimes/",  pattern="ps_B",full.names = T) %>%
      set_names(.,basename(.)) %>%  map(read_xml)

ps_harvest_C <- list.files(path="./SORTIE_files/Harvest_Regimes/",  pattern="ps_C",full.names = T) %>%
      set_names(.,basename(.)) %>%  map(read_xml)

ps_harvest_D <- list.files(path="./SORTIE_files/Harvest_Regimes/",  pattern="ps_D",full.names = T) %>%
      set_names(.,basename(.)) %>%  map(read_xml)


# We now create a function to define which entry point corresponds to each plot, and to prepare the file
# in a format that is insertable in the xml file
# The arguments are the ifn dataframe, the model dataframe and the harvest regime

harvest_regimes <- function (ifn, model, harvest) {
      distance <- vegdiststruct(model, ifn, method="manhattan")
      entrypoint <- apply(distance, 2, which.min)
      harvest_reg <- harvest[entrypoint]
      names(harvest_reg) <- names(ifn)
      harvest_reg
      
      # setNames(object = dd,nm = names(ifn) )
}

pn_harvest_A <- harvest_regimes(pn_dd, pn_modelA$dd, pn_harvest_A) 
pn_harvest_B <- harvest_regimes(pn_dd, pn_modelB$dd, pn_harvest_B)
pn_harvest_C <- harvest_regimes(pn_dd, pn_modelC$dd, pn_harvest_C)
pn_harvest_D <- harvest_regimes(pn_dd, pn_modelD$dd, pn_harvest_D)

ps_harvest_A <- harvest_regimes(ps_dd, ps_modelA$dd, ps_harvest_A)
ps_harvest_B <- harvest_regimes(ps_dd, ps_modelB$dd, ps_harvest_B)
ps_harvest_C <- harvest_regimes(ps_dd, ps_modelC$dd, ps_harvest_C)
ps_harvest_D <- harvest_regimes(ps_dd, ps_modelD$dd, ps_harvest_D)

harvest_A <- c(pn_harvest_A, ps_harvest_A )
harvest_A <- harvest_A[order(names(harvest_A))]

harvest_B <- c(pn_harvest_B, ps_harvest_B )
harvest_B <- harvest_B[order(names(harvest_B))]

harvest_C <- c(pn_harvest_C, ps_harvest_C )
harvest_C <- harvest_C[order(names(harvest_C))]

harvest_D <- c(pn_harvest_D, ps_harvest_D )
harvest_D <- harvest_D[order(names(harvest_D))]

# Climate files -----------------------------------------------------------

load("./Rdata/climatic_data.Rdata")

climate_scenarios <- function(df) {
      
      intro <- paste0("ClimateImporter2>
            <sc_ciJanRad>0.0</sc_ciJanRad>
            <sc_ciFebRad>0.0</sc_ciFebRad>
            <sc_ciMarRad>0.0</sc_ciMarRad>
            <sc_ciAprRad>0.0</sc_ciAprRad>
            <sc_ciMayRad>0.0</sc_ciMayRad>
            <sc_ciJunRad>0.0</sc_ciJunRad>
            <sc_ciJulRad>0.0</sc_ciJulRad>
            <sc_ciAugRad>0.0</sc_ciAugRad>
            <sc_ciSepRad>0.0</sc_ciSepRad>
            <sc_ciOctRad>0.0</sc_ciOctRad>
            <sc_ciNovRad>0.0</sc_ciNovRad>
            <sc_ciDecRad>0.0</sc_ciDecRad>
            <sc_ciAWS>0.0</sc_ciAWS>")
            
      ### Temperature #####
      
      temp_jan <- list()
            for (year in 1:100) {
           temp_jan[[year]]<- paste0(" <sc_cimtjanVal ts=\"",year,"\">",
                   df$temp[year,1],"</sc_cimtjanVal>") }
      
      temp_feb <- list()
      for (year in 1:100) {
            temp_feb[[year]]<- paste0(" <sc_cimtfebVal ts=\"",year,"\">",
                                      df$temp[year,2],"</sc_cimtfebVal>") }
      
      temp_mar <- list()
      for (year in 1:100) {
            temp_mar[[year]]<- paste0(" <sc_cimtmarVal ts=\"",year,"\">",
                                      df$temp[year,3],"</sc_cimtmarVal>") }
      
      temp_apr <- list()
      for (year in 1:100) {
            temp_apr[[year]]<- paste0(" <sc_cimtaprVal ts=\"",year,"\">",
                                      df$temp[year,4],"</sc_cimtaprVal>") }
      
      temp_may <- list()
      for (year in 1:100) {
            temp_may[[year]]<- paste0(" <sc_cimtmayVal ts=\"",year,"\">",
                                      df$temp[year,5],"</sc_cimtmayVal>") }
      
      temp_jun <- list()
      for (year in 1:100) {
            temp_jun[[year]]<- paste0(" <sc_cimtjunVal ts=\"",year,"\">",
                                      df$temp[year,6],"</sc_cimtjunVal>") }
      
      temp_jul <- list()
      for (year in 1:100) {
            temp_jul[[year]]<- paste0(" <sc_cimtjulVal ts=\"",year,"\">",
                                      df$temp[year,7],"</sc_cimtjulVal>") }
      
      temp_aug <- list()
      for (year in 1:100) {
            temp_aug[[year]]<- paste0(" <sc_cimtaugVal ts=\"",year,"\">",
                                      df$temp[year,8],"</sc_cimtaugVal>") }
      
      temp_sep <- list()
      for (year in 1:100) {
            temp_sep[[year]]<- paste0(" <sc_cimtsepVal ts=\"",year,"\">",
                                      df$temp[year,9],"</sc_cimtsepVal>") }
      
      temp_oct <- list()
      for (year in 1:100) {
            temp_oct[[year]]<- paste0(" <sc_cimtoctVal ts=\"",year,"\">",
                                      df$temp[year,10],"</sc_cimtoctVal>") }
      
      temp_nov <- list()
      for (year in 1:100) {
            temp_nov[[year]]<- paste0(" <sc_cimtnovVal ts=\"",year,"\">",
                                      df$temp[year,11],"</sc_cimtnovVal>") }
      
      temp_dec <- list()
      for (year in 1:100) {
            temp_dec[[year]]<- paste0(" <sc_cimtdecVal ts=\"",year,"\">",
                                      df$temp[year,12], "</sc_cimtdecVal>")
      }
      
      ##### ERRROR, crea uno para cada vez, por ser lista!!!!
      temp <- paste0("<sc_ciMonthlyTempJan>", paste(temp_jan,collapse=""),"</sc_ciMonthlyTempJan>",
                     "<sc_ciMonthlyTempFeb>", paste(temp_feb,collapse=""),"</sc_ciMonthlyTempFeb>",
                     "<sc_ciMonthlyTempMar>", paste(temp_mar,collapse=""),"</sc_ciMonthlyTempMar>",
                     "<sc_ciMonthlyTempApr>", paste(temp_apr,collapse=""),"</sc_ciMonthlyTempApr>",
                     "<sc_ciMonthlyTempMay>", paste(temp_may,collapse=""),"</sc_ciMonthlyTempMay>",
                     "<sc_ciMonthlyTempJun>", paste(temp_jun,collapse=""),"</sc_ciMonthlyTempJun>",
                     "<sc_ciMonthlyTempJul>", paste(temp_jul,collapse=""),"</sc_ciMonthlyTempJul>",
                     "<sc_ciMonthlyTempAug>", paste(temp_aug,collapse=""),"</sc_ciMonthlyTempAug>",
                     "<sc_ciMonthlyTempSep>", paste(temp_sep,collapse=""),"</sc_ciMonthlyTempSep>",
                     "<sc_ciMonthlyTempOct>", paste(temp_oct,collapse=""),"</sc_ciMonthlyTempOct>",
                     "<sc_ciMonthlyTempNov>", paste(temp_nov,collapse=""),"</sc_ciMonthlyTempNov>",
                     "<sc_ciMonthlyTempDec>", paste(temp_dec,collapse=""),"</sc_ciMonthlyTempDec>")

      ## Precipitation #####

      prec_jan <- list()
      for (year in 1:100) {
            prec_jan[[year]]<- paste0(" <sc_cimpjanVal ts=\"",year,"\">",
                                      df$prec[year,1],"</sc_cimpjanVal>") }
      
      prec_feb <- list()
      for (year in 1:100) {
            prec_feb[[year]]<- paste0(" <sc_cimpfebVal ts=\"",year,"\">",
                                      df$prec[year,2],"</sc_cimpfebVal>") }
      
      prec_mar <- list()
      for (year in 1:100) {
            prec_mar[[year]]<- paste0(" <sc_cimpmarVal ts=\"",year,"\">",
                                      df$prec[year,3],"</sc_cimpmarVal>") }
      
      prec_apr <- list()
      for (year in 1:100) {
            prec_apr[[year]]<- paste0(" <sc_cimpaprVal ts=\"",year,"\">",
                                      df$prec[year,4],"</sc_cimpaprVal>") }
      
      prec_may <- list()
      for (year in 1:100) {
            prec_may[[year]]<- paste0(" <sc_cimpmayVal ts=\"",year,"\">",
                                      df$prec[year,5],"</sc_cimpmayVal>") }
      
      prec_jun <- list()
      for (year in 1:100) {
            prec_jun[[year]]<- paste0(" <sc_cimpjunVal ts=\"",year,"\">",
                                      df$prec[year,6],"</sc_cimpjunVal>") }
      
      prec_jul <- list()
      for (year in 1:100) {
            prec_jul[[year]]<- paste0(" <sc_cimpjulVal ts=\"",year,"\">",
                                      df$prec[year,7],"</sc_cimpjulVal>") }
      
      prec_aug <- list()
      for (year in 1:100) {
            prec_aug[[year]]<- paste0(" <sc_cimpaugVal ts=\"",year,"\">",
                                      df$prec[year,8],"</sc_cimpaugVal>") }
      
      prec_sep <- list()
      for (year in 1:100) {
            prec_sep[[year]]<- paste0(" <sc_cimpsepVal ts=\"",year,"\">",
                                      df$prec[year,9],"</sc_cimpsepVal>") }
      
      prec_oct <- list()
      for (year in 1:100) {
            prec_oct[[year]]<- paste0(" <sc_cimpoctVal ts=\"",year,"\">",
                                      df$prec[year,10],"</sc_cimpoctVal>") }
      
      prec_nov <- list()
      for (year in 1:100) {
            prec_nov[[year]]<- paste0(" <sc_cimpnovVal ts=\"",year,"\">",
                                      df$prec[year,11],"</sc_cimpnovVal>") }
      
      prec_dec <- list()
      for (year in 1:100) {
            prec_dec[[year]]<- paste0(" <sc_cimpdecVal ts=\"",year,"\">",
                                      df$prec[year,12], "</sc_cimpdecVal>")
      }
      
      prec <- paste0("<sc_ciMonthlyPptJan>", paste(prec_jan,collapse=""),"</sc_ciMonthlyPptJan>",
                     "<sc_ciMonthlyPptFeb>", paste(prec_feb,collapse=""),"</sc_ciMonthlyPptFeb>",
                     "<sc_ciMonthlyPptMar>", paste(prec_mar,collapse=""),"</sc_ciMonthlyPptMar>",
                     "<sc_ciMonthlyPptApr>", paste(prec_apr,collapse=""),"</sc_ciMonthlyPptApr>",
                     "<sc_ciMonthlyPptMay>", paste(prec_may,collapse=""),"</sc_ciMonthlyPptMay>",
                     "<sc_ciMonthlyPptJun>", paste(prec_jun,collapse=""),"</sc_ciMonthlyPptJun>",
                     "<sc_ciMonthlyPptJul>", paste(prec_jul,collapse=""),"</sc_ciMonthlyPptJul>",
                     "<sc_ciMonthlyPptAug>", paste(prec_aug,collapse=""),"</sc_ciMonthlyPptAug>",
                     "<sc_ciMonthlyPptSep>", paste(prec_sep,collapse=""),"</sc_ciMonthlyPptSep>",
                     "<sc_ciMonthlyPptOct>", paste(prec_oct,collapse=""),"</sc_ciMonthlyPptOct>",
                     "<sc_ciMonthlyPptNov>", paste(prec_nov,collapse=""),"</sc_ciMonthlyPptNov>",
                     "<sc_ciMonthlyPptDec>", paste(prec_dec,collapse=""),"</sc_ciMonthlyPptDec>")
      
      
      close <- "</ClimateImporter2>"

      climate <- paste0(intro, temp, prec, close, collapse = "")
      
}  

CCLM_4.5 <- map(CCLM_4.5, climate_scenarios)
CCLM_8.5 <- map(CCLM_8.5, climate_scenarios)
RCA4_4.5 <- map(RCA4_4.5, climate_scenarios)
RCA4_8.5 <- map(RCA4_8.5, climate_scenarios)

CCLM_4.5 <- CCLM_4.5[names(densities_A)]
CCLM_8.5 <- CCLM_8.5[names(densities_A)]
RCA4_4.5 <- RCA4_4.5[names(densities_A)]
RCA4_8.5 <- RCA4_8.5[names(densities_A)]


# Insert xml into SORTIE parameter files ----------------------------------

narrative_A <- mapply(list, densities = densities_A, latitude = latitude, harvest = harvest_A, SIMPLIFY = FALSE)
narrative_B <- mapply(list, densities = densities_B, latitude = latitude, harvest = harvest_B, SIMPLIFY = FALSE)
narrative_C <- mapply(list, densities = densities_C, latitude = latitude, harvest = harvest_C, SIMPLIFY = FALSE)
narrative_D <- mapply(list, densities = densities_D, latitude = latitude, harvest = harvest_D, SIMPLIFY = FALSE)



parse_xml <- function (narrative, narrative_name, climate, climate_name, names) {
            
      densities <- narrative$densities
      plot_ID <- paste0(names,"_", climate_name,"_",narrative_name)
      latitude <- narrative$latitude
      harvest <- narrative$harvest
      
      
      y <- read_xml("./SORTIE_files/Informed_piloto.xml")
      
      
      ## Name a series of nodes that we want to substitute
      
            ## Plot Definition
            sortie_plot <- xml_children(y)[[1]]
                  sortie_plot_title <-  xml_children(sortie_plot)[13][[1]]
                  sortie_plot_latitude <-  xml_children(sortie_plot)[7][[1]]
            
            ## Trees
            sortie_trees <- xml_children(y)[[2]]
                  sortie_speciesList <- xml_children(sortie_trees)[[1]]
                  sortie_sizeClasses <- xml_children(sortie_trees)[[2]]
                  sortie_initialDensities <- xml_children(sortie_trees)[[3]]
            
            ## Harvest
            sortie_harvest <- xml_children(y)[[8]]
            
            ## Climate Importer
            sortie_climate <-xml_children(y)[[9]]
            
            ## Output
            sortie_output <- xml_children(y)[[21]]
                  sortie_output_name <- xml_children(sortie_output)[[1]]
                  
            sortie_short_output <- xml_children(y)[[22]]
                  sortie_short_output_name <- xml_children(sortie_short_output)[[1]]
      
      ## Replace each node in the xml file by the object created previously
      xml_text(sortie_plot_title) <- plot_ID
      xml_text(sortie_plot_latitude) <- latitude

      sortie_initialDensities <- xml_replace(sortie_initialDensities, densities)
      sortie_harvest <- xml_replace(sortie_harvest,harvest)
      sortie_climate <- xml_replace(sortie_climate,climate )

      xml_text(sortie_output_name) <- paste0("D:\\Activity\\INFORMED_CaseStudy\\SORTIE_files\\Results\\",
                                      plot_ID, ".gz.tar")

      xml_text(sortie_short_output_name) <- paste0("D:\\Activity\\INFORMED_CaseStudy\\SORTIE_files\\Results\\",
                                                       plot_ID, ".out",collapse ="")
      y
}

      #--------------------
      # Scenario CCLM_4.5
      #--------------------

      # Narrative A
      xml_A <- pmap(list(narrative_A, "A", CCLM_4.5, "CCLM_4.5",names(narrative_A)), parse_xml)
      names(xml_A) <- names(latitude)
      
      map(seq_along(xml_A), function(i){
            write_xml(xml_A[[i]], 
                      file=paste0("./SORTIE_files/INFORMED_", names(xml_A)[[i]],"_CCLM_4.5_A.xml")) })
      
      # Narrative B
      xml_B <- pmap(list(narrative_B, "B", CCLM_4.5, "CCLM_4.5",names(narrative_B)), parse_xml)
      names(xml_B) <- names(latitude)
      
      map(seq_along(xml_B), function(i){
            write_xml(xml_B[[i]], 
                      file=paste0("./SORTIE_files/INFORMED_", names(xml_B)[[i]],"_CCLM_4.5_B.xml")) })
      
      # Narrative C
      xml_C <- pmap(list(narrative_C, "C", CCLM_4.5, "CCLM_4.5",names(narrative_C)), parse_xml)
      names(xml_C) <- names(latitude)
      
      map(seq_along(xml_C), function(i){
            write_xml(xml_C[[i]], 
                      file=paste0("./SORTIE_files/INFORMED_", names(xml_C)[[i]],"_CCLM_4.5_C.xml")) })
      
      # Narrative D
      xml_D <- pmap(list(narrative_D, "D", CCLM_4.5, "CCLM_4.5",names(narrative_D)), parse_xml)
      names(xml_D) <- names(latitude)
      
      map(seq_along(xml_D), function(i){
            write_xml(xml_D[[i]], 
                      file=paste0("./SORTIE_files/INFORMED_", names(xml_D)[[i]],"_CCLM_4.5_D.xml")) })
      

      #--------------------
      # Scenario CCLM_8.5
      #--------------------
      
      # Narrative A
      xml_A <- pmap(list(narrative_A, "A", CCLM_8.5, "CCLM_8.5",names(narrative_A)), parse_xml)
      names(xml_A) <- names(latitude)
      
      map(seq_along(xml_A), function(i){
            write_xml(xml_A[[i]], 
                      file=paste0("./SORTIE_files/INFORMED_", names(xml_A)[[i]],"_CCLM_8.5_A.xml")) })
      
      # Narrative B
      xml_B <- pmap(list(narrative_B, "B", CCLM_8.5, "CCLM_8.5",names(narrative_B)), parse_xml)
      names(xml_B) <- names(latitude)
      
      map(seq_along(xml_B), function(i){
            write_xml(xml_B[[i]], 
                      file=paste0("./SORTIE_files/INFORMED_", names(xml_B)[[i]],"_CCLM_8.5_B.xml")) })
      
      # Narrative C
      xml_C <- pmap(list(narrative_C, "C", CCLM_8.5, "CCLM_8.5",names(narrative_C)), parse_xml)
      names(xml_C) <- names(latitude)
      
      map(seq_along(xml_C), function(i){
            write_xml(xml_C[[i]], 
                      file=paste0("./SORTIE_files/INFORMED_", names(xml_C)[[i]],"_CCLM_8.5_C.xml")) })
      
      # Narrative D
      xml_D <- pmap(list(narrative_D, "D", CCLM_8.5, "CCLM_8.5",names(narrative_D)), parse_xml)
      names(xml_D) <- names(latitude)
      
      map(seq_along(xml_D), function(i){
            write_xml(xml_D[[i]], 
                      file=paste0("./SORTIE_files/INFORMED_", names(xml_D)[[i]],"_CCLM_8.5_D.xml")) })
      
      
      #--------------------
      # Scenario RCA4_4.5
      #--------------------
      
      # Narrative A
      xml_A <- pmap(list(narrative_A, "A", RCA4_4.5, "RCA4_4.5",names(narrative_A)), parse_xml)
      names(xml_A) <- names(latitude)
      
      map(seq_along(xml_A), function(i){
            write_xml(xml_A[[i]], 
                      file=paste0("./SORTIE_files/INFORMED_", names(xml_A)[[i]],"_RCA4_4.5_A.xml")) })
      
      # Narrative B
      xml_B <- pmap(list(narrative_B, "B", RCA4_4.5, "RCA4_4.5",names(narrative_B)), parse_xml)
      names(xml_B) <- names(latitude)
      
      map(seq_along(xml_B), function(i){
            write_xml(xml_B[[i]], 
                      file=paste0("./SORTIE_files/INFORMED_", names(xml_B)[[i]],"_RCA4_4.5_B.xml")) })
      
      # Narrative C
      xml_C <- pmap(list(narrative_C, "C", RCA4_4.5, "RCA4_4.5",names(narrative_C)), parse_xml)
      names(xml_C) <- names(latitude)
      
      map(seq_along(xml_C), function(i){
            write_xml(xml_C[[i]], 
                      file=paste0("./SORTIE_files/INFORMED_", names(xml_C)[[i]],"_RCA4_4.5_C.xml")) })
      
      # Narrative D
      xml_D <- pmap(list(narrative_D, "D", RCA4_4.5, "RCA4_4.5",names(narrative_D)), parse_xml)
      names(xml_D) <- names(latitude)
      
      map(seq_along(xml_D), function(i){
            write_xml(xml_D[[i]], 
                      file=paste0("./SORTIE_files/INFORMED_", names(xml_D)[[i]],"_RCA4_4.5_D.xml")) })
      
      
      #--------------------
      # Scenario RCA4_8.5
      #--------------------
      
      # Narrative A
      xml_A <- pmap(list(narrative_A, "A", RCA4_8.5, "RCA4_8.5",names(narrative_A)), parse_xml)
      names(xml_A) <- names(latitude)
      
      map(seq_along(xml_A), function(i){
            write_xml(xml_A[[i]], 
                      file=paste0("./SORTIE_files/INFORMED_", names(xml_A)[[i]],"_RCA4_8.5_A.xml")) })
      
      # Narrative B
      xml_B <- pmap(list(narrative_B, "B", RCA4_8.5, "RCA4_8.5",names(narrative_B)), parse_xml)
      names(xml_B) <- names(latitude)
      
      map(seq_along(xml_B), function(i){
            write_xml(xml_B[[i]], 
                      file=paste0("./SORTIE_files/INFORMED_", names(xml_B)[[i]],"_RCA4_8.5_B.xml")) })
      
      # Narrative C
      xml_C <- pmap(list(narrative_C, "C", RCA4_8.5, "RCA4_8.5",names(narrative_C)), parse_xml)
      names(xml_C) <- names(latitude)
      
      map(seq_along(xml_C), function(i){
            write_xml(xml_C[[i]], 
                      file=paste0("./SORTIE_files/INFORMED_", names(xml_C)[[i]],"_RCA4_8.5_C.xml")) })
      
      # Narrative D
      xml_D <- pmap(list(narrative_D, "D", RCA4_8.5, "RCA4_8.5",names(narrative_D)), parse_xml)
      names(xml_D) <- names(latitude)
      
      map(seq_along(xml_D), function(i){
            write_xml(xml_D[[i]], 
                      file=paste0("./SORTIE_files/INFORMED_", names(xml_D)[[i]],"_RCA4_8.5_D.xml")) })
      
      
     
