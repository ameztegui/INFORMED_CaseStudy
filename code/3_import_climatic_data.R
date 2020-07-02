
rm(list=ls())

library(stringr)

# CCLM --------------------------------------------------------------------


temp_CCLM_4.5_files <- list.files(path="./SORTIE_files/Cimatic_Data/MeanTemperatureSortieIFN/CCLM4-8-17/rcp4.5/", full.names = T)
prec_CCLM_4.5_files <- list.files(path="./SORTIE_files/Cimatic_Data/PrecipitationSortieIFN/CCLM4-8-17/rcp4.5/", full.names = T)

temp_CCLM_4.5 =  map(temp_CCLM_4.5_files, read.table,header=TRUE,se="\t")
prec_CCLM_4.5 = map(prec_CCLM_4.5_files, read.table,header=TRUE,se="\t")
names(temp_CCLM_4.5) = str_sub(temp_CCLM_4.5_files,start=-10,-5)
names(prec_CCLM_4.5) = str_sub(prec_CCLM_4.5_files,start=-10,-5)

CCLM_4.5 <- mapply(list, temp=temp_CCLM_4.5, prec=prec_CCLM_4.5, SIMPLIFY = FALSE)
      

temp_CCLM_8.5_files <- list.files(path="./SORTIE_files/Cimatic_Data/MeanTemperatureSortieIFN/CCLM4-8-17/rcp8.5/", full.names = T)
prec_CCLM_8.5_files <- list.files(path="./SORTIE_files/Cimatic_Data/PrecipitationSortieIFN/CCLM4-8-17/rcp8.5/", full.names = T)

temp_CCLM_8.5 =  map(temp_CCLM_8.5_files, read.table,header=TRUE,se="\t")
prec_CCLM_8.5 = map(prec_CCLM_8.5_files, read.table,header=TRUE,se="\t")
names(temp_CCLM_8.5) = str_sub(temp_CCLM_8.5_files,start=-10,-5)
names(prec_CCLM_8.5) = str_sub(prec_CCLM_8.5_files,start=-10,-5)

CCLM_8.5 <- mapply(list, temp=temp_CCLM_8.5, prec=prec_CCLM_8.5, SIMPLIFY = FALSE)



      
# RCA4 --------------------------------------------------------------------

temp_RCA4_4.5_files <- list.files(path="./SORTIE_files/Cimatic_Data/MeanTemperatureSortieIFN/RCA4/rcp4.5/", full.names = T)
prec_RCA4_4.5_files <- list.files(path="./SORTIE_files/Cimatic_Data/PrecipitationSortieIFN/RCA4/rcp4.5/", full.names = T)

temp_RCA4_4.5 = map(temp_RCA4_4.5_files, read.table,header=TRUE,se="\t")
prec_RCA4_4.5 = map(prec_RCA4_4.5_files, read.table,header=TRUE,se="\t")
names(temp_RCA4_4.5) = str_sub(temp_RCA4_4.5_files,start=-10,-5)
names(prec_RCA4_4.5) = str_sub(prec_RCA4_4.5_files,start=-10,-5)

RCA4_4.5 <- mapply(list, temp=temp_RCA4_4.5, prec=prec_RCA4_4.5, SIMPLIFY = FALSE)


temp_RCA4_8.5_files <- list.files(path="./SORTIE_files/Cimatic_Data/MeanTemperatureSortieIFN/RCA4/rcp8.5/", full.names = T)
prec_RCA4_8.5_files <- list.files(path="./SORTIE_files/Cimatic_Data/PrecipitationSortieIFN/RCA4/rcp8.5/", full.names = T)

temp_RCA4_8.5 = map(temp_RCA4_8.5_files, read.table,header=TRUE,se="\t")
prec_RCA4_8.5 = map(prec_RCA4_8.5_files, read.table,header=TRUE,se="\t")
names(temp_RCA4_8.5) = str_sub(temp_RCA4_8.5_files,start=-10,-5)
names(prec_RCA4_8.5) = str_sub(prec_RCA4_8.5_files,start=-10,-5)

RCA4_8.5 <- mapply(list, temp=temp_RCA4_8.5, prec=prec_RCA4_8.5, SIMPLIFY = FALSE)


save (CCLM_4.5,CCLM_8.5,
      RCA4_4.5, RCA4_8.5,
      file="./data/climatic_data.Rdata")

