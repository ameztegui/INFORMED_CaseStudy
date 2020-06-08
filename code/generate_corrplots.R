library(corrplot)
library(tidyverse)
library(RColorBrewer)

pale <- brewer.pal(10, name ="BrBG")
plot_corr <- function(data, color.threshold) {
    bigs <- which(data > color.threshold)
    cols <- matrix("black", nrow(data), ncol(data))
    cols[bigs] <- "white"
    colores <- cols[upper.tri(cols)]
    corrplot(data, method = "color", type = "upper", diag = F,
             addCoef.col = colores, col = pale,
             tl.pos ="n", cl.ratio = 0.3, cl.align = "r",
             tl.col = "black")
}


cor_BAU_45_pears <- as.matrix(read_csv("C:/Dades/Activity/INFORMED/data/spatial_correlation/BAU_45.csv") %>% column_to_rownames("X1") )
cor_BAU_85_pears <- as.matrix(read_csv("C:/Dades/Activity/INFORMED/data/spatial_correlation/BAU_85.csv") %>% column_to_rownames("X1") )
cor_CAR_45_pears <- as.matrix(read_csv("C:/Dades/Activity/INFORMED/data/spatial_correlation/CARBON_45.csv") %>% column_to_rownames("X1") )
cor_CAR_85_pears <- as.matrix(read_csv("C:/Dades/Activity/INFORMED/data/spatial_correlation/CARBON_85.csv") %>% column_to_rownames("X1") )
cor_BIO_45_pears <- as.matrix(read_csv("C:/Dades/Activity/INFORMED/data/spatial_correlation/WOODENERGY_45.csv") %>% column_to_rownames("X1") )
cor_BIO_85_pears <- as.matrix(read_csv("C:/Dades/Activity/INFORMED/data/spatial_correlation/WOODENERGY_85.csv") %>% column_to_rownames("X1") )
cor_ADA_45_pears <- as.matrix(read_csv("C:/Dades/Activity/INFORMED/data/spatial_correlation/VULNERABILITY_45.csv") %>% column_to_rownames("X1") )
cor_ADA_85_pears <- as.matrix(read_csv("C:/Dades/Activity/INFORMED/data/spatial_correlation/VULNERABILITY_85.csv") %>% column_to_rownames("X1") )

corrplot(cor_BAU_45_pears)

pdf("C:/Dades/Activity/INFORMED/figs/spatial_45.pdf")
par(mfcol=c(2,2))
par(mar=c(5.1,4.1,4.1,2.1))
plot_corr(cor_BAU_45_pears, 0.6)
plot_corr(cor_BIO_45_pears, 0.6)
plot_corr(cor_CAR_45_pears, 0.6)
plot_corr(cor_ADA_45_pears, 0.6)
dev.off()

pdf("C:/Dades/Activity/INFORMED/figs/spatial_85.pdf")
par(mfcol=c(2,2))
par(mar=c(5.1,4.1,4.1,2.1))
plot_corr(cor_BAU_85_pears, 0.6)
plot_corr(cor_BIO_85_pears, 0.6)
plot_corr(cor_CAR_85_pears, 0.6)
plot_corr(cor_ADA_85_pears, 0.6)
dev.off()
