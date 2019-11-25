
load("./Rdata/erosion_A.Rdata")
load("./Rdata/erosion_B/erosion_B_CC_45_BAU.Rdata")
load("./Rdata/erosion_B/erosion_B_CC_45_scA.Rdata")
load("./Rdata/erosion_B/erosion_B_CC_45_scB.Rdata")
load("./Rdata/erosion_B/erosion_B_CC_45_scC.Rdata")

ER_CCLM_45_BAU = as.vector(as.matrix(A_cclm_45 - B_CC_45_BAU))
ER_CCLM_45_scA = as.vector(as.matrix(A_cclm_45 - B_CC_45_scA))
ER_CCLM_45_scB = as.vector(as.matrix(A_cclm_45 - B_CC_45_scB))
ER_CCLM_45_scC = as.vector(as.matrix(A_cclm_45 - B_CC_45_scC))
ERrel_CCLM_45_scA = (ER_CCLM_45_scA - ER_CCLM_45_BAU)
ERrel_CCLM_45_scB = (ER_CCLM_45_scB - ER_CCLM_45_BAU)
ERrel_CCLM_45_scC = (ER_CCLM_45_scC - ER_CCLM_45_BAU)

load("./Rdata/erosion_B/erosion_B_CC_85_BAU.Rdata")
load("./Rdata/erosion_B/erosion_B_CC_85_scD.Rdata")
load("./Rdata/erosion_B/erosion_B_CC_85_scE.Rdata")
load("./Rdata/erosion_B/erosion_B_CC_85_scF.Rdata")
ER_CCLM_85_BAU = as.vector(as.matrix(A_cclm_85 - B_CC_85_BAU))
ER_CCLM_85_scD = as.vector(as.matrix(A_cclm_85 - B_CC_85_scD))
ER_CCLM_85_scE = as.vector(as.matrix(A_cclm_85 - B_CC_85_scE))
ER_CCLM_85_scF = as.vector(as.matrix(A_cclm_85 - B_CC_85_scF))
ERrel_CCLM_85_scD = (ER_CCLM_85_scD - ER_CCLM_85_BAU)
ERrel_CCLM_85_scE = (ER_CCLM_85_scE - ER_CCLM_85_BAU)
ERrel_CCLM_85_scF = (ER_CCLM_85_scF - ER_CCLM_85_BAU)

load("./Rdata/erosion_B/erosion_B_RC_45_BAU.Rdata")
load("./Rdata/erosion_B/erosion_B_RC_45_scA.Rdata")
load("./Rdata/erosion_B/erosion_B_RC_45_scB.Rdata")
load("./Rdata/erosion_B/erosion_B_RC_45_scC.Rdata")
ER_RCA4_45_BAU = as.vector(as.matrix(A_rca4_45 - B_RC_45_BAU))
ER_RCA4_45_scA = as.vector(as.matrix(A_rca4_45 - B_RC_45_scA))
ER_RCA4_45_scB = as.vector(as.matrix(A_rca4_45 - B_RC_45_scB))
ER_RCA4_45_scC = as.vector(as.matrix(A_rca4_45 - B_RC_45_scC))
ERrel_RCA4_45_scA = (ER_RCA4_45_scA - ER_RCA4_45_BAU)
ERrel_RCA4_45_scB = (ER_RCA4_45_scB - ER_RCA4_45_BAU)
ERrel_RCA4_45_scC = (ER_RCA4_45_scC - ER_RCA4_45_BAU)

load("./Rdata/erosion_B/erosion_B_RC_85_BAU.Rdata")
load("./Rdata/erosion_B/erosion_B_RC_85_scD.Rdata")
load("./Rdata/erosion_B/erosion_B_RC_85_scE.Rdata")
load("./Rdata/erosion_B/erosion_B_RC_85_scF.Rdata")
ER_RCA4_85_BAU = as.vector(as.matrix(A_rca4_85 - B_RC_85_BAU))
ER_RCA4_85_scD = as.vector(as.matrix(A_rca4_85 - B_RC_85_scD))
ER_RCA4_85_scE = as.vector(as.matrix(A_rca4_85 - B_RC_85_scE))
ER_RCA4_85_scF = as.vector(as.matrix(A_rca4_85 - B_RC_85_scF))
ERrel_RCA4_85_scD = (ER_RCA4_85_scD - ER_RCA4_85_BAU)
ERrel_RCA4_85_scE = (ER_RCA4_85_scE - ER_RCA4_85_BAU)
ERrel_RCA4_85_scF = (ER_RCA4_85_scF - ER_RCA4_85_BAU)


ER = c(ER_CCLM_45_BAU, ER_CCLM_45_scA, ER_CCLM_45_scB, ER_CCLM_45_scC,
       ER_CCLM_85_BAU, ER_CCLM_85_scD, ER_CCLM_85_scE, ER_CCLM_85_scF,
       ER_RCA4_45_BAU, ER_RCA4_45_scA, ER_RCA4_45_scB, ER_RCA4_45_scC,
       ER_RCA4_85_BAU, ER_RCA4_85_scD, ER_RCA4_85_scE, ER_RCA4_85_scF)

ERbau = c(rep(NA, 26100), ERrel_CCLM_45_scA, ERrel_CCLM_45_scB, ERrel_CCLM_45_scC,
          rep(NA, 26100), ERrel_CCLM_85_scD, ERrel_CCLM_85_scE, ERrel_CCLM_85_scF,
          rep(NA, 26100), ERrel_RCA4_45_scA, ERrel_RCA4_45_scB, ERrel_RCA4_45_scC,
          rep(NA, 26100), ERrel_RCA4_85_scD, ERrel_RCA4_85_scE, ERrel_RCA4_85_scF)

er_plot = data.frame(Parcela = parcela, Recipe = recipe, RCP = RCP, Climate_Model = Climate_Model, 
                     Narrative = narrative, Management = management, Forest_Type = for_all, 
                     Year = as.numeric(as.character(yall)), ER = ER, ERbau = ERbau) %>% 
    mutate(Narrative = fct_relevel(Narrative, levels = c("BAU", "BIO","CAR","ADA")),
           Timestep = Year - 2001)


er_species <- er_plot %>%
    group_by (Recipe, RCP,Climate_Model, Management, Narrative, Year)  %>%
    summarize_at(vars(ER:ERbau), c(mean="mean", sd = "sd"), na.rm = T) %>%
    ungroup()
