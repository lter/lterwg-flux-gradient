library(dplyr)
source(file.path("R/MO_Length.R"))
source(file.path("R/eddydiffAE.R"))
source(file.path("R/eddydiffWP.R"))
source(file.path("R/FG_AE.WP.R"))
#add code to pull data off of google drive
#add code later that pulls zip files off of g drive
sitecode <- 'KONZ'
#load in interpolated 9 min data
load(file.path("data", sitecode, "KONZ_min9Diff.Rdata"))
load(file.path("data", sitecode, "KONZ_attr.Rdata"))
#desired concentration
#add in loop to include all gas concentrations?
cont.desired <- "H2O"
#call function to calculate eddy diffusivity using AE method
#add in calculation for all gas concentrations
min9EddyDiffAE.list <- eddydiffAE(cont.desired = cont.desired, sitecode = sitecode, min9 = min9Diff.list, attr = attr.df)
#call function to calculate eddy diffusivity using WP method
min9EddyDiffWP.list <- eddydiffWP(cont.desired = cont.desired, sitecode = sitecode, min9 = min9Diff.list, attr = attr.df)
#calculate fluxes using AE method
gas.conc.AE <- min9EddyDiffAE.list[[1]]
gas.conc.AE$dHeight <- as.numeric(gas.conc.AE$TowerHeight_A) - as.numeric(gas.conc.AE$TowerHeight_B)
min9FGAE.list <- FG_AE.WP(min9 = gas.conc.AE)
#calculate fluxes using WP method
gas.conc.WP <- min9EddyDiffWP.list[[1]]
gas.conc.WP$dHeight <- as.numeric(gas.conc.WP$TowerHeight_A) - as.numeric(gas.conc.WP$TowerHeight_B)
min9FGWP.list <- FG_AE.WP(min9 = gas.conc.WP)
#filter for top two tower positions
CO2.FG <- list()
CO2.AE <- min9FGAE.list$gas
CO2.WP <- min9FGWP.list$gas
CO2.AE <- CO2.AE %>% filter(TowerPosition_A == 4 & TowerPosition_B == 3)
CO2.WP <- CO2.WP %>% filter(TowerPosition_A == 4 & TowerPosition_B == 3)
CO2.FG <- list(AE = CO2.AE, WP = CO2.WP)

H2O.FG <- list()
H2O.AE <- min9FGAE.list$gas
H2O.WP <- min9FGWP.list$gas
H2O.AE <- H2O.AE %>% filter(TowerPosition_A == 4 & TowerPosition_B == 3)
H2O.WP <- H2O.WP %>% filter(TowerPosition_A == 4 & TowerPosition_B == 3)
H2O.FG <- list(AE = H2O.AE, WP = H2O.WP)

load(file.path("data", sitecode, "KONZ_AE_WP_Helgeson_1252023.Rdata"))
Flux.gradient.AE.WP <- list(CO2.AE = CO2.FG$AE, CO2.WP = CO2.FG$WP, H2O.AE = H2O.AE, H2O.WP = H2O.WP)
save(Flux.gradient.AE.WP, file = file.path("data", sitecode, "KONZ_AE_WP_Helgeson_1252023.Rdata"))
