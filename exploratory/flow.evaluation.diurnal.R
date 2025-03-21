
source('/Users/sm3466/YSE Dropbox/Sparkle Malone/Research/FluxGradient/lterwg-flux-gradient/exploratory/FUNCTION_DIURNAL.R' )

# Calculate Diurnal Patterns by Year-month:
Diurnal.MBR.CO2 <- DIURNAL.COMPILE.Sites( FG.tibble =  SITES_MBR_30min_FILTER_BH, 
                                          FG_flux = 'FG_mean', 
                                          EC_flux = 'FC_turb_interp', gas = "CO2")

Diurnal.WP.CO2 <- DIURNAL.COMPILE.Sites( FG.tibble =  SITES_WP_30min_FILTER_BH, 
                                         FG_flux = 'FG_mean', 
                                         EC_flux = 'FC_turb_interp', gas = "CO2")

Diurnal.AE.CO2 <- DIURNAL.COMPILE.Sites( FG.tibble =  SITES_AE_30min_FILTER_BH, 
                                         FG_flux = 'FG_mean', 
                                         EC_flux = 'FC_turb_interp', gas = "CO2")

diurnal.summary.CO2 <- Diurnal.Summary(diurnal.tibble = Diurnal.MBR.CO2, TYP='MBR' ) %>% rbind(Diurnal.Summary(diurnal.tibble = Diurnal.AE.CO2, TYP='AE' ) ) %>% rbind(Diurnal.Summary(diurnal.tibble = Diurnal.WP.CO2, TYP='WP' ) )  

# DIurnals for H2O

# add gas to the function!!!!
Diurnal.MBR.H2O <- DIURNAL.COMPILE.Sites( FG.tibble =  SITES_MBR_30min_FILTER_BH, 
                                          FG_flux = 'FG_mean', 
                                          EC_flux = 'FC_turb_interp', gas = "H2O")

Diurnal.WP.H2O <- DIURNAL.COMPILE.Sites( FG.tibble =  SITES_WP_30min_FILTER_BH, 
                                         FG_flux = 'FG_mean', 
                                         EC_flux = 'FC_turb_interp', gas = "H2O")

Diurnal.AE.H2O <- DIURNAL.COMPILE.Sites( FG.tibble =  SITES_AE_30min_FILTER_BH, 
                                         FG_flux = 'FG_mean', 
                                         EC_flux = 'FC_turb_interp', gas = "H2O")

diurnal.summary.H2O <- Diurnal.Summary(diurnal.tibble = Diurnal.MBR.H2O, TYP='MBR' ) %>% rbind(Diurnal.Summary(diurnal.tibble = Diurnal.AE.H2O, TYP='AE' ) ) %>% rbind(Diurnal.Summary(diurnal.tibble = Diurnal.WP.H2O, TYP='WP' ) )  

diurnal.summary.H2O$Type <- factor( diurnal.summary.H2O$Type, levels= c('MBR', 'AE', 'WP'))
