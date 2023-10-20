
# LE to FH2O ---> MOVE THIS TO flow.formatConcentrationDiffs.R
timeBgn <- as.POSIXct(strptime(m30.list$F_LE$timeBgn,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
timeEnd <- as.POSIXct(strptime(m30.list$F_LE$timeEnd,format='%Y-%m-%dT%H:%M:%OSZ',tz='GMT'))
flux <- m30.list$F_LE$turb
qf <- m30.list$F_LE$turb.qfFinl # filter
flux[qf == 1] <- NA
m9Diff.list <- lapply(m9Diff.list,FUN=function(var){
  timePred <- var$timeMid
  fluxPred <- interp_fluxes(timeBgn,timeEnd,flux,timePred)
  var$LE_interp <- fluxPred
  var$lambda <- (2.501-(2.361*1e-3)*Tair)*1e6 # lambda = J kg-1
  var$FH2O_interp <- fluxPred*(1/var$lambda) 
  return(var)
})

# Do the unit conversions from H to w'T' and LE to w'q'
# Compute & apply necessary corrections to fluxes

data.Tv=Tk.*(1+ma/mv*e_pa./P_pa) # virtual temperature
data.rho_mol=P_pa./(R*Tk) # molar air density  [mol/m3]

Lvap = (2.501-0.00237*data.TA)*1e6 # J/kg latent heat of vaporiz. - Eqn in back of Stull pg. 641                                 
mu = ma/mv
sigma = data.rhov./data.rhoa
#rhoa_mol = data.rhoa/ma
#data.rhoa - dry air density in kg m-3
#data.rhov - water vapor density in kg m-3
#data.rho - moist air density in kg m-3
#evaluate rho, VPD
P_pa = `PA_kPa`*1000  #Atmospheric pressure [Pa]
ma = 28.964/1000    #molar mass of dry air [Kg/mol]
mv = 18/1000        #molar mass of water vapor [Kg/mol]
R = 8.314          #Universal gas constant dry air [J/(K mol)]
Cpa_dry = 1004.67  #J Kg-1 K-1 - specific heat of dry air

tempK = `TAIR` + 273.16
esatPa = 611.2*exp(17.67*(Tk-273.16)/(Tk-29.65)) #[Pa] saturated water vapor pressure
e_Pa = `RH`*esat/100 #[Pa] vapor water pressure
VPD = (esat-e_pa)/1000
rhoa_kgm3 = ma*(P_pa - e_pa)/(R*Tk) # dry air density  [Kg/m3]
rhov_kgm3 = mv*e_pa/(R*Tk) # water vapor density  [Kg/m3]
rho_kgm3 = rhoa + rhov # moist air density  [Kg/m3]
specificHumidity_pct = rhov/rho # Specific humidity
Cp_moist = Cpa*(1+0.84*qs) # J kg-1 K-1 - Specific heat of moist air

H = Cp_moist*rho_kgm3*data.wt #W/m2 - Sensible heat flux (Eqn. 40 in WPL, 1980)
FH2O = H / (Cp_moist*rho_kgm3) 

# Filter for values between 4-3
CO2 = m9Diff.list[["CO2"]][m9Diff.list[["CO2"]]$dLevelsAminusB=="4_3",]
H2O = m9Diff.list[["H2O"]][m9Diff.list[["H2O"]]$dLevelsAminusB=="4_3",]
CH4 = m9Diff.list[["CH4"]][m9Diff.list[["CH4"]]$dLevelsAminusB=="4_3",]

# Combine data frames for two scalars
scalar_combine = dplyr::full_join(CO2, CH4, by="match_time")
scalar_combine = dplyr::filter(scalar_combine, !is.na(dConc.x) & !is.na(dConc.y))

scalar_combine$ch4flux = scalar_combine$FC_interp.x*(scalar_combine$dConc.y/scalar_combine$dConc.x)
scalar_combine$FC_interp.x <- scalar_combine$FC_interp.x/1000 #DELETE ME
plot <- plotly::plot_ly(data=scalar_combine, x=~match_time, y=~ch4flux,  type='scatter', mode='lines') %>%
  plotly::layout(margin = list(b = 50, t = 50, r=50),
                 title = 'CH4 Flux',
                 xaxis = list(title = base::paste0(c(rep("\n&nbsp;", 3),
                                                     rep("&nbsp;", 20),
                                                     paste0("Date-time"),
                                                     rep("&nbsp;", 20)),
                                                   collapse = ""),
                              nticks=6,
                              #range = c(1,48),
                              zeroline=TRUE
                 ),
                 yaxis = list(title = ''),
                 showlegend=TRUE) %>% 
  plotly::add_trace(data=scalar_combine,x=~match_time, y=~FC_interp.x,mode='lines',name='CO2 flux')


print(plot)