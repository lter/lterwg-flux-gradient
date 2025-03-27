
#' calculate.stability.correction
#'
#' @param gas dataframe of gas taken from min9
#'
#' @return dataframe with stability parameter cols
#' 
#'
#' @author Alexis Helgeson, Samuel Jurado, Roisin Commane, and Camilo Rey-Sanchez, Sparkle L. Malone

calculate.stability.correction <- function(gas){
  #remove NAs from data columns used in calculation for AE this includes: P_kPa, Tair1, H_turb_interp, LE_turb_interp, ustar_interp, z_displ_calc
  
  #select for data columns -> remember there are as many ubar cols as there are TowerPositions for a given site
  data.cols <- c("P_kPa", "Tair1", "H_turb_interp", "LE_turb_interp", "ustar_interp")
  
  # pull out the max level for Tair
  
  #remove NAs
  #gas <- gas[, data.cols]
  
  # Define top level where fluxes are measured
  maxL=max(min9Diff.list$H2O$TowerPosition_A)
  nextL = sort(unique(min9Diff.list$H2O$TowerPosition_A),
       decreasing = TRUE)[2]
  TopLevel = paste0(maxL,"_",nextL)
  
  #calculate obukov length (Obukhov length)
  
  # Detect the highest Tair
  Tair_at_TowerTop <- function(gas){
    #df <- data.frame(gas)
    #names(df) <- substring( names(df), 5)
    df <- gas
    levels <- df %>% select( starts_with( 'Tair')) %>% names %>% str_split_fixed( 'Tair',2) 
    max.levels <- levels[,2] %>% max 
    Tair <- paste('Tair',max.levels, sep="")
    return( Tair)
  }
  
  Tair <- Tair_at_TowerTop(gas)
  
  #gas <- gas[complete.cases(gas[,c(data.cols, Tair)]),]
  
  MO.vars <- MOlength(press = gas$P_kPa, temp = gas[,Tair], H = gas$H_turb_interp, LE = gas$LE_turb_interp, velofric = gas$ustar_interp)
  
  # add OB params to data frame for eddy diffusivty calculation
  #gas <- cbind(gas, MO.vars$rho, MO.vars$vpotflux, MO.vars$L)
  gas <- data.frame(gas, rho = MO.vars$rho, vpotflux = MO.vars$vpotflux, L = MO.vars$L)
  
  # Calculate Aerodynamic Canopy Height to later calculate d and zo
  
  daysAVG=20
  plotYN=0
  
  Mdate=gas$timeEnd_A[gas$dLevelsAminusB == TopLevel]
  ustar=gas$ustar_interp[gas$dLevelsAminusB == TopLevel]
  gas$mean_TowerH <- rowMeans((cbind(as.numeric(gas$TowerHeight_A), as.numeric(gas$TowerHeight_B))), na.rm = TRUE)
  z=mean(as.numeric((gas$mean_TowerH[gas$dLevelsAminusB == TopLevel])))
  L=gas$L[gas$dLevelsAminusB == TopLevel]
  if (maxL==8) {ubar=gas$ubar8}
  if (maxL==7) {ubar=gas$ubar7}
  if (maxL==6) {ubar=gas$ubar6}
  if (maxL==5) {ubar=gas$ubar5}
  if (maxL==4) {ubar=gas$ubar4}
  if (maxL==3) {ubar=gas$ubar3}
  u=ubar[gas$dLevelsAminusB == TopLevel]
  
  AeroVars=calc.AeroCanopyH(Mdate=Mdate, ustar=ustar, z=z, L=L, u=u, daysAVG=daysAVG, plotYN=plotYN)
  
  # Interpolate AeroCanopy Height to the other levels
  levels=unique(gas$dLevelsAminusB)
  for(i in 1:length(levels)){
    iLevel=levels[i]
    Mdate_new=gas$timeEnd_A[gas$dLevelsAminusB == iLevel]
    zh_interp <- approx(Mdate, AeroVars$zh_final, Mdate_new, method="linear", rule=2)$y
    gas$z_veg_aero[gas$dLevelsAminusB == iLevel] = zh_interp
  }
  
  #set d as canopy displacement height
  gas$z_displ_calc = gas$z_veg_aero*0.66
  gas$roughLength_calc = gas$z_veg_aero*0.1
  
  #set z as mean of tower positions A & B
  z = mean(c(as.numeric(gas$TowerHeight_A), as.numeric(gas$TowerHeight_B)))
  
  #calculate obukhov parameter
  gas$effective_h = z - as.numeric(gas$z_displ_calc)
  gas$effective_h[gas$effective_h<0.1] <- 0.1
  gas$MO.param = as.numeric((gas$effective_h)/as.numeric(gas$L))
  gas$MO.param <- as.numeric(gas$MO.param)
  
  #add canopy.flag
  min.height <- as.numeric(min(gas$TowerHeight_A, gas$TowerHeight_B))
  gas$canopy.flag = rep(NA,nrow(gas))
  gas$canopy.flag[min.height < gas$z_displ_calc] = 0
  gas$canopy.flag[min.height >= gas$z_displ_calc] =1
  
  # Calculate phih & phim
  gas$phih = rep(NA,nrow(gas))
  gas$phim = rep(NA,nrow(gas))
  
  #Eq 4.37 Lee. Stability coefficient for wind shear
  gas$phih[gas$MO.param>0&!is.na(gas$MO.param>0)] = 1+5*gas$MO.param[gas$MO.param>0&!is.na(gas$MO.param>0)] 
  gas$phim[gas$MO.param>0&!is.na(gas$MO.param>0)] = 1+5*gas$MO.param[gas$MO.param>0&!is.na(gas$MO.param>0)] 
  
  #Eq 4.35 Lee. Stability correction function for wind shear
  gas$phim[gas$MO.param<=0&!is.na(gas$MO.param>0)]=(1-16*mo.param[gas$MO.param<=0&!is.na(gas$MO.param>0)])^(-0.25)  
  gas$phih[gas$MO.param<=0&!is.na(gas$MO.param>0)]=(1-16*mo.param[gas$MO.param<=0&!is.na(gas$MO.param>0)])^(-0.5) 
  
  
  # #grab canopy height
  # #want to calculate stability param with respect to tower heights A & B
  # #want to use heights where min(TowerHeight_A, TowerHeight_B) > d
  # #add canopy.flag when min(TowerHeight_A, TowerHeight_B) < d, 0 = bad 1 = good
  # gas$MO.param <- "hold"
  # gas$phih <- "hold"
  # gas$phim <- "hold"
  # gas$canopy.flag <- "hold"
  # 
  # for(k in 1:nrow(gas)){
  #   #pull out row for looping
  #   row.loop <- gas[k,]
  #   #set z as mean of tower positions A & B
  #   z = mean(c(as.numeric(row.loop$TowerHeight_A), as.numeric(row.loop$TowerHeight_B)))
  #   #Set d
  #   d= as.numeric(row.loop$z_displ_calc)
  #   #calculate obukhov parameter
  #   effect_h= z - d
  #   effect_h[effect_h<0.1] <- 0.1
  #   gas[k,"effective_h"]<-effect_h
  #   gas[k,"MO.param"] <- as.numeric((effect_h)/as.numeric(row.loop$L))
  #   mo.param <- as.numeric(gas[k,"MO.param"])
  #   #add canopy.flag
  #   min.height <- as.numeric(min(row.loop$TowerHeight_A, row.loop$TowerHeight_B))
  #   if(min.height < d){
  #     gas[k,"canopy.flag"] <- "0"
  #   }else{
  #     gas[k,"canopy.flag"] <- "1"
  #   }
  #   #calculate stability correction for each row
  #   if(!is.na(mo.param)){
  #     if(mo.param > 0){
  #       zt=0
  #       phim=1+5*mo.param #Eq 4.37 Lee. Stability coefficient for wind shear
  #       phih=1+5*mo.param #Eq 4.37 Lee. Stability coefficient for eddy diffusivity of any scalar(k). Assumed to equal to diffusivity for heat (van Ulden,1978)
  #       psim = -5 * mo.param
  #       psih = psim
  #       gas[k,"phih"] <- as.numeric(phih)
  #       gas[k,"phim"] <- as.numeric(phim)
  #     }else{
  #       zt = (1-16*mo.param)^0.25
  #       phim=(1-16*mo.param)^(-0.25)  #Eq 4.35 Lee. Stability correction function for wind shear
  #       phih=(1-16*mo.param)^(-0.5) #Eq 4.36 Lee. Stability correction function for heat or any scalar(k). Assumed to equal to diffusivity for heat (van Ulden,1978)
  #       psim = log(((1 + zt^2)/2)*((1 + zt)/2)^2) - 2*atan(zt) + pi/2
  #       psih = 2*log((1+zt^2)/2)
  #       gas[k,"phih"] <- as.numeric(phih)
  #       gas[k,"phim"] <- as.numeric(phim)
  #     }
  #   } else {
  #     gas[k,"phih"] <- NA
  #     gas[k,"phim"] <- NA
  #   }
  #   
  #   
  #   print(paste("Here:",k,"of",nrow(gas)))
  # }
  
  
  # if(mo.param > 0){
  #   zt=0
  #   phim=1+5*mo.param #Eq 4.37 Lee. Stability coefficient for wind shear
  #   phih=1+5*mo.param #Eq 4.37 Lee. Stability coefficient for eddy diffusivity of any scalar(k). Assumed to equal to diffusivity for heat (van Ulden,1978)
  #   psim = -5 * mo.param
  #   psih = psim
  #   gas[k,"phih"] <- as.numeric(phih)
  #   gas[k,"phim"] <- as.numeric(phim)
  # }else{
  #   zt = (1-16*mo.param)^0.25
  #   phim=(1-16*mo.param)^(-0.25)  #Eq 4.35 Lee. Stability correction function for wind shear
  #   phih=(1-16*mo.param)^(-0.5) #Eq 4.36 Lee. Stability correction function for heat or any scalar(k). Assumed to equal to diffusivity for heat (van Ulden,1978)
  #   psim = log(((1 + zt^2)/2)*((1 + zt)/2)^2) - 2*atan(zt) + pi/2
  #   psih = 2*log((1+zt^2)/2)
  #   gas[k,"phih"] <- as.numeric(phih)
  #   gas[k,"phim"] <- as.numeric(phim)
  # }
  # 
  # #set columns as numeric
  # gas$MO.param <- as.numeric(gas$MO.param)
  # gas$phih <- as.numeric(gas$phih)
  # gas$phim <- as.numeric(gas$phim)
  
  return(gas)
  
}
