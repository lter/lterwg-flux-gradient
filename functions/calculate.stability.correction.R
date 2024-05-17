#' calculate.stability.correction
#'
#' @param gas dataframe of gas taken from min9
#'
#' @return dataframe with stability parameter cols
#' 
#'
#' @author Alexis Helgeson, Samuel Jurado, Roisin Commane, and Camilo Rey-Sanchez
calculate.stability.correction <- function(gas){
  #remove NAs from data columns used in calculation for AE this includes: P_kPa, Tair1, H_turb_interp, LE_turb_interp, ustar_interp, z_displ_calc
  #select for data columns -> remember there are as many ubar cols as there are TowerPositions for a given site
  data.cols <- c("P_kPa", "Tair1", "H_turb_interp", "LE_turb_interp", "ustar_interp")
  #remove NAs
  gas <- gas[complete.cases(gas[,data.cols]),]
  #calculate obukov length
  #MO.vars <- MOlength(press = gas$P_kPa, temp = gas$Tair1, H = gas$H_turb_interp, LE = gas$LE_turb_interp, velofric = gas$ustar_interp)
  MO.vars <- MOlength(press = gas$P_kPa, temp = gas$Tair1, H = gas$H_turb_interp, LE = gas$LE_turb_interp, velofric = gas$ustar_interp)
  #add OB params to data frame for eddy diffusivty calculation
  #gas <- cbind(gas, MO.vars$rho, MO.vars$vpotflux, MO.vars$L)
  gas <- data.frame(gas, rho = MO.vars$rho, vpotflux = MO.vars$vpotflux, L = MO.vars$L)
  # #rename columns
  # old.names <- grep("MO", names(gas))
  # names(gas)[old.names[1]] <- "rho"
  # names(gas)[old.names[2]] <- "vpotflux"
  # names(gas)[old.names[3]] <- "L"
  
  #grab canopy height
  #want to calculate stability param with respect to tower heights A & B
  #want to use heights where min(TowerHeight_A, TowerHeight_B) > d
  #add canopy.flag when min(TowerHeight_A, TowerHeight_B) < d, 0 = bad 1 = good
  gas$MO.param <- "hold"
  gas$phih <- "hold"
  gas$phim <- "hold"
  gas$canopy.flag <- "hold"
  for(k in 1:nrow(gas)){
    #pull out row for looping
    row.loop <- gas[k,]
    #set z as mean of tower positions A & B
    z = mean(c(as.numeric(row.loop$TowerHeight_A), as.numeric(row.loop$TowerHeight_B)))
    #set d as canopy displacement height
    d = as.numeric(row.loop$z_displ_calc)
    #calculate obukhov parameter
    gas[k,"MO.param"] <- as.numeric((z - d)/as.numeric(row.loop$L))
    mo.param <- as.numeric(gas[k,"MO.param"])
    #add canopy.flag
    min.height <- as.numeric(min(row.loop$TowerHeight_A, row.loop$TowerHeight_B))
    if(min.height < d){
      gas[k,"canopy.flag"] <- "0"
    }else{
      gas[k,"canopy.flag"] <- "1"
    }
    #calculate stability correction for each row
    if(mo.param > 0){
      zt=0
      phim=1+5*mo.param #Eq 4.37 Lee. Stability coefficient for wind shear
      phih=1+5*mo.param #Eq 4.37 Lee. Stability coefficient for eddy diffusivity of any scalar(k). Assumed to equal to diffusivity for heat (van Ulden,1978)
      psim = -5 * mo.param
      psih = psim
      gas[k,"phih"] <- as.numeric(phih)
      gas[k,"phim"] <- as.numeric(phim)
    }else{
      zt = (1-16*mo.param)^0.25
      phim=(1-16*mo.param)^(-0.25)  #Eq 4.35 Lee. Stability correction function for wind shear
      phih=(1-16*mo.param)^(-0.5) #Eq 4.36 Lee. Stability correction function for heat or any scalar(k). Assumed to equal to diffusivity for heat (van Ulden,1978)
      psim = log(((1 + zt^2)/2)*((1 + zt)/2)^2) - 2*atan(zt) + pi/2
      psih = 2*log((1+zt^2)/2)
      gas[k,"phih"] <- as.numeric(phih)
      gas[k,"phim"] <- as.numeric(phim)
    }
  }
  #set columns as numeric
  gas$MO.param <- as.numeric(gas$MO.param)
  gas$phih <- as.numeric(gas$phih)
  gas$phim <- as.numeric(gas$phim)
  
  return(gas)
  
}