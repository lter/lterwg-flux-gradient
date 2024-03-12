#' ustar.threshold.interp
#'
#' @param site df containing site NEE and PAR data
#'
#' @return df with u star threshold column
#'
#'
#' @author Alexis Helgeson
ustar.threshold.interp <- function(site){
  #ustar for CO2; calculate ustar based on eddy-covariance NEE flux and apply to other fluxes
  #filter to nighttime NEE flux
  site.CO2 <- site %>% filter(gas == "CO2" & day_night == "night")
  #plot(site.CO2$ustar_interp, site.CO2$FC_nee_interp)
  #separate out data by month
  Jan <- site.CO2 %>% filter(month == 1)
  Feb <- site.CO2 %>% filter(month == 2)
  Mar <- site.CO2 %>% filter(month == 3)
  Apr <- site.CO2 %>% filter(month == 4)
  May <- site.CO2 %>% filter(month == 5)
  Jun <- site.CO2 %>% filter(month == 6)
  Jul <- site.CO2 %>% filter(month == 7)
  Aug <- site.CO2 %>% filter(month == 8)
  Sep <- site.CO2 %>% filter(month == 9)
  Oct <- site.CO2 %>% filter(month == 10)
  Nov <- site.CO2 %>% filter(month == 11)
  Dec <- site.CO2 %>% filter(month == 12)
  #define function to calculate ustar threshold
  threshold <- function(NEE){
    # ustar.0NEE <- NEE[which(NEE$FC_nee_interp >= -1 & NEE$FC_nee_interp <= 1), "ustar_interp"]
    # ut <- median(ustar.0NEE)
    #filter to only NEE and ustar data and remove NAs
    NEE <- NEE %>% select(FC_nee_interp, ustar_interp) %>% na.omit()
    #check to make sure there is data if not return NA
    if(dim(NEE)[[1]] == 0){
      return(ut=NA)
    }
    #sample data to make sure there is enough to fit curve
    y = sample(NEE$FC_nee_interp, size = 500, replace = T)
    x = sample(NEE$ustar_interp, size = 500, replace = T)
    #fit quadratic curve to nighttime NEE and ustar data
    fit <- nls(y ~ a * x^2 + b * x + c, start = list(a = -0.5, b = 0.42, c = 0.11), data = list(y = y, x = x))
    #set model equation using estimated parameters
    a <- coef(fit)[["a"]]
    b <- coef(fit)[["b"]]
    c <- coef(fit)[["c"]]
    fit.curve <- expression(-1.45 * x^2 + 1.20 * x + 0.08)
    #we went to find the ustar (x) value that corresponds to when the slope of the curve is zero, so we can take the 1st derivative of the model equation to get the slope
    fit.slope <- D(fit.curve,'x')
    #evaluate slope for given ustar values
    #x <- NEE$ustar_interp
    slope <- eval(fit.slope)
    # plot(slope)
    # abline(h = 0)
    #plot(x, slope)
    #find which ustar values correspond to 0 slope
    ustar.0slope <- x[which(slope >= 0)]
    #take the minimum of those ustar values which correspond to 0 slope
    ut <- round(median(ustar.0slope), 2)
    
    #Ideally want to use change point detection methodology but currently cannot get fcn to detect change
    #order slopes in descending order
    # slope.order <- slope[order(slope, decreasing = T)]
    # #resample slope values
    # slope.resample <- sample(slope.order, size = 10000, replace = TRUE)
    # #get first difference of slope values
    # diff.slope <- c(0,diff(slope.resample))
    # cpts(cpt.mean(slope.resample, method = "PELT"))
    # 
    # #predict NEE using curve
    # predict.NEE <- predict(fit, newdata = data.frame(x = NEE$ustar_interp))
    # #get first difference of NEE predictions
    # diff.predict.NEE <- c(0, diff(predict.NEE))
    # cpts(cpt.mean(diff.predict.NEE, method = "BinSeg"))
    
    return(ut)
  }
  
  #calculate threshold for each temporal group, only calculate threshold for months with NEE, function will return NA otherwise
  Jan.threshold <- threshold(NEE=Jan)
  Feb.threshold <- threshold(NEE=Feb)
  Mar.threshold <- threshold(NEE=Mar)
  Apr.threshold <- threshold(NEE=Apr)
  May.threshold <- threshold(NEE=May)
  Jun.threshold <- threshold(NEE=Jun)
  Jul.threshold <- threshold(NEE=Jul)
  Aug.threshold <- threshold(NEE=Aug)
  Sep.threshold <- threshold(NEE=Sep)
  Oct.threshold <- threshold(NEE=Oct)
  Nov.threshold <- threshold(NEE=Nov)
  Dec.threshold <- threshold(NEE=Dec)
  #All.threshold <- threshold(NEE=site.CO2)
  #add ustar threshold as column match which month the ustar threshold comes from in the dataframe
  site[which(site$month==1),"ustar.threshold"] <- Jan.threshold
  site[which(site$month==2),"ustar.threshold"] <- Feb.threshold
  site[which(site$month==3),"ustar.threshold"] <- Mar.threshold
  site[which(site$month==4),"ustar.threshold"] <- Apr.threshold
  site[which(site$month==5),"ustar.threshold"] <- May.threshold
  site[which(site$month==6),"ustar.threshold"] <- Jun.threshold
  site[which(site$month==7),"ustar.threshold"] <- Jul.threshold
  site[which(site$month==8),"ustar.threshold"] <- Aug.threshold
  site[which(site$month==9),"ustar.threshold"] <- Sep.threshold
  site[which(site$month==10),"ustar.threshold"] <- Oct.threshold
  site[which(site$month==11),"ustar.threshold"] <- Nov.threshold
  site[which(site$month==12),"ustar.threshold"] <- Dec.threshold
  #add ustar.flag based on ustar.threshold column 
  site[which(site$ustar_interp > site$ustar.threshold),"ustar.flag"] <- "0"
  site[which(site$ustar_interp <= site$ustar.threshold),"ustar.flag"] <- "1"
  #calculate how much data is left after ustar filtering
  percent.good.data <- round((length(site[which(site$ustar.threshold == "0"),"FG"])/length(site$FG))*100, 2)
  print(paste0("After applying ustar.flag there is ~", percent.good.data, "% good data remaining for all fluxes"))
  
  return(site)
  #DEPRECIATED CODE
  #save ustar threshold values in dataframe
  # ustar.threshold.df <- data.frame(site = unique(site.CO2$site),Jan = Jan.threshold, Feb = Feb.threshold, Mar = Mar.threshold, Apr = Apr.threshold, May = May.threshold, Jun = Jun.threshold, Jul = Jul.threshold, Aug = Aug.threshold, Sep = Sep.threshold, Oct = Oct.threshold, Nov = Nov.threshold, Dec = Dec.threshold, All = All.threshold)
  #DEPRECIATED CODE
  #WE ARE CALCULATING USTAR THRESHOLD USING NEE AND APPYING TO ALL OTHER FLUXES
  #add u star flag columns to site data frame: use NEON convention "0" good data "1" bad data
  # site[which(site$ustar_interp > Jan.threshold),"Jan_ustar_threshold"] <- "0"
  # site[which(site$ustar_interp <= Jan.threshold),"Jan_ustar_threshold"] <- "1"
  # site[which(site$ustar_interp > Feb.threshold),"Feb_ustar_threshold"] <- "0"
  # site[which(site$ustar_interp <= Feb.threshold),"Feb_ustar_threshold"] <- "1"
  # site[which(site$ustar_interp > Mar.threshold),"Mar_ustar_threshold"] <- "0"
  # site[which(site$ustar_interp <= Mar.threshold),"Mar_ustar_threshold"] <- "1"
  # site[which(site$ustar_interp > Apr.threshold),"Apr_ustar_threshold"] <- "0"
  # site[which(site$ustar_interp <= Apr.threshold),"Apr_ustar_threshold"] <- "1"
  # site[which(site$ustar_interp > May.threshold),"May_ustar_threshold"] <- "0"
  # site[which(site$ustar_interp <= May.threshold),"May_ustar_threshold"] <- "1"
  # site[which(site$ustar_interp > Jun.threshold),"Jun_ustar_threshold"] <- "0"
  # site[which(site$ustar_interp <= Jun.threshold),"Jun_ustar_threshold"] <- "1"
  # site[which(site$ustar_interp > Jul.threshold),"Jul_ustar_threshold"] <- "0"
  # site[which(site$ustar_interp <= Jul.threshold),"Jul_ustar_threshold"] <- "1"
  # site[which(site$ustar_interp > Aug.threshold),"Aug_ustar_threshold"] <- "0"
  # site[which(site$ustar_interp <= Aug.threshold),"Aug_ustar_threshold"] <- "1"
  # site[which(site$ustar_interp > Sep.threshold),"Sep_ustar_threshold"] <- "0"
  # site[which(site$ustar_interp <= Sep.threshold),"Sep_ustar_threshold"] <- "1"
  # site[which(site$ustar_interp > Oct.threshold),"Oct_ustar_threshold"] <- "0"
  # site[which(site$ustar_interp <= Oct.threshold),"Oct_ustar_threshold"] <- "1"
  # site[which(site$ustar_interp > Nov.threshold),"Nov_ustar_threshold"] <- "0"
  # site[which(site$ustar_interp <= Nov.threshold),"Nov_ustar_threshold"] <- "1"
  # site[which(site$ustar_interp > Dec.threshold),"Dec_ustar_threshold"] <- "0"
  # site[which(site$ustar_interp <= Dec.threshold),"Dec_ustar_threshold"] <- "1"
  # site[which(site$ustar_interp > All.threshold),"All_ustar_threshold"] <- "0"
  # site[which(site$ustar_interp <= All.threshold),"All_ustar_threshold"] <- "1"
  
  # site[which(site$ustar_interp > All.threshold),"ustar.flag"] <- "0"
  # site[which(site$ustar_interp <= All.threshold),"ustar.flag"] <- "1"
  
  #return(ustar.threshold.df)
  #DEPRECIATED CODE
  #find which ustar values corresponds with ~ 0 NEE; take median of ustar values
  #try to find threshold by using fit -> problem returns u star value < 0
  # Function to find x-intercept where y > 0
  # find_x_intercept <- function(fit, NEE) {
  #   # Solve for x where y > 0
  #   x <- NEE$ustar_interp
  #   x_intercept <- uniroot(function(x) predict(fit, newdata = data.frame(x = x)), interval = c(min(x), max(x)))$root
  #   return(x_intercept)
  # }
  #plotting function to check estimated u star threshold against data
  # plot.ustar.threshold <- function(NEE, plot.label){
  #   ustar.0NEE <- NEE[which(NEE$FC_nee_interp >= -1 & NEE$FC_nee_interp <= 1), "ustar_interp"]
  #   #median(ustar.0NEE[which(ustar.0NEE > 0.2)])
  #   # Fitting a nonlinear curve using nls
  #   fit <- nls(y ~ a * x^2 + b * x + c, start = list(a = 1, b = 1, c = 1), data = list(y = NEE$FC_nee_interp, x = NEE$ustar_interp))
  #   # Plotting the data
  #   plot(NEE$ustar_interp, NEE$FC_nee_interp, pch=16, col="blue", xlab="Ustar", ylab="Nighttime NEE", main = paste0(plot.label), ylim = c(0, max(NEE$FC_nee_interp)), xlim = c(0, max(NEE$ustar_interp)))
  #   curve(predict(fit, newdata = list(x = x)), add = TRUE, col = "red", lwd = 2)
  #   #abline(v = median(ustar.0NEE[which(ustar.0NEE > 0.2)]), col = "green", lwd = 2)
  #   abline(v = median(ustar.0NEE), col = "purple", lwd = 2)
  #   text(0.2, max(NEE$FC_nee_interp), paste("Ustar threshold \n", round(median(ustar.0NEE), 3)), pos = 1, col = "black")
  #   text(0.5, max(NEE$FC_nee_interp), paste("NEE Above Threshold \n", round((length(NEE[NEE$ustar_interp > median(ustar.0NEE),"FC_nee_interp"])/length(NEE$FC_nee_interp))*100, 2), "%"), pos = 1, col = "black")
  # }
  # #check data
  # plot.ustar.threshold(NEE = Jan, plot.label = "Jan")
  # plot.ustar.threshold(NEE = Feb, plot.label = "Feb")
  # plot.ustar.threshold(NEE = Mar, plot.label = "Mar")
  # plot.ustar.threshold(NEE = Apr, plot.label = "Apr")
  # plot.ustar.threshold(NEE = May, plot.label = "May")
  # plot.ustar.threshold(NEE = Jun, plot.label = "Jun")
  # plot.ustar.threshold(NEE = Jul, plot.label = "Jul")
  # plot.ustar.threshold(NEE = Aug, plot.label = "Aug")
  # plot.ustar.threshold(NEE = Sep, plot.label = "Sep")
  # plot.ustar.threshold(NEE = Oct, plot.label = "Oct")
  # plot.ustar.threshold(NEE = Nov, plot.label = "Nov")
  # plot.ustar.threshold(NEE = Dec, plot.label = "Dec")
  # plot.ustar.threshold(NEE = growing, plot.label = "Growing Season")
  # plot.ustar.threshold(NEE = nongrowing, plot.label = "Nongrowing Season")
  # plot.ustar.threshold(NEE = site, plot.label = "Annual")
  
  
  
  
  
  
}