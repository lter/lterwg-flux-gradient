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
  #group data by growing and non growing season: use May-Oct (growing season) Nov-Apr (nongrowing season)
  # growing <- site.CO2 %>% filter(month == 1 | month == 2 | month == 3 | month == 4 | month == 11 | month == 12)
  # nongrowing <- site.CO2 %>% filter(month == 5 | month == 6 | month == 7 | month == 8 | month == 9 | month == 10)
  #define function to calculate ustar
  threshold <- function(NEE){
    ustar.0NEE <- NEE[which(NEE$FC_nee_interp >= -1 & NEE$FC_nee_interp <= 1), "ustar_interp"]
    ut <- median(ustar.0NEE)
    return(ut)
  }
  #calculate threshold for each temporal group
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
  All.threshold <- threshold(NEE=site.CO2)
  #save ustar threshold values in dataframe
  ustar.threshold.df <- data.frame(site = unique(site.CO2$site),Jan = Jan.threshold, Feb = Feb.threshold, Mar = Mar.threshold, Apr = Apr.threshold, May = May.threshold, Jun = Jun.threshold, Jul = Jul.threshold, Aug = Aug.threshold, Sep = Sep.threshold, Oct = Oct.threshold, Nov = Nov.threshold, Dec = Dec.threshold, All = All.threshold)
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
  
  return(ustar.threshold.df)
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