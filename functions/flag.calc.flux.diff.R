#' flag.calc.flux.diff
#'
#' @param day.night.df gas concentration data frame filtered to either day or night
#'
#' @return dataframe with spike.flag column where 1 = spike and 0 = no spike
#' 
#'
#' @author Alexis Helgeson
flag.calc.flux.diff <- function(day.night.df){
  #add a looping column where the numbers are all consecutive: cut() does not return consecutive numbers if there is missing data, which there is because our fluxes are not gapfilled
  num.groups <- unique(day.night.df$spike.bin)
  loop.num <- seq(1, length(num.groups),1)
  #add loop.num as column to dataframe for indexing within the loop
  for(l in 1:length(unique(day.night.df$date))){
    day.night.df[which(day.night.df$spike.bin==num.groups[l]),"loop.num"] <- loop.num[l]
  }
  #calculate median of differences (Md)/MAD for day/night fluxes for each spike.bin
  #daytime
  for(d in 1:length(num.groups)){
    group.bin <- day.night.df %>% filter(spike.bin==num.groups[d])
    #for last group need to pull in 1 day from previous bin to calculate differences
    #we don't want to grab 1st entry in previous bin because that will be over 13 days from current date, so we want to take the last entry in the previous bin and add it to the beginning
    if(d == length(num.groups)){
      group.bin.minus1 <- rbind(day.night.df[which(day.night.df$loop.num==(d-1))[length(which(day.night.df$loop.num==(d-1)))],], group.bin)
      #calculate difference of FG
      group.bin$day.d <- diff(group.bin.minus1$FG)
    }else{
      #add 1 obs from next bin to end of the dataframe to calculate differences
      group.bin.plus1 <- rbind(group.bin, day.night.df[which(day.night.df$loop.num==(d+1))[1],])
      #calculate difference of FG
      group.bin$day.d <- diff(group.bin.plus1$FG)
    }
    #TO DO: add error message here if there are NAs in group.bin$day.d, there should not be and this is an indication something went wrong with the group selection/matching
    #calculate median of differences
    day.Md <- median(group.bin$day.d)
    #calculate MAD
    day.MAD <- mad(group.bin$FG)
    #set upper/lower threshold for difference using z=5.5
    lower.threshold <- day.Md - ((5.5*day.MAD)/0.6745)
    upper.threshold <- day.Md + ((5.5*day.MAD)/0.6745)
    #flag which day.d fall outside of range, use NEON convention 1 = bad data, 0 = good data
    group.bin[which(group.bin$day.d < lower.threshold),"spike.flag"] <- "1"
    group.bin[which(group.bin$day.d > upper.threshold), "spike.flag"] <- "1"
    group.bin[which(group.bin$day.d >= lower.threshold & group.bin$day.d <= upper.threshold),"spike.flag"] <- "0"
    #add flag to larger df to save
    day.night.df[which(day.night.df$spike.bin==num.groups[d]),"spike.flag"] <- group.bin$spike.flag
  }
  
  return(day.night.df)
}