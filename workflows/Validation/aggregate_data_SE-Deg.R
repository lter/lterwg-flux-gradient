# install.packages("librarian")
# librarian::shelf(data.table)
library(data.table)
library(dplyr)
library(tictoc)
# Load data more efficiently
file_path <- "/global/scratch/users/kdelwiche/Flux_Gradient/concentration_2020_aa.csv"

# Clear environment
rm(list = ls())
Sys.setenv(tz="UTC")

base.path = "/global/scratch/users/kdelwiche/Flux_Gradient/"
filename1 = paste0(base.path,"concentration_2020_aa.csv")

### Read in csv file
### Terminal command to split the files up
### split -l 345600 -d concentration_2021.csv concentration_2021_
#data1.raw = read.csv("/Users/rcommane/Data_LTER/FluxGradient/data/SE-Deg_concentration_2020_first1000rows.csv",sep=",",header=T)
file.1 = paste0(base.path,"concentration_2020_aa.csv")
data1.raw <- fread(file.1,sep=",",header=T)
###fread(filename1, sep=",", header=TRUE)[-1, ]

### read in individual files
file.list1 = list.files(path=base.path,pattern="concentration_2020_",full.names = T)

### Read in first file on list
pb <- txtProgressBar(min = 1, max = length(file.list1), style = 3)

for (j in 1:length(file.list1)){
  #length(file.list1)){
  tic()
  print(j) #for (i in 1:length(file.list1)){
  
  #j=2
  data.rawA = fread(file.list1[j],sep=",",header=F,skip=2)
  colnames(data.rawA) = colnames(data1.raw)
  
  data2 <- data.rawA %>%
    mutate(
      DateTime = as.POSIXct(data.rawA$TIMESTAMP, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
      Date = as.Date(DateTime))
  
  # Convert all possible numeric columns
  data1 <- as.data.frame(data2 %>% 
                           mutate(across(where(is.character), ~ as.numeric(.))),)
  
  # Identify transition points for LEVEL changes
  end_time <- data1$DateTime[which(diff(data1$LEVEL) != 0)]
  time.avg.interval = 30
  #head(end_time)
  
  # Compute statistics efficiently
  print("mean_sum-ing")
  mean_sum <- lapply(end_time, function(t) {
    subset_data <- data1 %>%
      filter(DateTime > (t - time.avg.interval) & DateTime < t)
    
    if (nrow(subset_data) > 0) {
      subset_data %>%
        summarise(
          Date = as.POSIXct(mean(as.numeric(DateTime), na.rm=TRUE),origin="1970-01-01",tz="UTC"), 
          Start_Time = format(as.POSIXct(min(as.numeric(DateTime), na.rm=TRUE),
                                         origin="1970-01-01",tz="UTC"), "%H:%M:%S"),
          End_Time = format(as.POSIXct(max(as.numeric(DateTime), na.rm=TRUE),
                                       origin="1970-01-01",tz="UTC"), "%H:%M:%S"),
          LEVEL = mean(LEVEL, na.rm=TRUE),
          CO2.mean = mean(CO2, na.rm=TRUE),
          CH4.mean = mean(CH4, na.rm=TRUE),
          H2O.mean = mean(H2O, na.rm=TRUE),
          CO2.sd = sd(CO2, na.rm=TRUE),
          CH4.sd = sd(CH4, na.rm=TRUE),
          H2O.sd = sd(H2O, na.rm=TRUE),
          CO2.n = sum(!is.na(CO2)),
          CH4.n = sum(!is.na(CH4)),
          H2O.n = sum(!is.na(H2O))
        )
    }
  }) %>%
    bind_rows()  # Combine results into a data frame
  
  print("saving out data files")
  if (j < 10){use.num = sprintf("%02d", j)}else{use.num=j}
  file.out1 = paste0(base.path,"data_1min/conc_2020_",use.num,"_1min.csv")
  write.table(mean_sum,file=file.out1,sep = ",",row.names = F)
  
  #mean_sum_all =  rbind(mean_sum_all,mean_sum) 
  ### %>% bind_rows()  # Combine results into a data frame
  #setTxtProgressBar(pb, j)
  rm(list=c("data.rawA","data1","data2"))
  toc()
}
close(pb)
#file.out1 = paste0("/Users/rcommane/Data_LTER/FluxGradient/data/conc_2021_1min_all.csv")
#write.table(mean_sum_all,file=file.out1,sep = ",",row.names = F)
