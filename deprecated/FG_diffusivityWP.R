library(dplyr)
source(file.path("R/MO_Length.R"))
source(file.path("R/eddydiffWP.R"))
#function arguments
#desired concentration
cont.desired <- "CH4"
#add code later that pulls zip files off of g drive
sitecode <- 'KONZ'
#load in interpolated 9 min data
load(file.path("data", sitecode, "KONZ_min9Diff.Rdata"))
load(file.path("data", sitecode, "KONZ_attr.Rdata"))
#call function to calculate eddy diffusivity using AE method
min9EddyDiffWP.list <- eddydiffWP(cont.desired = cont.desired, sitecode = sitecode, min9 = min9Diff.list, attr = attr.df)
wp.check <- min9EddyDiff.list$CH4



# #grab desired concentration
# cont.9minInter <- min9Diff.list[[which(names(min9Diff.list) == cont.desired)]]
# #grab required data columns
# data.eddydiff <- cont.9minInter %>% select(timeEnd_A, timeBgn_A, TowerPosition_A, timeEnd_B, timeBgn_B, TowerPosition_B, timeMid, match_time, ubar4, ubar1, ubar2, ubar3, roughLength_interp)
# #grab only tower heights and positions for matching
# tower.heights <- attr.df %>% select(DistZaxsLvlMeasTow, TowerPosition)
# #adding place holder identifier to create tower height columns
# data.eddydiff$TowerHeight_A <- "hold"
# data.eddydiff$TowerHeight_B <- "hold"
# for(i in 1:dim(attr.df)[1]){
#   #loop over position A
#   data.eddydiff[which(data.eddydiff$TowerPosition_A == i),"TowerHeight_A"] <- tower.heights[which(tower.heights$TowerPosition == i),1]
#   #loop over position B
#   data.eddydiff[which(data.eddydiff$TowerPosition_B == i),"TowerHeight_B"] <- tower.heights[which(tower.heights$TowerPosition == i),1]
# }
# #calculate eddy diffusivty using WP
# #assuming von karman constant is 0.4
# k = 0.4
# #why are we using geometric mean instead of regular mean?
# data.eddydiff$GeometricMean_AB <- sqrt(as.numeric(data.eddydiff$TowerHeight_A)*as.numeric(data.eddydiff$TowerHeight_B))
# 
# #create column for store wind profile eddy diffusivity
# data.eddydiff$eddyDiff_wp <- "hold"
# for(j in 1:dim(data.eddydiff)[1]){
#   ubar = as.numeric(data.eddydiff[j,grep(as.character(data.eddydiff[j,"TowerPosition_A"]), names(data.eddydiff))])
#   z = as.numeric(data.eddydiff[j,"TowerHeight_A"])
#   
#   data.eddydiff[j,"eddyDiff_wp"] <- ((k^2)*ubar*as.numeric(data.eddydiff[j,"GeometricMean_AB"])/log(z/as.numeric(data.eddydiff[j,"roughLength_interp"])))
# }
# data.eddydiff.na <- na.omit(data.eddydiff)
