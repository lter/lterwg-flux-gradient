# Good levels:

rm(list=ls())

library(dplyr)
library(ggplot2)
library(ggridges)
library(ggpubr)

dir <- '/Volumes/MaloneLab/Research/FluxGradient/RandomForestModel/'
localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
load(fs::path(localdir,paste0("SITES_One2One.Rdata")))
canopy <- read.csv(file.path(paste(localdir, "canopy_commbined.csv", sep="/"))) %>% distinct

Highest.CCC <- SITES_One2One %>% reframe(.by= c(Site, gas, Approach), CCC.max = max(CCC, na.rm=T))

# Define the Good
SITES_One2One_canopy <- SITES_One2One %>% full_join(canopy, by=c("Site", "dLevelsAminusB" ) ) %>% 
  full_join( Highest.CCC,by = c("Site", "gas", "Approach")) %>% mutate(Approach = factor(Approach, levels = c("MBR", "AE", "WP") ),
                                                                       Good.CCC = case_when( CCC >= 0.5 & Approach == "MBR" ~ 1,
                                                                                             CCC >= 0.75 & Approach == "WP" ~ 1,
                                                                                             CCC >= 0.7 & Approach == "AE" ~ 1,
                                                                                             CCC < 0.5 & Approach == "MBR" ~ 0,
                                                                                             CCC < 0.75 & Approach == "WP" ~ 0,
                                                                                             CCC < 0.7 & Approach == "AE" ~ 0) %>% as.factor)
# Set up VSURF

SITES_One2One_canopy %>% names

# Divide into test and training datasets:
train <- SITES_One2One_canopy %>% 
  sample_frac(0.65) 

test <- anti_join(SITES_One2One_canopy, train )

SITES_One2One_canopy %>% summary
train  %>% summary
test  %>% summary

# Variable Selection : ####
library(VSURF)
library(randomForest)

SITES_One2One_canopy %>% names
train[, c(7:8, 10,78)] %>% names

rf_index.sdesign.vsurf <- VSURF(train[, c(7:8, 10,78)], 
                            train[["Good.CCC"]],
                            ntree = 500,
                            RFimplem = "randomForest", 
                            clusterType = "PSOCK", 
                            verbose = TRUE,
                            ncores = parallelly::availableCores() - 2, parallel= TRUE)

rf_index.sdesign.vsurf$varselect.pred
rf_index.sdesign.vars <-names( train[, c(7:8, 10,78)]) [rf_index.sdesign.vsurf$varselect.interp] 


train.sub <- train[, c(21:38,80)] %>% na.omit
rf_index.Cspec.vsurf <- VSURF(train.sub[-19], 
                              train.sub[["Good.CCC"]],
                                ntree = 500,
                              mtry=2,
                                RFimplem = "randomForest", 
                                clusterType = "PSOCK", 
                                verbose = TRUE,
                                ncores = parallelly::availableCores() - 2, parallel= TRUE)

rf_index.Cspec.vsurf$varselect.pred
rf_index.Cspec.vars <-names( train.sub[-19]) [rf_index.Cspec.vsurf$varselect.interp] 



train.sub <- train[, c(40:51, 61:64,80)] %>% na.omit
rf_index.Cstructure.vsurf <- VSURF(train.sub[-17], 
                                   train.sub[["Good.CCC"]],
                                   ntree = 500,
                                   mtry=2,
                                   RFimplem = "randomForest", 
                                   clusterType = "PSOCK", 
                                   verbose = TRUE,
                                   ncores = parallelly::availableCores() - 2, parallel= TRUE)

rf_index.Cstructure.vsurf$varselect.pred
rf_index.Cstructure.vars <-names( train.sub[-17]) [rf_index.Cstructure.vsurf$varselect.pred] 


train.sub <- train[,c(rf_index.sdesign.vars, rf_index.Cspec.vars, rf_index.Cstructure.vars,"Canopy_L1", "Canopy_L2","dLevelsAminusB","Good.CCC")] %>% na.omit
train.sub %>% names

rf_index.final.vsurf <- VSURF(train.sub[-10], 
                                   train.sub[["Good.CCC"]],
                                   ntree = 500,
                                   mtry=2,
                                   RFimplem = "randomForest", 
                                   clusterType = "PSOCK", 
                                   verbose = TRUE,
                                   ncores = parallelly::availableCores() - 2, parallel= TRUE)

rf_index.final.vsurf$varselect.pred
rf_index.final.vars <- c(names( train.sub[-25]) [rf_index.final.vsurf$varselect.pred], "Canopy_L2")

# Final Model Fit : ####

final.vars <- c("Approach","CHM.sd", "EVI.mean" ,"LAI.mean" ,"SAVI.mean" ,"Cutoff05.SDH", "Canopy_L2")    


rf_model <- randomForest( Good.CCC ~ .,
                              data= train %>% select(c(Good.CCC, all_of(final.vars))) %>% na.omit,
                              importance=TRUE,
                              predicted=TRUE,
                              keep.inbag=TRUE)

rf_model 

varImpPlot(rf_model)

library(caret)
train$rf_model <- predict(rf_model , train)
confusionMatrix(train$rf_model, train$Good.CCC)

# Save the model 
dir <- '/Volumes/MaloneLab/Research/FluxGradient/RandomForestModel/'
save(train, test,rf_model, final.vars, SITES_One2One_canopy, file= paste(dir, "Good_Fluxes.Rdata", sep="") )


# Sensitivity Analysis: ####
load(file= paste(dir, "Good_Fluxes.Rdata", sep="") )

SITES_One2One_canopy$Approach %>% unique
SITES_One2One_canopy$Canopy_L2 %>% unique

SITES_One2One_canopy$CHM.sd %>% summary
SITES_One2One_canopy$Cutoff05.SDH %>% summary
SITES_One2One_canopy$SAVI.mean %>% summary
SITES_One2One_canopy$EVI.mean %>% summary
SITES_One2One_canopy$LAI.mean %>% summary

mean.df <- SITES_One2One_canopy %>% reframe( .by=c(Approach, Canopy_L2), 
                                             CHM.sd = mean(CHM.sd,na.rm=T),
                                             Cutoff05.SDH = mean(Cutoff05.SDH,na.rm=T),
                                             SAVI.mean = mean(SAVI.mean, na.rm=T),
                                             EVI.mean = mean(EVI.mean,na.rm=T),
                                             LAI.mean = mean(LAI.mean,na.rm=T))

min.df <- SITES_One2One_canopy %>% reframe( .by=c(Approach, Canopy_L2), 
                                                CHM.sd = min(CHM.sd,na.rm=T),
                                                Cutoff05.SDH = min(Cutoff05.SDH,na.rm=T),
                                                SAVI.mean = min(SAVI.mean, na.rm=T),
                                                EVI.mean = min(EVI.mean,na.rm=T),
                                                LAI.mean = min(LAI.mean,na.rm=T))

max.df <- SITES_One2One_canopy %>% reframe( .by=c(Approach, Canopy_L2), 
                                            CHM.sd = max(CHM.sd,na.rm=T),
                                            Cutoff05.SDH = max(Cutoff05.SDH,na.rm=T),
                                            SAVI.mean = max(SAVI.mean, na.rm=T),
                                            EVI.mean = max(EVI.mean,na.rm=T),
                                            LAI.mean = max(LAI.mean,na.rm=T))

summary.df <- rbind( mean.df, min.df, max.df)

CHM.sd.df <- data.frame( CHM.sd = seq(SITES_One2One_canopy$CHM.sd %>% min(na.rm=T), SITES_One2One_canopy$CHM.sd %>% max(na.rm=T), 2) )
Cutoff05.SDH.df <- data.frame( Cutoff05.SDH = seq(SITES_One2One_canopy$Cutoff05.SDH %>% min(na.rm=T), SITES_One2One_canopy$Cutoff05.SDH %>% max(na.rm=T), 0.2) )
SAVI.mean.df <- data.frame( SAVI.mean = seq(SITES_One2One_canopy$SAVI.mean %>% min(na.rm=T), SITES_One2One_canopy$SAVI.mean %>% max(na.rm=T), 0.01) )
EVI.mean.df <- data.frame( EVI.mean = seq(SITES_One2One_canopy$EVI.mean %>% min(na.rm=T), SITES_One2One_canopy$EVI.mean %>% max(na.rm=T), 0.01) )
LAI.mean.df <- data.frame( LAI.mean = seq(SITES_One2One_canopy$LAI.mean %>% min(na.rm=T), SITES_One2One_canopy$LAI.mean %>% max(na.rm=T), 0.01) )

# Format Factors

format.factors <- function( data){
  data$Approach <- factor( data$Approach , levels = c("MBR", "AE", "WP"))
  data$Canopy_L2 <- factor( data$Canopy_L2 , levels = c("AA+" , "AA", "AW+-", "AW+", "AW-" , "AW",   "WW-", "WW" ))
  
  return(data)
}

CHM.sd.final <- summary.df %>% select(! CHM.sd) %>% cross_join(CHM.sd.df )
Cutoff05.SDH.final <- summary.df %>% select(!Cutoff05.SDH) %>% cross_join(Cutoff05.SDH.df )
SAVI.mean.final <- summary.df %>% select(! SAVI.mean) %>% cross_join(SAVI.mean.df )
EVI.mean.final <- summary.df %>% select(! EVI.mean) %>% cross_join(EVI.mean.df )
LAI.mean.final <- summary.df %>% select(! LAI.mean) %>% cross_join(LAI.mean.df )

CHM.sd.final$model <-  predict(rf_model  ,CHM.sd.final,'prob')[,2]
Cutoff05.SDH.final$model <-  predict(rf_model  ,Cutoff05.SDH.final,'prob')[,2]
SAVI.mean.final$model <-  predict(rf_model  ,SAVI.mean.final,'prob')[,2]
EVI.mean.final$model <-  predict(rf_model  ,EVI.mean.final,'prob')[,2]
LAI.mean.final$model <-  predict(rf_model  , LAI.mean.final,'prob')[,2]

CHM.sd.final <-  CHM.sd.final%>% format.factors 
Cutoff05.SDH.final <- Cutoff05.SDH.final %>% format.factors
SAVI.mean.final <- SAVI.mean.final %>% format.factors
EVI.mean.final <- EVI.mean.final%>% format.factors
LAI.mean.final <- LAI.mean.final %>% format.factors
 

Sensitivity_plot <- function(df, approach, label, var){

 
  plot <-  ggplot() + geom_smooth(data= df %>% filter(Approach == approach),
                                  aes_string( x= var , y = 'model', col= 'Canopy_L2'), alpha=0.2) + 
    scale_color_manual(values=c("goldenrod4", "goldenrod2",
                                "green1","green3", "aquamarine2","aquamarine4",
                                "purple", "darkmagenta")) +theme_bw() + xlab(label)
  
  return(plot)
}

# MBR
plot.mbr.1 <- Sensitivity_plot(df = CHM.sd.final, approach = "MBR", label= "SD Canopy Height", var='CHM.sd' )
plot.mbr.2 <- Sensitivity_plot(df = Cutoff05.SDH.final, approach = "MBR", label= "SDH", var='Cutoff05.SDH' )
plot.mbr.3 <- Sensitivity_plot(df = EVI.mean.final, approach = "MBR", label= "EVI", var='EVI.mean' )
plot.mbr.4 <- Sensitivity_plot(df = SAVI.mean.final , approach = "MBR", label= "SAVI", var='SAVI.mean' )
plot.mbr.5 <- Sensitivity_plot(df = LAI.mean.final , approach = "MBR", label= "LAI", var='LAI.mean' )  


ggarrange(plot.mbr.1,
          plot.mbr.2,
          plot.mbr.3,
          plot.mbr.4,
          plot.mbr.5, common.legend = TRUE )

plot.ae.1 <- Sensitivity_plot(df = CHM.sd.final, approach = "AE", label= "Deviation in Canopy Height", var='CHM.sd' )
plot.ae.2 <- Sensitivity_plot(df = Cutoff05.SDH.final, approach = "AE", label= "SD Height", var='Cutoff05.SDH' )
plot.ae.3 <- Sensitivity_plot(df = EVI.mean.final, approach = "AE", label= "EVI", var='EVI.mean' )
plot.ae.4 <- Sensitivity_plot(df = SAVI.mean.final , approach = "AE", label= "SAVI", var='SAVI.mean' )
plot.ae.5 <- Sensitivity_plot(df = LAI.mean.final , approach = "AE", label= "LAI", var='LAI.mean' )  

ggarrange(plot.ae.1,
          plot.ae.2,
          plot.ae.3,
          plot.ae.4,
          plot.ae.5, common.legend = TRUE )

plot.wp.1 <- Sensitivity_plot(df = CHM.sd.final, approach = "WP", label= "Deviation in Canopy Height", var='CHM.sd' )
plot.wp.2 <- Sensitivity_plot(df = Cutoff05.SDH.final, approach = "WP", label= "SDH", var='Cutoff05.SDH' )
plot.wp.3 <- Sensitivity_plot(df = EVI.mean.final, approach = "WP", label= "EVI", var='EVI.mean' )
plot.wp.4 <- Sensitivity_plot(df = SAVI.mean.final , approach = "WP", label= "SAVI", var='SAVI.mean' )
plot.wp.5 <- Sensitivity_plot(df = LAI.mean.final , approach = "WP", label= "LAI", var='LAI.mean' )  

ggarrange(plot.wp.1,
          plot.wp.2,
          plot.wp.3,
          plot.wp.4,
          plot.wp.5, common.legend = TRUE )
