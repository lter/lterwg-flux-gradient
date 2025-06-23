load(file='/Volumes/MaloneLab/Research/FluxGradient/Sites_AOP_Summary.Rdata')
localdir <- '/Volumes/MaloneLab/Research/FluxGradient/FluxData'
Sites.Summary

# Library
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(ggpubr)
library(factoextra)
library(reshape2)

Sites.Summary %>% names

# Range plots for the AOP ...
plot.range.evi <- Sites.Summary %>% ggplot() +
  geom_segment( aes(x=EVI.mean - EVI.sd , 
                    xend= EVI.mean + EVI.sd, 
                    y=site), color="black") + 
  ylab( "") + xlab("EVI") + theme(text = element_text(size = 10),
                                               panel.background = element_rect(fill='transparent') ) 


plot.range.ndvi <- Sites.Summary %>% ggplot() +
  geom_segment( aes(x=NDVI.mean - NDVI.sd , 
                    xend= NDVI.mean + NDVI.sd, 
                    y=site), color="black") + 
  ylab( "") + xlab("NDVI")+ theme(text = element_text(size = 10),
                                  panel.background = element_rect(fill='transparent') ) 

plot.range.pri <- Sites.Summary %>% ggplot() +
  geom_segment( aes(x= round(PRI.mean - PRI.sd,2) , 
                    xend= round(PRI.mean + PRI.sd,2), 
                    y=site), color="black") + 
   ylab( "") + xlab("PRI") + theme(text = element_text(size = 10),
                                   panel.background = element_rect(fill='transparent') ) 

plot.range.lai <- Sites.Summary %>% ggplot() +
  geom_segment( aes(x= round(LAI.mean - LAI.sd,2) , 
                    xend= round(LAI.mean + LAI.sd,2), 
                    y=site), color="black") + 
  ylab( "") + xlab("LAI") + theme(text = element_text(size = 10),
                                  panel.background = element_rect(fill='transparent') ) 


plot.range.chm <- Sites.Summary %>% ggplot() +
  geom_segment( aes(x= round(CHM.mean - CHM.sd,2) , 
                    xend= round(CHM.mean + CHM.sd,2), 
                    y=site), color="black") + 
  ylab( "") + xlab("CHM") + theme(text = element_text(size = 10),
                                  panel.background = element_rect(fill='transparent') ) 

plot.range.savi <- Sites.Summary %>% ggplot() +
  geom_segment( aes(x= round(SAVI.mean - SAVI.sd,2) , 
                    xend= round(SAVI.mean + SAVI.sd,2), 
                    y=site), color="black") + 
  ylab( "") + xlab("SAVI") + theme(text = element_text(size = 10),
                                  panel.background = element_rect(fill='transparent') ) 

setwd(localdir)
png("Structure_Summary_Allsites.png", width=10, 
    height=10, units="in", res=1200)

ggarrange( plot.range.evi, 
           plot.range.ndvi,
           plot.range.pri, 
           plot.range.lai,
           plot.range.savi,
           labels=c('a', 'b', 'c', 'd', 'e'), nrow=1)
dev.off()

# Scale data:

Sites.Summary.scaled <- Sites.Summary %>% mutate( EVI.mean.scaled = scale(EVI.mean),
                                                  NDVI.mean.scaled = scale(NDVI.mean),
                                                  PRI.mean.scaled = scale(PRI.mean),
                                                  LAI.mean.scaled = scale(LAI.mean),
                                                  SAVI.mean.scaled = scale(SAVI.mean),
                                                  CHM.mean.scaled = scale(CHM.mean))

# create a matrix plot of the scaled data:

matrix <- Sites.Summary.scaled %>% select(EVI.mean.scaled,
                                          NDVI.mean.scaled,
                                          PRI.mean.scaled,
                                          LAI.mean.scaled, 
                                          SAVI.mean.scaled,
                                          CHM.mean.scaled) %>% as.matrix.data.frame()

rownames(matrix) <- Sites.Summary.scaled$site
colnames(matrix) <- c("EVI", "NDVI", "PRI", "LAI", "SAVI", "CHM")



longData <- melt(matrix)
longData<-longData[longData$value!=0,] %>% na.omit


setwd(localdir)
png("Structure_Summary_Allsites_Matrix.png", width=7, 
    height=10, units="in", res=1200)

ggplot(longData, aes(x = Var2, y = Var1)) + 
  geom_raster(aes(fill=value)) + 
  scale_fill_gradient("" ,low="goldenrod", high="darkgreen") +
  labs(x="", y="", title="", names="") +
  theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                     axis.text.y=element_text(size=9),
                     plot.title=element_text(size=11))

dev.off()

png("Structure_Summary_Allsites_CHM.png", width=6, 
    height=3, units="in", res=1200)
Sites.Summary.scaled %>% ggplot(aes(x= LAI.mean.scaled, y = CHM.mean.scaled, col=NDVI.mean.scaled)) +  
  geom_point( alpha=0.3) +
  geom_text(aes(label = site), check_overlap = TRUE) + theme_bw() + 
  labs(x="LAI", y="CHM") 
dev.off()
# Use MDS on the scaled data: - cluster analysis:

distance <- dist(Sites.Summary.scaled ) # euclidean 
library(usedist)
distance <- dist_setNames(distance, Sites.Summary.scaled$site)
fit <- cmdscale(distance ,eig=TRUE, k=2)

Sites.Summary.scaled$MDS1 <- fit$points[,1]
Sites.Summary.scaled$MDS2 <- fit$points[,2]

# Cluster analysis and Explanation of clusters:
library(caret)

fviz_nbclust(matrix %>% na.omit , kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

cluster <- distance %>% kmeans( centers = 4, nstart = 30)

Sites.Summary.scaled$cluster <- cluster$cluster %>% as.factor

Sites.Summary.scaled %>% ggplot( aes(x= MDS1 , y = MDS2, col = cluster, label=site )) + 
  geom_point( alpha=0.2) + geom_text(nudge_y = 0.15, nudge_x =- 0.15, size = 2) 

png("Structure_Summary_Allsites_Cluster-Matrix.png", width=8, 
    height=7, units="in", res=1200)

fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
dev.off()


png("Structure_Summary_Allsites_Cluster.png", width=8, 
    height=7, units="in", res=1200)
fviz_cluster(cluster, data=distance, pointsize = 0.5, labelsize=8) + theme_bw()
dev.off()



