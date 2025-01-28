
# Load libraries:
library( sf)
library(ggplot2)
library(dplyr)
# Import the csv of sites and their locations. 
Sites <- read.csv('/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/FG_Site_Locations - Sheet1.csv')

# Make dataframe a shapfile:
Sites.shp <- st_as_sf(x = Sites,                         
                      coords = c("Longitude",  "Latitude"),
                      crs = "epsg:4326")

# Create a USA AOI:
aoi.usa <- AOI::aoi_get(country= c("USA", "PR") )

# Visualize the point locations within the USA:
ggplot( ) + geom_sf(data=aoi.usa) + geom_sf(data=Sites.shp)

# Add projection to the shape file crs:
#Sites.shp.proj <- sf::st_transform(Sites.shp ,crs = "epsg:4087" )

st_wedge <- function(x,y,r,start,width,n=20){
  theta = seq(start, start+width, length=n)
  xarc = x + r*sin(theta)
  yarc = y + r*cos(theta)
  xc = c(x, xarc, x)
  yc = c(y, yarc, y)
  st_polygon(list(cbind(xc,yc)))   
}

st_wedges <- function(x, y, r, nsegs){
  width = (2*pi)/nsegs
  starts = (1:nsegs)*width
  polys = lapply(starts, function(s){st_wedge(x,y,r,s,width)})
  mpoly = st_cast(do.call(st_sfc, polys), "MULTIPOLYGON")
  mpoly
}

n_wedges <- 8

Sites.shp$X <- Sites.shp %>% st_coordinates() %>% as.data.frame %>% select(X)
Sites.shp$Y <- Sites.shp %>% st_coordinates() %>% as.data.frame %>% select(Y)

assign.wedge <- function(shp, r, n_wedges) {
  wedges <- st_wedges(shp$X[1,], shp$Y[1,], r, n_wedges) %>%
    st_as_sf() %>%
    st_set_crs(4326) %>%
    st_transform(3488)
  
  wedges$wedge <- seq(1,8, 1)
  
  return(wedges )
}

length(Sites.shp$Name)

sites.wedges <-c()

for( i in 1:length(Sites.shp$Name)){
  print(i)
  sf <- assign.wedge(shp=Sites.shp[i,], r=4, n_wedges=8) %>% st_as_sf
  sf$Site.Id.NEON <- Sites.shp$Site.Id.NEON[i]
  sites.wedges <-rbind( sites.wedges, sf)
}

# Load shapefile created in Site.Spatial.Homo:
load('/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/data/NEONLTERsiteBuffers.Rdata')
st_crs(sites.wedges)
"epsg:4326"
sites.wedges <- sites.wedges %>% st_transform( st_crs(BONA.shp))

# Take the intersection of the site files and the wedges:
BONA.wedges <- sites.wedges %>% st_intersection(BONA.shp )
GUAN.wedges <- sites.wedges %>% st_intersection(GUAN.shp )
HARV.wedges <- sites.wedges %>% st_intersection(HARV.shp )
JORN.wedges <- sites.wedges %>% st_intersection(JORN.shp )
KONZ.wedges <- sites.wedges %>% st_intersection(KONZ.shp )
NIWO.wedges <- sites.wedges %>% st_intersection(NIWO.shp )
TOOL.wedges <- sites.wedges %>% st_intersection(TOOL.shp )
CPER.wedges <- sites.wedges %>% st_intersection(CPER.shp )

plot(CPER.wedges$x)

save(sites.wedges,
     BONA.wedges,
     GUAN.wedges,
     HARV.wedges,
     JORN.wedges, 
     KONZ.wedges,
     NIWO.wedges, 
     TOOL.wedges,
     CPER.wedges,
     file='/Users/sm3466/Dropbox (YSE)/Research/FluxGradient/FG_Site_Wdges.RDATA')