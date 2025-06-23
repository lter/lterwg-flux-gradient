
# Load libraries:
library( sf)
library(ggplot2)
library(dplyr)


# Import the csv of sites and their locations. 
Sites <- read.csv('/Volumes/MaloneLab/Research/FluxGradient/Ameriflux_NEON field-sites.csv')

# Make dataframe a shapfile:
Sites.shp <- st_as_sf(x = Sites,                         
                      coords = c("Longitude..degrees.",  "Latitude..degrees."),
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
  sf$site <- Sites.shp$Site_Id.NEON[i]
  sites.wedges <-rbind( sites.wedges, sf)
}


# Load shapefile created in flow.neon.site.squarebuffers.R:
load('/Volumes/MaloneLab/Research/FluxGradient/NEONLTERsiteBuffers.Rdata')

sites.wedges <- sites.wedges %>% st_transform( st_crs(Sites.shp))
site.Buffers <- Site.Buffers %>% st_transform( st_crs(Sites.shp)) 

# Take the intersection of the site files and the wedges:
site.buffers.wedges <- site.Buffers %>% st_intersection(sites.wedges) %>% filter( site == site.1)

save(site.Buffers,site.buffers.wedges,
     file='/Volumes/MaloneLab/Research/FluxGradient/FG_Site_Wdges.RDATA')

message("Next run flow_AOP_FormatLayers")

message('After you prepare the AOP layers now you can use the products of this script to summarize information at the site level')