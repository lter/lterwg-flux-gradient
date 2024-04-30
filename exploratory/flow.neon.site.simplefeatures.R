
# Load libraries:
library( sf)
library(ggplot2)

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


test <- assign.wedge(shp=Sites.shp, r=4, n_wedges=8)

plot(test$x)
