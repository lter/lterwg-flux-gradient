## ----------------------------------------------- ##
       # Single Site Download / Wrangling
## ----------------------------------------------- ##
# Written by: Nick Lyon, ...

## ------------------------------ ##
          # Housekeeping ----
## ------------------------------ ##

# Load needed libraries
# install.packages("librarian")
librarian::shelf(neonUtilities, tidyverse, BiocManager, rhdf5, oce,
                 naniar, gtools, R.utils) 

# Clear environment
rm(list = ls())

# Load fun custom functions
for(fxn in dir(path = file.path("R"))){
  source(file = file.path("R", fxn ))
}

# Set desired site name and NEON code
sitename <- 'Konza Praire'
sitecode <- 'KONZ'

# Create needed folder(s)
dir.create(path = file.path("data"), showWarnings = F)
dir.create(path = file.path(sitename), showWarnings = F)

## ------------------------------ ##
      # Download Data ----
## ------------------------------ ##

# Set start and end date for observations
startdate <- "2021-01"
enddate <- "2023-09"

# Download desired data file
# neonUtilities::zipsByProduct(dpID = "DP4.00200.001", site = sitecode, 
#                              startdate = "2021-11", enddate = "2023-08",
#                              package = "basic", check.size = T,
#                              savepath = file.path(sitename))

# Identify names of downloaded ZIP files
zip.files <- dir(path = file.path(sitename, "filesToStack00200"), pattern = ".zip")

# Unzip those files!
for(j in 1:length(zip.files)){
  unzip(file.path(sitename, "filesToStack00200", zip.files[j]),
        exdir = file.path(sitename, "filesToStack00200"))
}

# Identify the .gz files created by unzipping
gz.files <- dir(path = file.path(sitename, "filesToStack00200"), pattern = ".gz")

# Process those as well!
for(k in 1:length(gz.files)){
  R.utils::gunzip(file.path(sitename, "filesToStack00200", gz.files[k]), 
                  destname = file.path(sitename,
                                       gsub(pattern = ".gz", replacement = "", 
                                            x = gz.files[k])), remove = F)
}

## ------------------------------ ##
    # Calculate CH4 Fluxes ----
## ------------------------------ ##

# Grab h5 files to be passed to SiteAttributes and SiteDF
hd.files <- dir(path = file.path(sitename), pattern = "\\.h5$")

# Strip out attribute data
attr <- data.frame(rhdf5::h5readAttributes(file.path(sitename, hd.files[1]),
                                           name = paste0("/", sitecode))) %>%
  # Pare down to only some needed columns
  dplyr::select(DistZaxsLvlMeasTow, DistZaxsCnpy) %>%
  # Generate a site column
  dplyr::mutate(Site = sitecode, .before = dplyr::everything())

# Check that out
dplyr::glimpse(attr)




# (vVv) BASEMENT (vVv) ----

# Basement note:
## Code "basements" are for storing code that hasn't been completely debugged



#grab co2, h20, ch4 level 1 data at all 30min resolution tower heights along with level 4 co2, sensible heat, latent heat fluxes, uStar, uBar, air temp, z0
cont.df <- Site.DF(hd.files, sitecode, frequency = "high")
#filter for good data
h2o.qfqm.df <- cont.df %>% filter(h2o_qfqm.000_040_30m == "0") %>% filter(h2o_qfqm.000_060_30m == "0") %>% filter(F_co2_qfqm == "0")

co2.qfqm.df <- cont.df %>% filter(co2_qfqm.000_040_30m == "0") %>% filter(co2_qfqm.000_060_30m == "0") %>% filter(F_LE_qfqm == "0")
#save df as csv
# write.csv(cont.df, paste0(sitecode, "_", startdate, "_", enddate, "_cont.csv"))
#calculate flux gradient
#select lower twoer height
z1_height <- attr.df$DistZaxsLvlMeasTow[4]
#select upper tower height
z2_height <- attr.df$DistZaxsLvlMeasTow[6]
#select tallest height for sonic anemometer
z_height <- attr.df$DistZaxsLvlMeasTow[6]
#calculate fluxes using Modified Bowan Ratio
mbr.df <- Flux_Gradient_MBR(cont.df = co2.qfqm.df, attr.df, z1_height, z2_height)

#save df as csv
# write.csv(mbr.df, paste0(sitecode, "_", startdate, "_", enddate, "_mbr.csv"))
#calculate fluxes using aerodynamic profile and wind profile
ae.df <- Flux_Gradient_AE(cont.df = co2.qfqm.df, attr.df, z1_height, z2_height, z_height)
#save df as csv
write.csv(ae.df, paste0(sitecode, "_", startdate, "_", enddate, "_ae.csv"))



# End ----
