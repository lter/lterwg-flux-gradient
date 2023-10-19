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

# Un-ZIP those downloaded products
unzip_neon(in_path = file.path(sitename, "filesToStack00200"),
           out_path = file.path(sitename), 
           quiet = FALSE)

## ------------------------------ ##
  # Extract HD5 Information ----
## ------------------------------ ##

# Identify temporal resolution
temporal_res <- "09m"

# Grab h5 files to be passed to SiteAttributes and SiteDF
hd.files <- dir(path = file.path(sitename), pattern = "\\.h5$")

# Strip out attribute data
attr <- data.frame(rhdf5::h5readAttributes(file.path(sitename, hd.files[1]),
                                           name = paste0("/", sitecode))) %>%
  # Pare down to only some needed columns
  # dplyr::select(DistZaxsLvlMeasTow, DistZaxsCnpy) %>%
  # Generate a site column
  dplyr::mutate(Site = sitecode, .before = dplyr::everything())

# Check that out
dplyr::glimpse(attr)

# Make an empty list for storage purposes
tower_conc <- list()

# Grab actual data in .hd5 files
# for(g in 1:length(hd.files)){
for(g in 1){
  
  # Identify focal file
  focal.hd <- hd.files[g]
  
  # Starting message
  message("Processing begun for '", focal.hd, "'")
  
  # List the contents of hdf5 file and returns a df with file contents
  focal.df <- rhdf5::h5ls(file = file.path(sitename, focal.hd),
                          recursive = T, all = T, datasetinfo = T,
                          index_type = rhdf5::h5default("H5_INDEX"),
                          order = rhdf5::h5default("H5_ITER"), 
                          s3 = F, s3credentials = NULL, native = F)
  
  # Figure out the 'group' name
  focal.group <- paste0("/", sitecode, "/dp01/data/ch4Conc")
  
  # Identify the index values within that
  focal.indices <- unique(focal.df[which(focal.df$group == focal.group), ]$name)
  
  # Pare down to only the codes beginning with 000s
  focal.0s <- unique(stringr::str_subset(string = focal.indices, pattern = "000"))
  
  # And further pare down to the desired temporal resolution
  focal.temporal <- unique(stringr::str_subset(string = focal.0s, pattern = temporal_res))
  
  # And rip out just the height number
  height.number <- as.numeric(stringr::str_sub(string = focal.temporal, start = 6, end = 6))
  
  # Make some placeholder data objects for later use
  df_CH4 <- data.frame()
  df_CO2 <- data.frame()
  df_H2O <- data.frame()
  
  # Now we'll loop across the height numbers within this dataset
  for(position in 1:length(height.number)){
    
    # Progress message
    message("Processing height ", position, " (of ", length(height.number), ")")
    
    # Set gas concentration dataset names
    ch4.dir <- paste0("/", sitecode, '/dp01/data/ch4Conc/000_0', 
                      height.number[position], "0_", temporal_res, "/rtioMoleDryCh4")
    co2.dir <- paste0("/", sitecode, '/dp01/data/isoCo2/000_0', 
                      height.number[position], "0_", temporal_res, "/rtioMoleDryCo2")
    h2o.dir <- paste0("/", sitecode, '/dp01/data/isoH2o/000_0',
                      height.number[position], "0_", temporal_res, "/rtioMoleDryH2o")
    
    # Set the QFQM dataset name to be used by h5read
    ch4.qfqm <- paste0("/", sitecode, '/dp01/qfqm/ch4Conc/000_0', 
                       height.number[position], "0_", temporal_res, "/rtioMoleDryCh4")
    co2.qfqm <- paste0("/", sitecode, '/dp01/qfqm/isoCo2/000_0', 
                       height.number[position], "0_", temporal_res, "/rtioMoleDryCo2")
    h2o.qfqm <- paste0("/", sitecode, '/dp01/qfqm/isoH2o/000_0',
                       height.number[position], "0_", temporal_res, "/rtioMoleDryH2o")
    
    # Read in h5 files as arrays
    CH4 <- rhdf5::h5read(file = file.path(sitename, focal.hd), name = ch4.dir)
    CO2 <- rhdf5::h5read(file = file.path(sitename, focal.hd), name = co2.dir)
    H2O <- rhdf5::h5read(file = file.path(sitename, focal.hd), name = h2o.dir)
    
    # Read in QFQM files
    CH4.qfqm <- rhdf5::h5read(file = file.path(sitename, focal.hd), name = ch4.qfqm)
    CO2.qfqm <- rhdf5::h5read(file = file.path(sitename, focal.hd), name = co2.qfqm)
    H2O.qfqm <- rhdf5::h5read(file = file.path(sitename, focal.hd), name = h2o.qfqm)
    
    # Combine data with QFQMs (and add tower position)
    CH4.both <- dplyr::full_join(CH4, CH4.qfqm, by = c("timeBgn", "timeEnd")) %>%
      dplyr::mutate(hd5.file = focal.hd,
                    tower.position = height.number[position],
                    .before = dplyr::everything())
    CO2.both <- dplyr::full_join(CO2, CO2.qfqm, by = c("timeBgn", "timeEnd")) %>%
      dplyr::mutate(hd5.file = focal.hd,
                    tower.position = height.number[position],
                    .before = dplyr::everything())
    H2O.both <- dplyr::full_join(H2O, H2O.qfqm, by = c("timeBgn", "timeEnd")) %>%
      dplyr::mutate(hd5.file = focal.hd,
                    tower.position = height.number[position],
                    .before = dplyr::everything())
    
    # Combine with prior iterations of the loop (and arrange by beginning time)
    df_CH4 <- dplyr::bind_rows(df_CH4, CH4.both) %>%
      dplyr::arrange(timeBgn)
    df_CO2 <- dplyr::bind_rows(df_CO2, CO2.both) %>%
      dplyr::arrange(timeBgn)
    df_H2O <- dplyr::bind_rows(df_H2O, H2O.both) %>%
      dplyr::arrange(timeBgn)
    
  } # Close tower position loop
  
  # Assemble that information into a list!
  tower_conc[[paste0("attr_", g)]] <- attr
  tower_conc[[paste0("CH4_", g)]] <- df_CH4
  tower_conc[[paste0("CO2_", g)]] <- df_CO2
  tower_conc[[paste0("H2O_", g)]] <- df_H2O
  
} # Close loop across hd5 files

## ------------------------------ ##
# Process Data ----
## ------------------------------ ##

# Check out output list
names(tower_conc)

# Make a new list to store simplified outputs in
result_list <- list()

# Now (ironically) we'll use a loop to unlist what the first loop made
for(data_type in c("attr", "CH4", "CO2", "H2O")){
  
  # For each data type...
  list_sub <- tower_conc %>%
    # Identify all list elements that contain this type of data
    purrr::keep(.p = stringr::str_detect(string = names(.),
                                         pattern = data_type)) %>%
    # Unlist by selecting all columns of each list element
    purrr::list_rbind()
  
  # Add this to the simpler results list
  result_list[[data_type]] <- list_sub
  
  # And print a message
  message("Centralized dataframe for ", data_type, " extracted") }

# Check out the simplified results list we're left with
names(result_list)
dplyr::glimpse(result_list)

# Grab each bit as a dataframe for ease of further modification and/or export
attr.total <- result_list[["attr"]]

## For non-attribute data, also remove 'bad' data (based on quality flags)
CH4.total <- result_list[["CH4"]] %>%
  dplyr::filter(qfFinl == 0)
CO2.total <- result_list[["CO2"]] %>%
  dplyr::filter(qfFinl == 0)
H2O.total <- result_list[["H2O"]] %>%
  dplyr::filter(qfFinl == 0)



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
