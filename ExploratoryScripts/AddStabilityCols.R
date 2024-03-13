load(file.path("data", "Validation", paste0("SITES_WP_val.Rdata")))
all.sites.wp <- bind_rows(SITES_WP_validation)
load(file.path("data", "Validation", paste0("SITES_AE_val.Rdata")))
all.sites.ae <- bind_rows(SITES_AE_validation)
#ADD STABILITY COLUMNS TO WP AND MBR
#select just site, gas, timeMid, and stability columns
stability.df <- all.sites.ae %>% select(site, gas, timeMid, Stability_100, Stability_500)
#METHOD WP
#add columns
all.sites.wp <- all.sites.wp %>% left_join(stability.df, by = c("site", "gas", "timeMid"))

#METHOD MBR
#grab CO2 cols for mbr
co2.cols <- grep("_CO2", names(all.sites.mbr), value = T)
#select for only CO2 cols for mbr
mbr.co2 <- all.sites.mbr[,c(co2.cols, "site")]
#filter to gas = CO2 for stability columns
stability.co2.df <- stability.df %>% filter(gas == "CO2")
#remove gas column
stability.co2.df$gas <- NULL
#rename cols to match mbr convention
names(stability.co2.df)[which(names(stability.co2.df) == "timeMid")] <- "timeMid_CO2"
names(stability.co2.df)[which(names(stability.co2.df) == "Stability_500")] <- "Stability_500_CO2"
names(stability.co2.df)[which(names(stability.co2.df) == "Stability_100")] <- "Stability_100_CO2"
#add stability cols
mbr.co2 <- mbr.co2 %>% left_join(stability.co2.df, by = c("site", "timeMid_CO2"))

#grab H2O cols for mbr
h2o.cols <- grep("_H2O", names(all.sites.mbr), value = T)
#select for only CO2 cols for mbr
mbr.h2o <- all.sites.mbr[,c(h2o.cols, "site")]
#filter to gas = CO2 for stability columns
stability.h2o.df <- stability.df %>% filter(gas == "H2O")
#remove gas column
stability.h2o.df$gas <- NULL
#rename cols to match mbr convention
names(stability.h2o.df)[which(names(stability.h2o.df) == "timeMid")] <- "timeMid_H2O"
names(stability.h2o.df)[which(names(stability.h2o.df) == "Stability_500")] <- "Stability_500_H2O"
names(stability.h2o.df)[which(names(stability.h2o.df) == "Stability_100")] <- "Stability_100_H2O"
#add stability cols
mbr.h2o <- mbr.h2o %>% left_join(stability.h2o.df, by = c("site", "timeMid_H2O"))
#remove site col (otherwise there will be duplicates when we combine)
mbr.h2o$site <- NULL

#grab CH4 cols for mbr
ch4.cols <- grep("_CH4", names(all.sites.mbr), value = T)
#select for only CO2 cols for mbr
mbr.ch4 <- all.sites.mbr[,c(ch4.cols, "site")]
#filter to gas = CO2 for stability columns
stability.ch4.df <- stability.df %>% filter(gas == "CH4")
#remove gas column
stability.ch4.df$gas <- NULL
#rename cols to match mbr convention
names(stability.ch4.df)[which(names(stability.ch4.df) == "timeMid")] <- "timeMid_CH4"
names(stability.ch4.df)[which(names(stability.ch4.df) == "Stability_500")] <- "Stability_500_CH4"
names(stability.ch4.df)[which(names(stability.ch4.df) == "Stability_100")] <- "Stability_100_CH4"
#add stability cols
mbr.ch4 <- mbr.ch4 %>% left_join(stability.ch4.df, by = c("site", "timeMid_CH4"))
#remove site col (otherwise there will be duplicates when we combine)
mbr.ch4$site <- NULL

#combine all data frames
all.sites.mbr <- cbind(mbr.co2, mbr.h2o, mbr.ch4)

#save as .Rdata objects
save(all.sites.wp, file = file.path("data", "Validation", "SITES_WP_Stability.Rdata"))
save(all.sites.mbr, file = file.path("data", "Validation", "SITES_MBR_Stability.Rdata"))

#upload to gdrive
# Pull data from google drive
email <- 'alexisrose0525@gmail.com'
#copy this browser url from the site folder on the shared G drive (located at https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3) you wish to upload your zip files to
#this url should point to the NEONSITES_Validation folder
drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/14Ga9sLRMlQVvorZdHBiYxCbUybiGwPNp")

googledrive::drive_upload(media = paste0("data/Validation/SITES_WP_Stability.Rdata"), overwrite = T, path = drive_url)
googledrive::drive_upload(media = paste0("data/Validation/SITES_MBR_Stability.Rdata"), overwrite = T, path = drive_url)


