# Download Data from google Drive:

email <- 'sparklelmalone@gmail.com'
googledrive::drive_auth(email = TRUE) 

drive_url <- googledrive::as_id("https://drive.google.com/drive/folders/1Q99CT77DnqMl2mrUtuikcY47BFpckKw3") # The Data 

data_folder <- googledrive::drive_ls(path = drive_url)
localdir <- "~/data"
focal_file <- "FilterReport_MS1Sites.Rdata"
pathDnld <- fs::path(localdir,focal_file)

googledrive::drive_download(file = data_folder$id[data_folder$name == focal_file], 
                            path = pathDnld,
                            overwrite = T)

load( file=pathDnld)
