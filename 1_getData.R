# data download workflow
# DHS data comes from IFPRI
# health sites, population, ... from https://data.humdata.org/

dir <- "C:\\Users\\anibi\\Documents\\work\\covid_hotspot"
dir <- "/share/spatial02/users/anighosh/covid"
dir.create(dir, FALSE, TRUE)

# remotes::install_github("dickoa/rhdx")
library(rhdx)
library(sf)

set_rhdx_config(hdx_site = "prod")
get_rhdx_config()

# list of population dataset for a country

country <- "Bangladesh"

# search query
pop <- search_datasets(paste(country,"- Age and sex structures"), rows = 5)
# generally the first one is the one we are looking for
ds <- pop[[1]]
# number of total resources/tifs
n <- ds$data$num_resources

getDataTif <- function(i, ds, year, dir, country){
  
  # insepect all resources 
  dd <- get_resource(ds, i)
  
  # check if for the year we are interested
  ok <- grep(year, dd$data$name)
  
  dir <- dir.create(file.path(dir, country), FALSE, TRUE)
  
  if(isTRUE(ok > 0)){
    read_resource(dd, download_folder = dir)
  }
}

ff <- lapply(1:n, getDataTif, ds, year = "2020", dir)
# we don't want the STARS object
rm(ff)



# get health sites

# search query
hsites <- search_datasets(paste(country," - healthsites"), rows = 5)
# generally the first one is the one we are looking for
hds <- hsites[[1]]
# try csv
hds <- get_resource(hds, 2)
hv <- read_resource(hds, download_folder = dir)

# clean and save as shapefile for EarthEngine
hv <- hv[complete.cases(hv[,c("X","Y")]), c("X","Y","amenity")]

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
hvsf <- st_as_sf(x = hv,                         
               coords = c("X", "Y"),
               crs = projcrs)

st_write(hvsf, file.path(dir, "health_facilities_all.shp"))

# covid info: may not exist for all countries

# search query
cinfo <- search_datasets(paste(country," - covid"), rows = 5)
# generally the first one is the one we are looking for
cds <- cinfo[[1]]
# try xls
cds <- get_resource(cds, 1)
read_resource(cds, download_folder = dir)


# accessibility computation using the healthsite facilities
# interactive: https://access-mapper.appspot.com/
