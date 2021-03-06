# data download workflow
# DHS data comes from IFPRI
# health sites, population, ... from https://data.humdata.org/

# remotes::install_github("dickoa/rhdx")
library(rhdx)

set_rhdx_config(hdx_site = "prod")
get_rhdx_config()

getCovariates <- function(country, dir, year){
  # population
  # search query
  pop <- search_datasets(paste(country,"- Age and sex structures"), rows = 5)
  # generally the first one is the one we are looking for
  ds <- pop[[1]]
  # number of total resources/tifs
  n <- ds$data$num_resources
  
  # where to save results
  dir1 <- file.path(dir, country, "population")
  dir.create(dir1, FALSE, TRUE)
  
  # download population tif
  if(length(n) > 0){
    cat("Downloading population data for", country, "\n")
    lapply(1:n, getDataTif, ds, year, dir1)
  }
  
  # where to save results
  dir2 <- file.path(dir, country, "healthsites")
  dir.create(dir2, FALSE, TRUE)
  
  # get health sites
  hsites <- search_datasets(paste(country," - healthsites"), rows = 5)
  # generally the first one is the one we are looking for
  hds <- hsites[[1]]
  # try all
  lapply(1:hds$data$num_resources, trydownload, hds, dir2)
  
  # where to save results
  dir3 <- file.path(dir, country, "covid_cases")
  dir.create(dir3, FALSE, TRUE)
  
  # get sub-national covid stat
  covcases <- search_datasets(paste(country," - covid-19 data"), rows = 5)
  # generally the first one is the one we are looking for
  cds <- covcases[[1]]
  # try all
  lapply(1:cds$data$num_resources, trydownload, cds, dir3)
}


trydownload <- function(i, hds, dir){
  ds <- get_resource(hds, i)
  out <- tryCatch(read_resource(ds, download_folder = dir), error = function(e) e)
  return(out)
}

getDataTif <- function(i, ds, year, dir){
  
  # insepect all resources 
  dd <- get_resource(ds, i)
  
  # check if for the year we are interested
  ok <- grep(year, dd$data$name)
  
  if(isTRUE(ok > 0)){
    read_resource(dd, download_folder = dir)
  }
}

# directory
# dir <- "C:\\Users\\anibi\\Documents\\work\\covid_hotspot"
# dir <- "/share/spatial02/users/anighosh/covid"
# dir.create(dir, FALSE, TRUE)
dir <- "shared_data/covid/input"
dir.create(dir, TRUE, TRUE)

# FTF countries
countries <- c("Bangladesh", "Ethiopia", "Ghana", "Guatemala", 
               "Honduras", "Kenya", "Mali", "Nepal",
               "Niger", "Nigeria", "Senegal", "Uganda")

# run 1
# getCovariates("Bangladesh", dir, 2020)

# run all
lapply(countries, getCovariates, dir, 2020)


#############################################################################
# health sites data for Africa
# https://www.nature.com/articles/s41597-019-0142-2#Sec7
# url <- "https://www.who.int/malaria/areas/surveillance/who-cds-gmp-2019-01-eng.xlsx?ua=1"
# BGD and NEP is good from HUMDATA

# covid info: may not exist for all countries

# # search query
# cinfo <- search_datasets(paste(country," - covid"), rows = 5)
# # generally the first one is the one we are looking for
# cds <- cinfo[[1]]
# # try xls
# cds <- get_resource(cds, 1)
# read_resource(cds, download_folder = dir)

# accessibility computation using the healthsite facilities
# interactive: https://access-mapper.appspot.com/


getCovariates <- function(country, dir, year){
  
  # where to save results
  dir2 <- file.path(dir, country, "healthsites")
  dir.create(dir2, FALSE, TRUE)
  
  # get health sites
  hsites <- search_datasets(paste(country," - healthsites"), rows = 5)
  # generally the first one is the one we are looking for
  hds <- hsites[[1]]
  # try all
  lapply(1:hds$data$num_resources, trydownload, hds, dir2)
  
  # where to save results
  dir3 <- file.path(dir, country, "covid_cases")
  dir.create(dir3, FALSE, TRUE)
  
  # get sub-national covid stat
  covcases <- search_datasets(paste(country," - covid-19 data"), rows = 5)
  # generally the first one is the one we are looking for
  cds <- covcases[[1]]
  # try all
  lapply(1:cds$data$num_resources, trydownload, cds, dir3)
}


trydownload <- function(i, hds, dir){
  ds <- get_resource(hds, i)
  out <- tryCatch(read_resource(ds, download_folder = dir), error = function(e) e)
  return(out)
}

# directory
# dir <- "C:\\Users\\anibi\\Documents\\work\\covid_hotspot"
# dir <- "/share/spatial02/users/anighosh/covid"
# dir.create(dir, FALSE, TRUE)
dir <- "shared_data/covid/input"
dir.create(dir, TRUE, TRUE)

# FTF countries
countries <- c("Bangladesh", "Ethiopia", "Ghana", "Guatemala", 
               "Honduras", "Kenya", "Mali", "Nepal",
               "Niger", "Nigeria", "Senegal", "Uganda")

# run 1
# getCovariates("Bangladesh", dir, 2020)

# run all
lapply(countries, getCovariates, dir, 2020)

