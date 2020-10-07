# New accessibility raster provided by the malaria atlas group
# https://malariaatlas.org/research-project/accessibility-to-healthcare/
# https://malariaatlas.org/geoserver/ows?service=CSW&version=2.0.1&request=DirectDownload&ResourceId=Explorer:2020_walking_only_travel_time_to_healthcare
# https://malariaatlas.org/geoserver/ows?service=CSW&version=2.0.1&request=DirectDownload&ResourceId=Explorer:2020_motorized_travel_time_to_healthcare

# there was name matching issues with the csv statistics provided by ihme
# so I decided to recompute the statistics from ihme raster data using GADM level 2 boundary
library(terra)

getAccessibilityRiskScore <- function(iso, dir, medium){
  cat("processing access to healthsites", iso, "\n")
  
  indir <- file.path(dir, "input")
  datadir <- file.path(indir, "malaria_atlas")
  odir <- file.path(dir, "output", iso)
  dir.create(odir, FALSE, TRUE)
  
  # folders
  ffd <- list.dirs(datadir, full.names = TRUE)
  ffd <- grep(medium, ffd, value = TRUE)
  
  # files
  ff <- list.files(ffd, pattern = ".geotiff", full.names = TRUE)
  # read raster
  rr <- rast(ff)
  
  # admin boundaries
  v <- raster::getData("GADM", country = iso, level = 2, path = file.path(dir,"processed",iso))  
  # vect class
  vct <- vect(v)
  
  # summary stat for each district
  wstat <- extract(rr, vct, fun = sum, na.rm = TRUE)
  wstat <- wstat[,2:ncol(wstat)]
  wstat <- data.frame(NAME_1 = v$NAME_1, NAME_2 = v$NAME_2, wstat, stringsAsFactors = FALSE)
  
  # read population summary
  pop <- read.csv(file.path(dir, "output", iso, "pop_agegroup_summary.csv"), stringsAsFactors = FALSE)
  pops <- pop[,grep("60_70|70_80|80_100", colnames(pop), value = TRUE)]
  pop <- data.frame(pop[,c("NAME_1", "NAME_2")], pop_age_above_60 = rowSums(pops, na.rm = TRUE)) 
  
  # merge with accessibility
  astat <- merge(wstat, pop, by = c("NAME_1","NAME_2"))
  astat$health_facilityaccess_ht60 <- astat$wstat/astat$pop_age_above_60
  # to save the results
  astat <- astat[,c("NAME_1", "NAME_2", "health_facilityaccess_ht60")]
  names(astat)[3] <- paste0(medium, "_health_facilityaccess_ht60")
  
  outname <- file.path(dir, "output", iso, paste0(iso,"_",medium,"_access_to_healthsite_riskscore.csv"))
  write.csv(astat, outname, row.names = FALSE, fileEncoding = "latin1")
}

# run analysis
dir <- "C:/Users/anibi/Documents/work/covid_hotspot/data"

iso3 <- c("BGD", "ETH", "GHA", "GTM", "HND", "KEN", "MLI", "NPL", "NER", "NGA", "SEN", "UGA")
lapply(iso3, getAccessibilityRiskScore, dir, "walking")
lapply(iso3, getAccessibilityRiskScore, dir, "motorized")
