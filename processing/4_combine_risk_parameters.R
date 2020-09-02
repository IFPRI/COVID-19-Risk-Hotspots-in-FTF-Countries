# combine all data
library(raster)
library(sf)

saveAllRisk <- function(i, ciso, dir){
  country <- ciso[i,"country"]
  iso <- ciso[i,"iso"]
  
  cat("processing ", country, "\n")
  
  # output directory
  odir <- file.path(dir, "output/final")
  dir.create(odir, FALSE, TRUE)
  
  # spatial layer
  vsp <- raster::getData("GADM", country = iso, level = 2, path = file.path(dir, "processed", iso))
  vsp <- vsp[, c("NAME_1","NAME_2")]
 
  # all risks
  ff <- list.files(file.path(dir, "output", iso), pattern = "*.csv", full.names = TRUE)
  ff <- sort(ff)
  fd <- lapply(ff, read.csv, stringsAsFactors = FALSE)
  
  # merge risk dataframes by admin names
  ffd <- Reduce(function(x, y) merge(x, y, all.x = TRUE, by=c("NAME_1","NAME_2")), fd, accumulate = FALSE)
  # get rid of any specific name
  names(ffd) <- gsub(paste0(iso,"_"), "", names(ffd))
  
  # merge with spatial
  vsf <- merge(vsp, ffd, by = c("NAME_1","NAME_2"), all.x=TRUE)
  # add iso/country
  vsf@data <- data.frame(country = country, iso3 = iso, ffd, stringsAsFactors = FALSE)
  # save csv
  write.csv(vsf@data, file = file.path(odir, paste0(iso, "_all_risk.csv")), row.names = FALSE)
  
  vsf <- st_as_sf(vsf)
  st_write(vsf, file.path(odir, paste0(iso, "_all_risk.geojson")), overwrite = TRUE, append = FALSE)
}

#############################################################################################
# input
dir <- "C:/Users/anibi/Documents/work/covid_hotspot/data"
# country
countries <- c("Bangladesh", "Ethiopia", "Ghana", "Guatemala", 
               "Honduras", "Kenya", "Mali", "Nepal",
               "Niger", "Nigeria", "Senegal", "Uganda")
iso3 <- c("BGD", "ETH", "GHA", "GTM", "HND", "KEN", "MLI", "NPL", "NER", "NGA", "SEN", "UGA")
ciso <- data.frame(country = countries, iso = iso3, stringsAsFactors = FALSE)

lapply(1:nrow(ciso), saveAllRisk, ciso, dir)

#############################################################################################
# merge geojson objects to create a single one
jj <- list.files(file.path(dir, "output/final"), pattern = ".geojson", full.names = TRUE)
# read all except NIGER
jj <- grep("NER", jj, invert = TRUE, value = TRUE)

sj <- lapply(jj, st_read)
asj <- do.call(rbind, sj)
st_write(asj, file.path(dir, "output/final/all_countries_risk_exNER.geojson"))

# zip everything for share
zz <- list.files(file.path(dir, "output/final"), pattern = ".geojson", full.names = TRUE)
zname <- paste0("all_countries_geojson_", Sys.time(), ".zip")
zname <- gsub(":| ","_",zname)
zip(file.path(dir, "output/final", zname), zz, flags = " a -tzip",
    zip = "C:\\Program Files\\7-Zip\\7Z")
