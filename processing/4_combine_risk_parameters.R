# combine all data
library(raster)
library(sf)

saveAllRisk <- function(iso, dir){
  cat("processing", iso, "\n")
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
  # add iso
  ffd <- data.frame(iso3=iso, ffd, stringsAsFactors = FALSE)
  # save csv
  write.csv(ffd, file = file.path(odir, paste0(iso, "_all_risk.csv")), row.names = FALSE)
  
  # merge with spatial
  vsf <- merge(vsp, ffd, by = c("NAME_1","NAME_2"), all.x=TRUE)
  
  vsf <- st_as_sf(vsf)
  st_write(vsf, file.path(odir, paste0(iso, "_all_risk.geojson")))
}

dir <- "C:/Users/anibi/Documents/work/covid_hotspot/data"
iso3 <- c("BGD", "ETH", "GHA", "GTM", "HND", "KEN", "MLI", "NPL", "NER", "NGA", "SEN", "UGA")

lapply(iso3, saveAllRisk, dir)


# merge geojson objects to create a single one
jj <- list.files(file.path(dir, "output/final"), pattern = ".geojson", full.names = TRUE)
# read all except NIGER
jj <- grep("NER", jj, invert = TRUE, value = TRUE)

sj <- lapply(jj, st_read)
asj <- do.call(rbind, sj)
st_write(asj, file.path(dir, "output/final/all_countries_risk_exNER.geojson"))

# zip everything for share
zz <- list.files(file.path(dir, "output/final"), pattern = ".geojso", full.names = TRUE)
zname <- paste0("all_countries_geojson_", Sys.time(), ".zip")
zname <- gsub(":| ","_",zname)
zip(file.path(dir, "output/final", zname), zz, flags = " a -tzip",
    zip = "C:\\Program Files\\7-Zip\\7Z")
