country <- "Bangladesh"

# combine population rasters based on DHS age groups
# search datasets downloaded
ff <- list.files(dir, pattern = "*.tif", full.names = TRUE)
ff <- grep("1km", ff, value = TRUE, invert = TRUE)

# combine worldpop agegroup using the nature paper
agroup <- data.frame(low = c(18,40,50,60,70,80),
                     high = c(40,50,60,70,80,100), stringsAsFactors = FALSE)


combineAgeRaster <- function(i, agroup, ff, gender, dir){
  ag <- agroup[i,]
  outname <- file.path(dir, paste0("pop_",gender,"_",ag$low,"_",ag$high,".tif"))
  
  if(!file.exists(outname)){
    f <- grep(paste0("_",gender,"_"), ff, value = TRUE)
    # age of f
    fg <- as.numeric(unlist(lapply(strsplit(basename(f), "_"), "[[", 3)))
    
    fs <- f[fg >= ag$low & fg < ag$high] # confirm age group with Jawoo
    r <- stack(fs)
    r <- overlay(r, fun = sum, filename = outname) 
  }
}


# DHS
dhs <- read.csv("data/DHS_biomarkers_cluster_level.csv", stringsAsFactors = FALSE)
View(dhs)


# clean health sites
# clean and save as shapefile for other applications
hv <- hv[complete.cases(hv[,c("X","Y")]), c("X","Y","amenity")]
# unique(hv$amenity)
hv1 <- hv[hv$amenity == "hospital",]
hv2 <- hv[hv$amenity == "clinic",]
hv3 <- hv[hv$amenity %in% c("pharmacy","doctors"),]

write.csv(hv1, "data/bgd_hospital.csv", row.names = FALSE)
write.csv(hv2, "data/bgd_clinic.csv", row.names = FALSE)
write.csv(hv3, "data/bgd_pharmacy_doctors.csv", row.names = FALSE)

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
hvsf <- st_as_sf(x = hv,                         
                 coords = c("X", "Y"),
                 crs = projcrs)

st_write(hvsf, file.path(dir, "health_facilities_all.shp"))



v <- getData("GADM", country = "BGD", level = 2, path = dir)

r1 <- crop(r, v)
r1 <- mask(r1,v)
plot(r1)


