library(terra)

# combine population rasters based on DHS age groups
combineAgeRaster <- function(i, agroup, gender, dir, country){
  datadir <- file.path(dir, country, "population")
  outdir <- file.path(dir, country, "processed")
  dir.create(outdir, FALSE, TRUE)
  
  ff <- list.files(datadir, pattern = "*.tif", full.names = TRUE)
  # don't use the 1km one
  ff <- grep("1km", ff, value = TRUE, invert = TRUE)
  
  # which age group to process
  ag <- agroup[i,]
  
  # output file name
  outname <- file.path(outdir, paste0("pop_",gender,"_",ag$low,"_",ag$high,"_1km.tif"))
  
  if(!file.exists(outname)){
    f <- grep(paste0("_",gender,"_"), ff, value = TRUE)
    # all age of files
    fg <- as.numeric(unlist(lapply(strsplit(basename(f), "_"), "[[", 3)))
    # which are falling within the range 
    fs <- f[fg >= ag$low & fg < ag$high] # confirm age group with Jawoo
    # r <- stack(fs)
    # r <- overlay(r, fun = sum)
    # save the result as 1 km
    # r <- aggregate(r, fact = 10,  filename = outname)
    r <- rast(fs)
    r <- app(r, fun=sum, nodes=4)
    r <- aggregate(r, fact=10,  filename=outname, nodes=4)
  }
}

dir <- "/share/spatial02/users/anighosh/covid"

# combine worldpop agegroup using the nature paper
agroup <- data.frame(low = c(18,40,50,60,70,80),
                     high = c(40,50,60,70,80,100), stringsAsFactors = FALSE)

countries <- c("Bangladesh", "Ethiopia", "Ghana", "Guatemala", 
               "Honduras", "Kenya", "Mali", "Nepal",
               "Niger", "Nigeria", "Senegal", "Uganda")

for (country in countries){
  cat("processing", country, "\n")
  lapply(1:nrow(agroup), combineAgeRaster, agroup, "m", dir, country)
  lapply(1:nrow(agroup), combineAgeRaster, agroup, "f", dir, country)
}


###################################################################################################
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


