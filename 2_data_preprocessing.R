library(terra)

# combine population rasters based on DHS age groups
combineAgeRaster <- function(i, agroup, gender, dir, country, overwrite){
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
  
  if(!(file.exists(outname))|overwrite){
    f <- grep(paste0("_",gender,"_"), ff, value = TRUE)
    # all age of files
    fg <- as.numeric(unlist(lapply(strsplit(basename(f), "_"), "[[", 3)))
    # which are falling within the range 
    fs <- f[fg >= ag$low & fg < ag$high] # confirmed age group with Jawoo
    r <- rast(fs)
    # order of operation helps with faster processing
    r <- aggregate(r, fact=10, fun="sum", na.rm=TRUE)
    r <- app(r, fun=sum, nodes=5, na.rm=TRUE, filename=outname, overwrite=overwrite)
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
  lapply(1:nrow(agroup), combineAgeRaster, agroup, "m", dir, country, overwrite = TRUE)
  lapply(1:nrow(agroup), combineAgeRaster, agroup, "f", dir, country, overwrite = TRUE)
}


# check the results
# summary stat for the results
# r <- rast(ff)
# r1 <- aggregate(r, fact=10, fun="sum", na.rm = TRUE)
# # check total pop
# # r2 <- app(r1, fun=sum, nodes=4)
# x1 <- global(r1, "sum", na.rm = TRUE)
# sum(x1$sum)
# 
# x <- global(r, "sum", na.rm = TRUE)
# sum(x$sum)

#############################################################################################
# in case processing the 1 km raster downloaded from the WorldPop website
library(terra)

cropRaster <- function(iso, r, dir){
  
  cat("processing", basename(r), "\n")
  
  datadir <- file.path(dir,  "processed", iso)
  dir.create(datadir, FALSE, TRUE)
  
  vsp <- raster::getData("GADM", country = iso, level = 0, path = datadir)
  v <- vect(vsp)
  
  outname <- file.path(datadir, gsub("global", iso, basename(r)))
  
  r <- rast(r)
  r <- crop(r, v)
  r <- mask(r, v, filename = outname, overwrite = TRUE)
}

dir <- "/share/spatial02/users/anighosh/covid/worldpop"
rr <- list.files(file.path(dir, "global"), pattern = ".tif", full.names = TRUE)

iso3 <- c("BGD", "ETH", "GHA", "GTM", "HND", "KEN", "MLI", "NPL", "NER", "NGA", "SEN", "UGA")

for(r in rr){
  lapply(iso3, cropRaster, r, dir)
}

##################################################################################################
# combine worldpop agegroup using the nature paper

# combine population rasters based on DHS age groups
combineAgeRaster <- function(i, agroup, gender, dir, iso, overwrite){
  datadir <- file.path(dir, iso)
    
  ff <- list.files(datadir, pattern = glob2rx(paste0(iso,"_",gender,"_","*2020_1km.tif")), full.names = TRUE)
  
  # don't use anything other than the 1km one
  ff <- grep("1km", ff, value = TRUE)
  
  # which age group to process
  ag <- agroup[i,]
  
  # output file name
  outname <- file.path(datadir, paste0(iso,"_pop_agegroup_",gender,"_",ag$low,"_",ag$high,"_1km.tif"))
  
  if(!(file.exists(outname))|overwrite){
    f <- grep(paste0("_",gender,"_"), ff, value = TRUE)
    # all age of files
    fg <- as.numeric(unlist(lapply(strsplit(basename(f), "_"), "[[", 3)))
    # which are falling within the range 
    fs <- f[fg >= ag$low & fg < ag$high] # confirmed age group with Jawoo
    rin <- rast(fs)
    r <- app(rin, fun=sum, nodes=5, na.rm=TRUE, filename=outname, overwrite=overwrite)
  }
}

agroup <- data.frame(low = c(18,40,50,60,70,80),
                     high = c(40,50,60,70,80,100), stringsAsFactors = FALSE)

countries <- c("Bangladesh", "Ethiopia", "Ghana", "Guatemala", 
               "Honduras", "Kenya", "Mali", "Nepal",
               "Niger", "Nigeria", "Senegal", "Uganda")

iso3 <- c("BGD", "ETH", "GHA", "GTM", "HND", "KEN", "MLI", "NPL", "NER", "NGA", "SEN", "UGA")

# dir <- "/share/spatial02/users/anighosh/covid/worldpop/processed"
# indir <- "C:/Users/anibi/Documents/work/covid_hotspot/data/processed"

for (iso in iso3){
  cat("processing", iso, "\n")
  for (gender in c("m","f")){
    lapply(1:nrow(agroup), combineAgeRaster, agroup, gender, dir, iso, overwrite = TRUE)
  }
}

# create agegroup (15-45) for female with child bearning age that would be used for the BMI
outname <- file.path(datadir, paste0(iso,"_women_agegroup_15_45_1km.tif"))

##################################################################################################
# compute and save population statistics

getStatPop <- function(iso, indir, outdir){
  cat("processing", iso, "\n")
  datadir <- file.path(indir, iso)
  outdir <- file.path(outdir, iso)
  dir.create(outdir, FALSE, TRUE)
  
  # raster files
  ff <- list.files(datadir, pattern = glob2rx(paste0(iso,"*_pop_agegroup_*.tif")), full.names = TRUE)
  rr <- rast(ff)
  
  # compute stat by district
  vsp <- raster::getData("GADM", country = iso, level = 2, path = datadir)
  v <- vect(vsp)
  
  # output csv filename
  outname <- file.path(outdir, "pop_agegroup_summary.csv")
  
  # summary stat for each district
  popstat <- extract(rr, v, fun = sum, na.rm = TRUE)
  # drop ID column
  popstat <- popstat[, -1]
  colnames(popstat) <- gsub(".tif","",basename(ff)) 
  # save result
  popstat <- data.frame(vsp[,c("NAME_1", "NAME_2")], area_polygons_sqkm = area(v)/1000000, popstat, stringsAsFactors = FALSE)
  write.csv(popstat, outname, row.names = FALSE)
}

# indir <- "/share/spatial02/users/anighosh/covid/worldpop/processed"
# indir <- "C:/Users/anibi/Documents/work/covid_hotspot/data/processed"
# outdir <- "C:/Users/anibi/Documents/work/covid_hotspot/data/output"
lapply(iso3, getStatPop, indir, outdir)

#####################################################################################################
# get health sites
library(raster)

getStatHealthSites <- function(iso, hv, datadir, outdir){
  cat("processing", iso, "\n")
  datadir <- file.path(indir, iso)
  outdir <- file.path(outdir, iso)
  dir.create(outdir, FALSE, TRUE)
  
  # compute stat by district
  vsp <- raster::getData("GADM", country = iso, level = 2, path = datadir)
  v <- vect(vsp)
  
  # output csv filename
  outname <- file.path(outdir, "pop_agegroup_summary.csv")
  
  # summary stat for each district
  popstat <- extract(rr, v, fun = sum, na.rm = TRUE)
  # drop ID column
  popstat <- popstat[, -1]
  colnames(popstat) <- gsub(".tif","",basename(ff)) 
  # save result
  popstat <- data.frame(vsp[,c("NAME_1", "NAME_2")], area_polygons_sqkm = area(v)/1000000, popstat, stringsAsFactors = FALSE)
  write.csv(popstat, outname, row.names = FALSE)
}

hv <- shapefile("C:\\Users\\anibi\\Documents\\work\\covid_hotspot\\data\\vector\\healthsites\\World-node.shp")
