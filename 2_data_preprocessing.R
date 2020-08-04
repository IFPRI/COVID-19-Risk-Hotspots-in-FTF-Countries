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
dhs <- read.csv("data/DHS_biomarkers_region_level.csv", stringsAsFactors = FALSE)
View(dhs)
