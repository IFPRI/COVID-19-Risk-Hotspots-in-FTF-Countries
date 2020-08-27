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
