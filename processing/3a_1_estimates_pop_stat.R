library(terra)

getPopStat<- function(iso, dir){
  cat("processing pop stat", iso, "\n")
  
  # directory
  datadir <- file.path(dir, "processed", iso)
  outdir <- file.path(dir, "output" ,iso)
  dir.create(outdir, FALSE, TRUE)
  
  # list of input files
  ff <- list.files(datadir, pattern = glob2rx(paste0(iso,"*_1km.tif")), full.names = TRUE)
  
  # output file name
  outname <- file.path(outdir, paste0(iso,"_popstat.csv"))
  
  # read raster
  rr <- rast(ff)
  
  # admin boundaries
  v <- raster::getData("GADM", country = iso, level = 2, path = file.path(dir,"processed",iso))  
  # vect class
  vct <- vect(v)
  
  # summary stat for each district
  wstat <- extract(rr, vct, fun = sum, na.rm = TRUE)
  wstat <- wstat[,2:ncol(wstat)]
  wstat <- data.frame(NAME_0 = v$NAME_0, NAME_1 = v$NAME_1, NAME_2 = v$NAME_2, wstat, stringsAsFactors = FALSE)
  
  write.csv(wstat, outname, row.names = FALSE, fileEncoding = "latin1")
}

# run analysis
dir <- "/share/spatial02/users/anighosh/covid/worldpop/"

iso3 <- c("BGD", "ETH", "GHA", "GTM", "HND", "KEN", "MLI", "NPL", "NER", "NGA", "SEN", "UGA")

parallel::mclapply(iso3, getPopStat, dir, mc.cores=6, mc.preschedule=FALSE)
# lapply(iso3, getPopStat, dir))

# combine all csv and write a final one
csvs <- list.files(file.path(dir, "output"), pattern = "_popstat.csv$", 
                   full.names = TRUE, recursive = TRUE)

d <- lapply(csvs, read.csv, stringsAsFactors = FALSE)
d <- do.call("rbind", lapply(d, function(x) data.frame(as.list(x))))

write.csv(d, file.path(dir, "output/all_country_pop_stat.csv"), 
          row.names = FALSE)
