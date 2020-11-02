# wash score
# https://cloud.ihme.washington.edu/s/bkH2X2tFQMejMxy

# there was name matching issues with the csv statistics provided by ihme
# so I decided to recompute the statistics from ihme raster data using GADM level 2 boundary
library(terra)
library(raster)

getWashScore <- function(iso, dir){
  cat("processing wash score", iso, "\n")
  
  indir <- file.path(dir, "input")
  datadir <- file.path(indir, "ihme")
  odir <- file.path(dir, "output", iso)
  dir.create(odir, FALSE, TRUE)
  
  toremove <- "IHME_LMIC_WASH_2000_2017_|_Y2020M06D02"
  
  # files
  ff <- list.files(datadir, pattern = glob2rx("*MEAN_2017_Y2020M06D02*.TIF"), 
                   full.names = TRUE)
  # read raster
  rr <- rast(ff)
  names(rr) <- gsub(toremove, "", names(rr))
  
  # admin boundaries
  v <- getData("GADM", country = iso, level = 2, path = file.path(dir,"processed",iso))  
  # vect class
  vct <- vect(v)
  
  # summary stat for each district
  wstat <- extract(rr, vct, fun = mean, na.rm = TRUE)
  wstat <- wstat[,2:ncol(wstat)]
  wstat[,1:4] <- 100 - wstat[,1:4]
  colnames(wstat) <- paste0(colnames(wstat), "_no_access")
  
  wstat <- data.frame(NAME_1 = v$NAME_1, NAME_2 = v$NAME_2, wstat, stringsAsFactors = FALSE)
  
  outname <- file.path(dir, "output", iso, paste0(iso,"_wash_riskscore.csv"))
  write.csv(wstat, outname, row.names = FALSE, fileEncoding = "latin1")
}

# download the zipped csv statistics
# setup directories
dir <- "C:/Users/anibi/Documents/work/covid_hotspot/data"

iso3 <- c("BGD", "ETH", "GHA", "GTM", "HND", "KEN", "MLI", "NPL", "NER", "NGA", "SEN", "UGA")
lapply(iso3, getWashScore, dir)

# indir <- file.path(dir, "input")
# odir <- file.path(dir, "output")
# datadir <- file.path(indir, "ihme")
# zdir <- file.path(datadir, "Data [CSV]", "Admin 2")

# check if the urls are working
# dir.create(datadir, FALSE, TRUE)
# u <- "https://cloud.ihme.washington.edu/s/bkH2X2tFQMejMxy/download?path=%2F&files=Data%20%5BCSV%5D&downloadStartSecret=j47kqmvso7r"
# download.file(u, file.path(datadir, "data_csv.zip"), mode = "wb")
# 
# dfile <- file.path(datadir, "data_csv.zip")
# u1 <- "https://cloud.ihme.washington.edu/s/bkH2X2tFQMejMxy/download?path=%2F&downloadStartSecret=5700chmiled"
# download.file(u1, dfile, mode = "wb")
# 
# unzip(dfile, exdir = datadir)
# 

# f <- "https://cloud.ihme.washington.edu/s/bkH2X2tFQMejMxy/download?path=%2FW_PIPED%20-%20Access%20to%20piped%20water%20%5BGeoTIFF%5D%2FPercent&files=IHME_LMIC_WASH_2000_2017_W_PIPED_PERCENT_UPPER_2017_Y2020M06D02.TIF"
# dw <- file.path(datadir, "IHME_LMIC_WASH_2000_2017_W_PIPED_PERCENT_UPPER_2017_Y2020M06D02.TIF")
# download.file(f, dw, mode = "wb")

# zfiles <- list.files(zdir, pattern = ".zip$", full.names = TRUE)
# sapply(zfiles, unzip, exdir = zdir)

getDataAll <- function(f, countries){
  d <- read.csv(f, stringsAsFactors = FALSE)
  d <- d[d$ADM0_NAME %in% countries & d$year == 2017 & d$metric == "Percent",]
  
  toremove <- "IHME_LMIC_WASH_2000_2017_|_ADMIN_2_Y2020M06D02|.CSV"
  cname <- gsub(toremove, "", basename(f))
  d <- d[,c("ADM0_NAME","ADM1_NAME","ADM2_NAME", "mean")]
  names(d) <- c("NAME_0","NAME_1","NAME_2", paste0(cname, "_pct"))
  return(d)
}

splitDataISO <- function(i, ciso, dbase, dir){
  country <- ciso[i,"country"]
  iso <- ciso[i,"iso"]
  db1 <- dbase[dbase$NAME_0 == country,]
  # remove extra column
  nm1 <- db1$NAME_1
  nm2 <- db1$NAME_2
  c1 <- grep("NAME_0|NAME_1|NAME_2", colnames(db1))
  db1 <- db1[, -c1]
  # pct of population with no access to WASH facilities
  db1 <- 100 - db1
  names(db1) <- paste0(names(db1), "_no_access")
  db1 <- data.frame(NAME_1 = nm1, NAME_2 = nm2, db1, stringsAsFactors = FALSE)
  
  outname <- file.path(dir, "output", iso, paste0(iso,"_wash_riskscore.csv"))
  write.csv(db1, outname, row.names = FALSE, fileEncoding = "latin1")
}

# input
countries <- c("Bangladesh", "Ethiopia", "Ghana", "Guatemala", 
               "Honduras", "Kenya", "Mali", "Nepal",
               "Niger", "Nigeria", "Senegal", "Uganda")

iso3 <- c("BGD", "ETH", "GHA", "GTM", "HND", "KEN", "MLI", "NPL", "NER", "NGA", "SEN", "UGA")
ciso <- data.frame(country = countries, iso = iso3, stringsAsFactors = FALSE)

ff <- list.files(zdir, pattern = ".CSV", full.names = TRUE)
dd <- lapply(ff, getDataAll, countries)

# https://web.archive.org/web/20131114060032/http://rwiki.sciviews.org/doku.php?id=tips%3adata-frames%3amerge
# doing sucessing merge to make sure the name matches --> takes a lot of time/resource
# dbase <- Reduce(function(dtf1, dtf2) merge(dtf1, dtf2, by = c("NAME_0", "NAME_2"), all.x = TRUE), dd)
# dbase <- reshape::merge_all(dd, by = c("NAME_0", "NAME_2"), all.x = TRUE)

# alternative, if all the elements have same number of rows, extremely unsafe
sapply(dd, dim)
dbase <- do.call(cbind, dd)

# save by country
lapply(1:nrow(ciso), splitDataISO, ciso, dbase, dir)

# now change the names that are messed due to encoding
iso <- "NER"
fname <- file.path(dir, "output", iso, paste0(iso,"_wash_riskscore.csv"))
d <- read.csv(fname, stringsAsFactors = FALSE)
d$NAME_1[d$NAME_1 == "TillabÃ©ry"] <- "Tillabéry"

d$NAME_2[d$NAME_2 == "AguiÃ©"] <- "Aguié"
d$NAME_2[d$NAME_2 == "FilinguÃ©"] <- "Filingué"
d$NAME_2[d$NAME_2 == "GourÃ©"] <- "Gouré"
d$NAME_2[d$NAME_2 == "MaÃ¯nÃ©-Soroa"] <- "Maïné-Soroa"
d$NAME_2[d$NAME_2 == "IllÃ©la"] <- "Illéla"
d$NAME_2[d$NAME_2 == "TÃ©ra"] <- "Téra"
d$NAME_2[d$NAME_2 == "TillabÃ©ry"] <- "Tillabéry"
write.csv(d, fname, row.names = FALSE, fileEncoding = "latin1")
