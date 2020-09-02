# wash score
# https://cloud.ihme.washington.edu/s/bkH2X2tFQMejMxy

# download the zipped csv statistics
# setup directories
dir <- "C:/Users/anibi/Documents/work/covid_hotspot/data"
indir <- file.path(dir, "input")
odir <- file.path(dir, "output")
datadir <- file.path(indir, "ihme")
zdir <- file.path(datadir, "Data [CSV]", "Admin 2")

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

splitDataISO <- function(i, ciso, dbase, odir){
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
  
  outname <- file.path(odir, iso, paste0(iso,"_wash_riskscore.csv"))
  write.csv(db1, outname, row.names = FALSE)
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
odir <- file.path(dir, "output")
lapply(1:nrow(ciso), splitDataISO, ciso, dbase, odir)
