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
  fd <- lapply(ff, read.csv, stringsAsFactors = FALSE, fileEncoding = "latin1")
  
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
  
  # delete_dsn doesn't work for geojson driver
  oname <- file.path(odir, paste0(iso, "_all_risk.geojson"))
  
  unlink(oname)
  st_write(vsf, oname)
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

# just one
# lapply(9, saveAllRisk, ciso, dir)

#############################################################################################
# merge geojson objects to create a single one
jj <- list.files(file.path(dir, "output/final"), pattern = ".geojson", full.names = TRUE)
# read all except all countries
jj <- grep("all_countries", jj, invert = TRUE, value = TRUE)

sj <- lapply(jj, st_read)
asj <- do.call(rbind, sj)
ofile <- file.path(dir, "output/final/all_countries_risk.geojson")
unlink(ofile)
st_write(asj, ofile)

# zip everything for share
zz <- list.files(file.path(dir, "output/final"), pattern = ".geojson", full.names = TRUE)
zname <- paste0("all_countries_geojson_", Sys.time(), ".zip")
zname <- gsub(":| ","_",zname)
zip(file.path(dir, "output/final", zname), zz, flags = " a -tzip",
    zip = "C:\\Program Files\\7-Zip\\7Z")



############################################################################################
# Data Analysis
library(igraph) # build network
library(spdep) # builds network
library(spatialreg)
library(raster)
library(tmap)

meanObeseRiskNeighbor <- function(iso, asj){
  cat("processing ", iso, "\n")
  asjs <- asj[asj$iso3==iso,]
  dds <- as.data.frame(asjs)
  cnd <- is.na(dds$obese30_35_risk)|is.na(dds$obese35_40_risk)|is.na(dds$obese40over_risk)|is.na(dds$n_total)
  vmiss <- sum(cnd)
  
  if(vmiss>0){
    v <- getData("GADM", country = iso, level = 2, path = file.path(dir, "processed", iso))
    # find the ones with missing value
    adms <- v$NAME_2[cnd]
    vd <- findNeighbor(v)
    orm <- lapply(adms, findNeighborMean, vd, k=3, dds)
    orm <- do.call(rbind, orm)
    asjs[cnd, grep("obese|n_total",colnames(asjs))] <- orm[,grep("obese|n_total",colnames(orm))]
    return(asjs)
  } else{
    return(asjs)
  }
}


findNeighborMean <- function(adm, vd, k=3, dds){
  # cat("processing ", adm)
  sa <- vd[,c("NAME_2", adm)]
  # find the closest neighbor
  nadms <- sa$NAME_2[sa@data[,2,drop=TRUE] < k]
  # colnames(dds)[colSums(is.na(dds)) > 0]
  or <- dds[dds$NAME_2 %in% nadms, grep("obese|n_total",colnames(dds))]
  or <- data.frame(NAME_2=adm, t(colMeans(or, na.rm=TRUE)))
  return(or)
}


# https://mikeyharper.uk/calculating-neighbouring-polygons-in-r/
findNeighbor <- function(v){
  nb_q <- poly2nb(v)
  
  # Sparse matrix
  nb_B <- nb2listw(nb_q, style="B", zero.policy=TRUE)
  B <- as(nb_B, "symmetricMatrix")
  
  # Calculate shortest distance
  g1 <- graph.adjacency(B, mode="undirected")
  sp_mat <- shortest.paths(g1)
  
  # Name used to identify data
  referenceCol <- v$NAME_2
  
  # Rename spatial matrix
  sp_mat2 <- as.data.frame(sp_mat)
  sp_mat2$id <- rownames(v@data)
  names(sp_mat2) <- paste0(referenceCol)
  # Add distance to shapefile data
  v@data <- cbind(v@data, sp_mat2)
  v@data$id <- rownames(v@data)
  return(v)
}


# fill missing values for obesity risk using mean values from neighbors
dir <- "C:/Users/anibi/Documents/work/covid_hotspot/data"
# # country
countries <- c("Bangladesh", "Ethiopia", "Ghana", "Guatemala",
               "Honduras", "Kenya", "Mali", "Nepal",
               "Niger", "Nigeria", "Senegal", "Uganda")
iso3 <- c("BGD", "ETH", "GHA", "GTM", "HND", "KEN", "MLI", "NPL", "NER", "NGA", "SEN", "UGA")

ofile <- file.path(dir, "output/final/all_countries_risk.geojson")
asj <- st_read(ofile)

vv <- lapply(iso3, meanObeseRiskNeighbor, asj)
vasj <- do.call(rbind, vv)

ofile1 <- file.path(dir, "output/final/all_countries_risk_obs_imputed.geojson")
unlink(ofile1)
st_write(vasj, ofile1)

# predict missing values for obesity risk using the population structure
# the model is trained on each country
# ciso <- data.frame(country = countries, iso = iso3, stringsAsFactors = FALSE)
# 
# ofile <- file.path(dir, "output/final/all_countries_risk.geojson")
# asj <- st_read(ofile)
# age <- read.csv(file.path(dir,"processed", "all_country_pop_stat.csv"),
#                 stringsAsFactors = FALSE)
# 
# bmi <- c("obese30_35_risk_wt", "obese35_40_risk_wt", "obese40over_risk_wt")
# 
# dd <- asj[,c("country","NAME_1","NAME_2",
#             "obese30_35_risk_wt", "obese35_40_risk_wt", "obese40over_risk_wt")]
# 
# dd <- merge(as.data.frame(dd), age, by.x = c("country","NAME_1","NAME_2"),
#             by.y = c("NAME_0","NAME_1","NAME_2"),
#             all.x=TRUE)
# 
# tp <- rowSums(dds[,grep("global",colnames(dds))], na.rm = TRUE)
# dds <- dd[dd$country=="Ethiopia",]
# dds <- data.frame(dds, total_population = tp)
# train <- dds[!is.na(dds$obese30_35_risk_wt),]
# test <- dds[is.na(dds$obese30_35_risk_wt),]
# 
# 
# lm(train$obese30_35_risk_wt ~ train$global_f_40_2020_1km/train$total_population)
