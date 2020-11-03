library(raster)
library(terra)
# library(gridExtra)
library(dplyr)


#####################################################################################################################
# 
# computeRisk <- function(x){
#   ifelse(x < 30, 1, 
#          ifelse(x >= 30 & x < 35, 1.05, 
#                 ifelse(x >= 35 & x < 40, 1.40,
#                        ifelse(x >= 40, 1.92, 0)
#                 )
#          )
#   )
# }

getBMIRisk <- function(iso, dir){
  cat("processing", iso, "\n")
  # directory
  datadir <- file.path(dir, "processed", iso)
  outdir <- file.path(dir, "output" ,iso)
  dir.create(outdir, FALSE, TRUE)
  
  # get boudaries
  cc <- ccodes()
  v <- getData("GADM", country = iso, level = 2, path = datadir)
  
  # iso2 code
  iso2 <- cc$ISO2[cc$ISO3 == iso]
  iso2 <- ifelse(iso2=="GT", "GU", iso2)
  iso2 <- ifelse(iso2=="NE", "NI", iso2)
  
  # bmi based risk
  bmi <- read.csv("data/DHS_bmi_cluster_level.csv", stringsAsFactors = FALSE)
  
  bmis <- bmi[bmi$country_code == iso2,]
  bmis <- bmis[complete.cases(bmis[,c("latnum", "longnum")]), c("latnum", "longnum", "obese30_35","obese35_40","obese40over","n")]
  
  # breaks above if all lat lon are missing
  
  # multiply by HR for each age group
  bmis$obese30_35_risk <- bmis$obese30_35*1.05*bmis$n
  bmis$obese35_40_risk <- bmis$obese35_40*1.40*bmis$n
  bmis$obese40over_risk <- bmis$obese40over*1.92*bmis$n
  bmis$obese_norisk <- (1-(bmis$obese30_35+bmis$obese35_40+bmis$obese40over))*bmis$n
  
  # convert to spatial object
  coordinates(bmis) = ~  longnum + latnum
  crs(bmis) <- crs(v)
  
  # district name for each point
  nn <- over(bmis, v)
  bmis$NAME_1 <- nn$NAME_1
  bmis$NAME_2 <- nn$NAME_2
  
  # bmis <- bmis[complete.cases(bmis@data),]
  
  # # compute weighted risk
  # bmiag <- ddply(bmis@data, .(NAME_2),
  #                function(x) data.frame(obese30_35_wt=weighted.mean(x$obese30_35, x$n, na.rm = TRUE),
  #                                       obese35_40_wt=weighted.mean(x$obese35_40, x$n, na.rm = TRUE),
  #                                       obese40over_wt=weighted.mean(x$obese40over, x$n, na.rm = TRUE)))
  
  # compute sum across the admin
  
  bmiag <- bmis@data %>% group_by(NAME_1, NAME_2) %>%
              summarise(obese30_35_risk = sum(obese30_35_risk, na.rm = TRUE),
                        obese35_40_risk = sum(obese35_40_risk, na.rm = TRUE),
                        obese40over_risk = sum(obese40over_risk, na.rm = TRUE),
                        obese_norisk = sum(obese_norisk, na.rm = TRUE),
                        n_total = sum(n, na.rm=TRUE))

  
  # if any of the sample point is outside are outside
  bmiag <- bmiag[!is.na(bmiag$NAME_2), ]
  
  # total population women of child bearing age
  r <- file.path(datadir, paste0(iso,"_women_agegroup_15_45_1km.tif"))
  r <- rast(r)
  # vect class
  vct <- vect(v)
  
  # summary stat for each district
  popstat <- extract(r, vct, fun = sum, na.rm = TRUE)
  popstat <- data.frame(NAME_1 = v$NAME_1, NAME_2 = v$NAME_2, women_pop_15_45 = popstat[,2], stringsAsFactors = FALSE)
  
  # standardize by women population
  bmiag <- merge(popstat, bmiag, by = c("NAME_1","NAME_2"), all.x = TRUE)
  # bmiag$obese30_35_risk_wt <- bmiag$obese30_35_risk/bmiag$women_pop_15_45
  # bmiag$obese35_40_risk_wt <- bmiag$obese35_40_risk/bmiag$women_pop_15_45
  # bmiag$obese40over_risk_wt <- bmiag$obese40over_risk/bmiag$women_pop_15_45
  
  # save as csv 
  # v <- merge(v, bmiag, all.x = TRUE, by = "NAME_2")
  write.csv(bmiag, 
            file.path(outdir, paste0(iso, "_bmirisk_values.csv")), row.names = FALSE, fileEncoding = "latin1")
}


dir <- "C:/Users/anibi/Documents/work/covid_hotspot/data"
iso3 <- c("BGD", "ETH", "GHA", "GTM", "HND", "KEN", "MLI", "NPL", "NGA", "SEN", "UGA")

# 
# getBMIRisk(iso[5], indir, odir)
execFunction <- function (iso3, dir) {
  return(tryCatch(getBMIRisk(iso3, dir), error=function(e) NULL))
}

lapply(iso3, execFunction, dir)

lapply("ETH", execFunction, dir)
# library(future.apply)
# plan(multisession, workers = 4)
# future_lapply(iso, getBMIRisk, indir, odir)

##########################################################################################################
# Process NIGER seperately since the DHS data doesn't contain any lat lon information
# also the GADM names didn't match with the DHS names, but Jawoo found out a dataset that might work; same as GADM level 1

getBMIRiskbyADM <- function(iso, dir){
  cat("processing", iso, "\n")
  # directory
  datadir <- file.path(dir, "processed", iso)
  outdir <- file.path(dir, "output" ,iso)
  dir.create(outdir, FALSE, TRUE)
  
  # get boudaries
  cc <- ccodes()
  v <- getData("GADM", country = iso, level = 2, path = datadir)
  
  # iso2 code
  iso2 <- cc$ISO2[cc$ISO3 == iso]
  iso2 <- ifelse(iso2=="GT", "GU", iso2)
  iso2 <- ifelse(iso2=="NE", "NI", iso2)
  
  # bmi based risk
  bmi <- read.csv("data/DHS_bmi_cluster_level.csv", stringsAsFactors = FALSE)
  
  bmis <- bmi[bmi$country_code == iso2,]
  
  if(iso == "NER"){
    bmis <- bmis[complete.cases(bmis[,c("region")]), c("region", "obese30_35","obese35_40","obese40over","n")]
    bmis$region[bmis$region == "Tillaberi"] <- "Tillabéry" #GADM name matching
  }
  
  # breaks above if all lat lon are missing
  
  # multiply by HR for each age group
  bmis$obese30_35_risk <- bmis$obese30_35*1.05*bmis$n
  bmis$obese35_40_risk <- bmis$obese35_40*1.40*bmis$n
  bmis$obese40over_risk <- bmis$obese40over*1.92*bmis$n
  bmis$obese_norisk <- (1-(bmis$obese30_35+bmis$obese35_40+bmis$obese40over))*bmis$n
  
  # compute sum across the admin
  
  bmiag <- bmis %>% group_by(region) %>%
    summarise(obese30_35_risk = sum(obese30_35_risk, na.rm = TRUE),
              obese35_40_risk = sum(obese35_40_risk, na.rm = TRUE),
              obese40over_risk = sum(obese40over_risk, na.rm = TRUE),
              obese_norisk = sum(obese_norisk, na.rm = TRUE),
              n_total = sum(n, na.rm=TRUE))
  
  # total population women of child bearing age
  r <- file.path(datadir, paste0(iso,"_women_agegroup_15_45_1km.tif"))
  r <- rast(r)
  # vect class
  vct <- vect(v)
  
  # summary stat for each district
  popstat <- extract(r, vct, fun = sum, na.rm = TRUE)
  # terra operations, but saving the name from sp
  popstat <- data.frame(NAME_1 = v$NAME_1, NAME_2 = v$NAME_2, women_pop_15_45 = popstat[,2], stringsAsFactors = FALSE)
  
  # popstat <- data.frame(NAME_1 = v$NAME_1, NAME_2 = v$NAME_2, women_pop_15_45 = popstat, stringsAsFactors = FALSE)
  
  # standardize by women population
  # merge by admin 1 names, but divide by pop estimate to get admin 2 level risk
  bmiag <- merge(popstat, bmiag, by.x = "NAME_1", by.y = "region", all.x = TRUE)
  # bmiag$obese30_35_risk_wt <- bmiag$obese30_35_risk/bmiag$women_pop_15_45
  # bmiag$obese35_40_risk_wt <- bmiag$obese35_40_risk/bmiag$women_pop_15_45
  # bmiag$obese40over_risk_wt <- bmiag$obese40over_risk/bmiag$women_pop_15_45
  # 
  # save as csv 
  # v <- merge(v, bmiag, all.x = TRUE, by = "NAME_2")
  write.csv(bmiag, 
            file.path(outdir, paste0(iso, "_bmirisk_values.csv")), row.names = FALSE, fileEncoding = "latin1")
}


# process one country
dir <- "C:/Users/anibi/Documents/work/covid_hotspot/data"
getBMIRiskbyADM("NER", dir)


# # plot result
# outname <- file.path(outdir, paste0(iso, "_bmirisk_plot.png"))
# 
# pols <- list("sp.lines", as(v, 'SpatialLines'), lwd = 0.5, col = 'dimgray')
# ck <- list(space = 'bottom', labels = list(cex = 1), width = 1, height = 0.8)
# colsf1 <- colorRampPalette(c('yellow','orange','red','darkred'))
# cols1 <- colsf1(n=25)
# 
# colsf2 <- colorRampPalette(c('lightblue','yellow','red'))
# cols2 <- colsf2(n=10)
# 
# png(file = outname, height=6, width=8, units = "in", res=300)
# 
# p1 <- spplot(v, zcol = c("wtbmi"),
#              sp.layout = list(pols), col = 'transparent',
#              main = paste(iso, "_weighted_bmi"),
#              col.regions = cols1, 
#              colorkey = ck,
#              par.settings = list(axis.line = list(col = 'transparent'), 
#                                  strip.background = list(col = "transparent"),
#                                  strip.border = list(col = 'transparent')),
#              par.strip.text = list(cex = 1.25))
# 
# p2 <- spplot(v, zcol = c("bmirisk"),
#              sp.layout = list(pols), col = 'transparent',
#              main = paste(iso, "_bmi_risk"),
#              col.regions = cols2, 
#              colorkey = ck,
#              par.settings = list(axis.line = list(col = 'transparent'), 
#                                  strip.background = list(col = "transparent"),
#                                  strip.border = list(col = 'transparent')),
#              par.strip.text = list(cex = 1.25))
# 
# pp <- grid.arrange(p1, p2, ncol=2)
# print(pp)
# dev.off()

