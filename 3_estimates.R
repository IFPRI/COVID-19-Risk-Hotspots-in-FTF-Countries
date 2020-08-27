library(terra)
library(plyr)
library(sf)

# combine worldpop agegroup using the nature paper
low <- c(18,40,50,60,70,80)
high <- c(40,50,60,70,80,100)
agroup <- paste0(low,"_",high)

risktable <- data.frame(property = c(rep("age", 6), rep("sex", 2), rep("BMI", 4), rep("diabetes", 2)),
                        cat =  c(agroup, "female", "male", 
                                 "not_obese", "obsese_class_I", "obese_class_II", "obsese_class_III",
                                 "HbA1c<58", "HbA1c>58"),
                        hr = c(0.06, 0.3, 1, 2.4, 6.08, 20.61, 1, 1.59, 1, 1.05, 1.4, 1.92, 1.31, 1.95),
                        stringsAsFactors = FALSE)

# age weighted risk table
createHRScore <- function(f, risktable){
  key <- basename(f)
  
  # get gender risk score
  if(grepl("_m_", key)){
    g <- "male"
    w <- risktable$hr[risktable$cat == g]
  } else {
    g <- "female"
    w <- risktable$hr[risktable$cat == g]
  }
  
  # get age risk score
  ag <- gsub('pop|_m_|_f_|_1km.tif', "" ,key)
  agw <- risktable$hr[risktable$cat == ag]
  # composite risk score
  cw <- agw*w
  # multiply with indicator
  r <- rast(f)
  return(r*cw)
}


getPopRiskScore <- function(i, ciso, dir, gender, overwrite){
  country <- ciso[i,"country"]
  iso <- ciso[i,"iso"]
  
  cat("processing ", country, "\n")
  
  datadir <- file.path(dir, country, "processed")
  rr <- list.files(datadir, pattern = "_1km.tif$", full.names = TRUE)
  
  # output file name
  outdir <- file.path(dir, "riskscores")
  dir.create(outdir, FALSE, TRUE)
  outname <- file.path(outdir, paste0(iso,"_pop_",gender,"_riskscore.rds"))
  
  if(!(file.exists(outname))|overwrite){
    # filter by gender
    rs <- grep(paste0("_",gender,"_"), rr, value = TRUE)
    # numerator of the weighted mean (sum(npop*w))
    rw <- lapply(rs, createHRScore, risktable)
    rw <- rast(rw)
    # sum all risk
    nr <- app(rw, sum, nodes = 4, na.rm = TRUE)
    
    # denominator of the weighted mean (sum(sex_pop))
    dr <- rast(rs)
    dr <- app(dr, sum, nodes = 4, na.rm = TRUE)
    
    # final weighted mean risk for age
    rskag <- nr/dr
    
    # compute risk by district
    vsp <- raster::getData("GADM", country = iso, level = 2, path = file.path(dir, country))
    v <- vect(vsp)
    
    # summary stat for each district
    vrskag <- extract(rskag, v, fun = mean, na.rm = TRUE)
    # save result
    vsp <- vsp[,c("NAME_1", "NAME_2")]
    vsp$poprisk <- vrskag[,2]
    saveRDS(vsp, outname)
  }
  # spplot(vsp,"poprisk", col.regions=rev(heat.colors(25)), main="covid_risk_population")
}

##################################################################################################

getHeathFacilityRiskScore <- function(i, ciso, dir, overwrite){
  country <- ciso[i,"country"]
  iso <- ciso[i,"iso"]
  
  cat("processing ", country, "\n")
  
  datadir <- file.path(dir, country, "processed")
  rr <- list.files(datadir, pattern = "_1km.tif$", full.names = TRUE)
  
  # output file name
  outdir <- file.path(dir, "riskscores")
  dir.create(outdir, FALSE, TRUE)
  outname <- file.path(outdir, paste0(iso,"_healthfacility_riskscore.rds"))
  
  if(!(file.exists(outname))|overwrite){
    # population age > 60
    rs <- grep("60_70|70_80|80_100", rr, value = TRUE)
    rs <- rast(rs)
    pop <- app(rs, sum, nodes = 4)
    
    # compute risk by district
    vsp <- raster::getData("GADM", country = iso, level = 2, path = file.path(dir, country))
    vsp <- vsp[,"NAME_2"]
    v <- vect(vsp)
    
    # health sites for each district
    hf <- read.csv(file.path(dir, country, "healthsites", paste0(tolower(country), ".csv")),
                   stringsAsFactors = FALSE)
    
    # clean data and only keep the critical amenities
    hf <- hf[complete.cases(hf[,c("X","Y")]), c("X","Y","amenity")]
    hf <- hf[hf$amenity %in% c("clinic","doctors","hospital","pharmacy"),]
    
    # convert to spatial layer
    coordinates(hf) <- ~ X+Y
    crs(hf) <- crs(vsp)
    
    # health facility in each district
    ct <- table(over(hf, vsp))
    hfc <- data.frame(NAME_2 = as.character(names(ct)), ct = as.numeric(ct), stringsAsFactors = FALSE)
    
    # total population for each district
    subpop <- extract(pop, v, fun = sum, na.rm = TRUE)
    
    # save result
    vsp$popht60 <- subpop[,2]
    vsp <- merge(vsp, hfc, by = "NAME_2")
    vsp$hfacilityrisk <- vsp$ct*100000/vsp$popht60
    saveRDS(vsp, outname)
  }
  # spplot(vsp,"poprisk", col.regions=rev(heat.colors(25)), main="covid_risk_population")
}




# input
dir <- "/share/spatial02/users/anighosh/covid"

# country
countries <- c("Bangladesh", "Ethiopia", "Ghana", "Guatemala", 
               "Honduras", "Kenya", "Mali", "Nepal",
               "Niger", "Nigeria", "Senegal", "Uganda")
iso3 <- c("BGD", "ETH", "GHA", "GTM", "HND", "KEN", "MLI", "NPL", "NER", "NGA", "SEN", "UGA")
ciso <- data.frame(country = countries, iso = iso3, stringsAsFactors = FALSE)


# process pop risk by gender
lapply(1:nrow(ciso), getPopRiskScore, ciso, dir, "m", overwrite = TRUE)
lapply(1:nrow(ciso), getPopRiskScore, ciso, dir, "f", overwrite = TRUE)

# health facility risk score
lapply(1:nrow(ciso), getHeathFacilityRiskScore, ciso, dir, overwrite = FALSE)


#####################################################################################################################
# bmi based risk
bmi <- read.csv("data/DHS_bmi_cluster_level.csv", stringsAsFactors = FALSE)
bmi <- bmi[bmi$country_code == "BD",]
bmis <- bmi[complete.cases(bmi[,c("latnum", "longnum", "wom_bmi")]), c("region","latnum", "longnum", "wom_bmi","n")]

# convert to spatial object
coordinates(bmis) = ~  longnum + latnum
crs(bmis) <- crs(v)
# plot(bmi, add = T)
# plot(v)

# district name for each point
bmis$district <- over(bmis, v)$NAME_2
bmis <- bmis[complete.cases(bmis@data),]

# compute weighted risk
bmiag <- ddply(bmis@data, .(district),   # so by asset class invoke following function
               function(x) data.frame(wtbmi=weighted.mean(x$wom_bmi, x$n)))

bmiag <- bmiag[!is.na(bmiag$district), ]

# make sure that the estimate is correct
x <- bmis[bmis$district == "Barguna",]
x <- x@data
sum(x$wom_bmi*x$n)/sum(x$n)

computeRisk <- function(x){
  ifelse(x < 30, 1, 
         ifelse(x >= 30 & x < 35, 1.05, 
                ifelse(x >= 35 & x < 40, 1.40,
                       ifelse(x >= 40, 1.92, 0)
                )
         )
  )
}




# v$meanrisk <- computeRisk(vmean$wom_bmi)
# v$maxrisk <- computeRisk(vmax$wom_bmi)
# 
# v$medianrisk <- computeRisk(vmedian$wom_bmi)
# 
# pols <- list("sp.lines", as(v, 'SpatialLines'), lwd = 0.5, col = 'dimgray')
# 
# ck <- list(space = 'bottom', labels = list(cex = 1), width = 1.5, height = 0.5)
# 
# cols <- rev(c('#d73027', '#fc8d59', '#fee090', '#e0f3f8'))
# 
# png(file = "data/bmi_median.png", height=6, width=6, units = "in", res=300)
# 
# spplot(v,"medianrisk", col.regions=cols, main="median_covid_risk_bmi", 
#        colorkey = FALSE, lwd=1, col="black")
# 
# dev.off()

