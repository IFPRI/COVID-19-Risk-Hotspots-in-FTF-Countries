library(raster)
library(gridExtra)
library(plyr)


#####################################################################################################################

computeRisk <- function(x){
  ifelse(x < 30, 1, 
         ifelse(x >= 30 & x < 35, 1.05, 
                ifelse(x >= 35 & x < 40, 1.40,
                       ifelse(x >= 40, 1.92, 0)
                )
         )
  )
}

getBMIRisk <- function(iso3, indir, odir){
  cat("processing", iso3, "\n")
  # directory
  datadir <- file.path(indir, iso3)
  outdir <- file.path(odir, iso3)
  dir.create(outdir, FALSE, TRUE)
  
  # get boudaries
  cc <- ccodes()
  v <- getData("GADM", country = iso3, level = 2, path = datadir)
  
  # iso2 code
  iso2 <- cc$ISO2[cc$ISO3 == iso3]
  iso2 <- ifelse(iso2=="GT", "GU", iso2)
  iso2 <- ifelse(iso2=="NE", "NI", iso2)
  
  # bmi based risk
  bmi <- read.csv("data/DHS_bmi_cluster_level.csv", stringsAsFactors = FALSE)
  
  bmis <- bmi[bmi$country_code == iso2,]
  bmis <- bmis[complete.cases(bmis[,c("latnum", "longnum", "wom_bmi")]), c("region","latnum", "longnum", "wom_bmi","n")]
  
  # convert to spatial object
  coordinates(bmis) = ~  longnum + latnum
  crs(bmis) <- crs(v)
  
  # district name for each point
  bmis$NAME_2 <- over(bmis, v)$NAME_2
  bmis <- bmis[complete.cases(bmis@data),]
  
  # compute weighted risk
  bmiag <- ddply(bmis@data, .(NAME_2),   # so by asset class invoke following function
                 function(x) data.frame(wtbmi=weighted.mean(x$wom_bmi, x$n, na.rm = TRUE)))
  
  bmiag <- bmiag[!is.na(bmiag$NAME_2), ]
  
  # make sure that the estimate is correct
  # x <- bmis[bmis$district == "xxxxx",]
  # x <- x@data
  # sum(x$wom_bmi*x$n)/sum(x$n)

  bmiag$bmirisk <- computeRisk(bmiag$wtbmi)
  v <- merge(v, bmiag, all.x = TRUE, by = "NAME_2")
  write.csv(v@data[,c("NAME_1","NAME_2","wtbmi","bmirisk")], 
            file.path(outdir, paste0(iso3, "_bmirisk_values.csv")), row.names = FALSE)
  
  # plot result
  outname <- file.path(outdir, paste0(iso3, "_bmirisk_plot.png"))
  
  pols <- list("sp.lines", as(v, 'SpatialLines'), lwd = 0.5, col = 'dimgray')
  ck <- list(space = 'bottom', labels = list(cex = 1), width = 1, height = 0.8)
  colsf1 <- colorRampPalette(c('yellow','orange','red','darkred'))
  cols1 <- colsf1(n=25)
  
  colsf2 <- colorRampPalette(c('lightblue','yellow','red'))
  cols2 <- colsf2(n=10)
  
  png(file = outname, height=6, width=8, units = "in", res=300)
  
  p1 <- spplot(v, zcol = c("wtbmi"),
               sp.layout = list(pols), col = 'transparent',
               main = paste(iso3, "_weighted_bmi"),
               col.regions = cols1, 
               colorkey = ck,
               par.settings = list(axis.line = list(col = 'transparent'), 
                                   strip.background = list(col = "transparent"),
                                   strip.border = list(col = 'transparent')),
               par.strip.text = list(cex = 1.25))
  
  p2 <- spplot(v, zcol = c("bmirisk"),
               sp.layout = list(pols), col = 'transparent',
               main = paste(iso3, "_bmi_risk"),
               col.regions = cols2, 
               colorkey = ck,
               par.settings = list(axis.line = list(col = 'transparent'), 
                                   strip.background = list(col = "transparent"),
                                   strip.border = list(col = 'transparent')),
               par.strip.text = list(cex = 1.25))
  
  pp <- grid.arrange(p1, p2, ncol=2)
  print(pp)
  dev.off()
}


indir <- "C:/Users/anibi/Documents/work/covid_hotspot/data/processed"
odir <- "C:/Users/anibi/Documents/work/covid_hotspot/data/output"
iso <- c("BGD", "ETH", "GHA", "GTM", "HND", "KEN", "MLI", "NPL", "NER", "NGA", "SEN", "UGA")

# 
# getBMIRisk(iso[5], indir, odir)
# lapply(iso, getBMIRisk, indir, odir)

# library(future.apply)
plan(multisession, workers = 4)
future_lapply(iso, getBMIRisk, indir, odir)

# 
# pols <- list("sp.lines", as(v, 'SpatialLines'), lwd = 0.5, col = 'dimgray')
# ck <- list(space = 'bottom', labels = list(cex = 1), width = 1.5, height = 0.5)
# 
# cols <- rev(c('#d73027', '#fc8d59', '#fee090', '#e0f3f8'))

# # png(file = "bmirisk.png", height=6, width=6, units = "in", res=300)
# 
# spplot(v, zcol = c("bmi", "bmirisk"), col.regions=cols, main="covid_risk_bmi", 
#        colorkey = FALSE, lwd=1, col="black")
# 
# # dev.off()

