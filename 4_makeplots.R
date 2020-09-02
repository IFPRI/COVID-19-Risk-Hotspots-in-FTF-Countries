library(raster)
library(RColorBrewer)
library(gridExtra)

makePlotISOPopRisk <- function(i, ciso, dir){
  country <- ciso[i,"country"]
  iso <- ciso[i,"iso"]
  
  outdir <- file.path(dir, "plots")
  dir.create(outdir, FALSE, TRUE)
  
  outname <- file.path(outdir, paste0(iso, "_poprisk_plot.png"))
  
  ff <- list.files(dir, pattern = "_riskscore.rds", full.names = TRUE)
  f <- grep(iso, ff, value = TRUE)
  
  frisk <- readRDS(grep("_f_", f, value = TRUE))
  mrisk <- readRDS(grep("_m_", f, value = TRUE))
  
  # names(frisk)[[3]] <- "female_risk"
  
  # combine both risks
  # v <- frisk
  # v$male_risk <- mrisk$poprisk
  
  pols <- list("sp.lines", as(frisk, 'SpatialLines'), lwd = 0.5, col = 'dimgray')
  
  ck <- list(space = 'bottom', labels = list(cex = 1), width = 1.5, height = 0.5)
  
  colsf <- colorRampPalette(c('yellow','orange','red','darkred'))
  cols <- colsf(n=25)
  
  png(file = outname, height=6, width=8, units = "in", res=300)
  
  p1 <- spplot(frisk, zcol = "poprisk", 
               sp.layout = list(pols), col = 'transparent',
               main = paste(country, "_female_risk_age"),
               col.regions = cols, 
               colorkey = ck,
               par.settings = list(axis.line = list(col = 'transparent'), 
                                   strip.background = list(col = "transparent"),
                                   strip.border = list(col = 'transparent')),
               par.strip.text = list(cex = 1.25),
               # # add padding between plots
               xlim = c(frisk@bbox["x", "min"] - 0.1,
                        frisk@bbox["x", "max"] + 0.1),
               ylim = c(frisk@bbox["y", "min"] - 0.1,
                        frisk@bbox["y", "max"] + 0.1))
  
  
  p2 <- spplot(mrisk, zcol = "poprisk", 
               sp.layout = list(pols), col = 'transparent',
               main = paste(country, "_male_risk_age"),
               col.regions = cols, 
               colorkey = ck,
               par.settings = list(axis.line = list(col = 'transparent'), 
                                   strip.background = list(col = "transparent"),
                                   strip.border = list(col = 'transparent')),
               par.strip.text = list(cex = 1.25),
               # # add padding between plots
               xlim = c(frisk@bbox["x", "min"] - 0.1,
                        frisk@bbox["x", "max"] + 0.1),
               ylim = c(frisk@bbox["y", "min"] - 0.1,
                        frisk@bbox["y", "max"] + 0.1))
  
  pp <- grid.arrange(p1, p2, ncol=2)
  print(pp)
  dev.off()
}


makePlotISOfacilityRisk <- function(i, ciso, dir){
  country <- ciso[i,"country"]
  iso <- ciso[i,"iso"]
  
  outdir <- file.path(dir, "plots")
  dir.create(outdir, FALSE, TRUE)
  
  outname <- file.path(outdir, paste0(iso, "_healthfacility_risk_plot.png"))
  
  ff <- list.files(dir, pattern = "_healthfacility_riskscore.rds", full.names = TRUE)
  f <- grep(iso, ff, value = TRUE)
  
  hrisk <- readRDS(f)
  
  # plotting
  pols <- list("sp.lines", as(frisk, 'SpatialLines'), lwd = 0.5, col = 'dimgray')
  
  ck <- list(space = 'bottom', labels = list(cex = 1), width = 1.5, height = 0.5)
  
  colsf <- colorRampPalette(c('green','yellow','orange','red','darkred'))
  cols <- rev(colsf(n=50))
  
  png(file = outname, height=6, width=8, units = "in", res=300)
  
  p1 <- spplot(hrisk, zcol = "hfacilityrisk", 
               sp.layout = list(pols), col = 'transparent',
               main = paste0(country, "_health_facility/100000"),
               col.regions = cols, 
               colorkey = ck,
               par.settings = list(axis.line = list(col = 'transparent'), 
                                   strip.background = list(col = "transparent"),
                                   strip.border = list(col = 'transparent')),
               par.strip.text = list(cex = 1.25),
               # add padding between plots
               xlim = c(hrisk@bbox["x", "min"] - 0.1,
                        hrisk@bbox["x", "max"] + 0.1),
               ylim = c(hrisk@bbox["y", "min"] - 0.1,
                        hrisk@bbox["y", "max"] + 0.1))
  
  print(p1)
  dev.off()
}


# Load and prepare data
# dir <- "C:\\Users\\anibi\\Documents\\work\\covid_hotspot\\poprisk"
dir <- "/share/spatial02/users/anighosh/covid/riskscores"

countries <- c("Bangladesh", "Ethiopia", "Ghana", "Guatemala", 
               "Honduras", "Kenya", "Mali", "Nepal",
               "Niger", "Nigeria", "Senegal", "Uganda")
iso3 <- c("BGD", "ETH", "GHA", "GTM", "HND", "KEN", "MLI", "NPL", "NER", "NGA", "SEN", "UGA")
ciso <- data.frame(country = countries, iso = iso3, stringsAsFactors = FALSE)

# makePlotISO(2, ciso, dir)
lapply(1:nrow(ciso), makePlotISOPopRisk, ciso, dir)

lapply(1:nrow(ciso), makePlotISOfacilityRisk, ciso, dir)


# save result as powerpoints
# library(magrittr)
# 
# pp <- list.files(dir, ".png", full.names = TRUE)
# pptpath <- file.path(dir, "covid_risk_age.pptx")
# out <- read_pptx()
# 
# out %>%
#   add_slide(layout = "Blank", master = "Office Theme") %>%
#   ph_with(external_img(img, width = w, height = h),
#           location = ph_location(left = 0.2, top = 0.6),
#           use_loc_size = FALSE) %>%
#   print(target = pptpath)