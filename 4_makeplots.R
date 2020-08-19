library(raster)
library(RColorBrewer)
library(officer)

makePlotISO <- function(i, ciso, dir){
  country <- ciso[i,"country"]
  iso <- ciso[i,"iso"]
  
  outname <- file.path(dir, paste0(iso, "_poprisk_plot.png"))
  
  ff <- list.files(dir, pattern = ".rds", full.names = TRUE)
  f <- grep(iso, ff, value = TRUE)
  
  frisk <- readRDS(grep("_f_", f, value = TRUE))
  mrisk <- readRDS(grep("_m_", f, value = TRUE))
  
  names(frisk)[[3]] <- "female_risk"
  
  # combine both risks
  v <- frisk
  v$male_risk <- mrisk$poprisk
  
  pols <- list("sp.lines", as(v, 'SpatialLines'), lwd = 0.5, col = 'dimgray')
  
  ck <- list(space = 'bottom', labels = list(cex = 1), width = 1.5, height = 0.5)
  
  colsf <- colorRampPalette(c('blue', 'green', 'yellow', 'red'))
  cols <- colsf(n=nrow(v))
  
  png(file = outname, height=6, width=8, units = "in", res=300)
  
  toplot <- c("female_risk", "male_risk")
  
  pp <- spplot(v, zcol = toplot, 
         layout = c(2, 1), as.table = TRUE,
         names.attr = toplot,
         sp.layout = list(pols), col = 'transparent',
         main = paste(country, "covid risk based on age"),
         col.regions = cols, 
         colorkey = ck,
         par.settings = list(axis.line = list(col = 'transparent'), 
                             strip.background = list(col = "transparent"),
                             strip.border = list(col = 'transparent')),
         par.strip.text = list(cex = 1.25),
         # # add padding between plots
         xlim = c(v@bbox["x", "min"] - 0.1,
                  v@bbox["x", "max"] + 0.1),
         ylim = c(v@bbox["y", "min"] - 0.1,
                  v@bbox["y", "max"] + 0.1))
  print(pp)
  dev.off()
}

# Load and prepare data
dir <- "C:\\Users\\anibi\\Documents\\work\\covid_hotspot\\poprisk"

countries <- c("Bangladesh", "Ethiopia", "Ghana", "Guatemala", 
               "Honduras", "Kenya", "Mali", "Nepal",
               "Niger", "Nigeria", "Senegal", "Uganda")
iso3 <- c("BGD", "ETH", "GHA", "GTM", "HND", "KEN", "MLI", "NPL", "NER", "NGA", "SEN", "UGA")
ciso <- data.frame(country = countries, iso = iso3, stringsAsFactors = FALSE)

# makePlotISO(2, ciso, dir)
lapply(1:nrow(ciso), makePlotISO, ciso, dir)


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