# count health facilities for 1000000 for each country

library(raster)

getHeathFacilityRiskScore <- function(i, ciso, dir, overwrite){
  country <- ciso[i,"country"]
  iso <- ciso[i,"iso"]
  
  cat("processing ", country, "\n")
  
  # directory setup
  datadir1 <- file.path(dir, "input", country)
  datadir2 <- file.path(dir,"processed",iso)
  outdir <- file.path(dir, "output", iso)
  dir.create(outdir, FALSE, TRUE)
  
  # list population data files
  rr <- list.files(datadir2, pattern = glob2rx(paste0(iso,"*_pop_agegroup_*.tif")), full.names = TRUE)
  
  # output file name
  outname <- file.path(outdir, paste0(iso,"_healthfacility_riskscore.csv"))
  
  if(!(file.exists(outname))|overwrite){
    # population age > 60
    rs <- grep("60_70|70_80|80_100", rr, value = TRUE)
    rs <- rast(rs)
    pop <- app(rs, sum, nodes = 4)
    
    # compute risk by district
    vsp <- raster::getData("GADM", country = iso, level = 2, path = datadir2)
    vsp <- vsp[,c("NAME_1","NAME_2")]
    v <- vect(vsp)
    
    # health sites for each district
    # if outside Africa, use Humdata
    if (iso %in% c("BGD","NPL","GTM","HND")){
      hf <- read.csv(file.path(datadir1, "healthsites", paste0(tolower(country), ".csv")),
                   stringsAsFactors = FALSE)
    
      # clean data and only keep the critical amenities
      hf <- hf[complete.cases(hf[,c("X","Y")]), c("X","Y","amenity")]
      hf <- hf[hf$amenity %in% c("clinic","doctors","hospital","pharmacy"),]
  
      # otherwise use data from 
      # https://www.nature.com/articles/s41597-019-0142-2#Sec7
      # url <- "https://www.who.int/malaria/areas/surveillance/who-cds-gmp-2019-01-eng.xlsx?ua=1"
    } else {
      ahf <- readxl::read_excel("data/who-cds-gmp-2019-01-eng.xlsx")
      hf <- data.frame(ahf[ahf$Country == country,])
      hf <- hf[complete.cases(hf[,c("Lat","Long")]), c("Lat","Long","Facility.type")]
      names(hf) <- c("Y", "X", "amenity")
    }
    
    # convert to spatial layer
    coordinates(hf) <- ~ X+Y
    crs(hf) <- crs(vsp)
    
    # health facility in each district
    agct <- over(hf, vsp)
    hfc <- dplyr::count(agct, NAME_1, NAME_2)
    names(hfc) <- c("NAME_1","NAME_2","health_facility_count")
    
    # total population for each district
    subpop <- extract(pop, v, fun = sum, na.rm = TRUE)
    
    # save result
    vsp$pop_ht60 <- subpop[,2]
    vsp <- merge(vsp, hfc, by = c("NAME_1","NAME_2"))
    vsp$health_facilitycount_100K_ht60 <- vsp$health_facility_count*100000/vsp$pop_ht60
    write.csv(vsp, outname, row.names = FALSE)
  }
}

# input
countries <- c("Bangladesh", "Ethiopia", "Ghana", "Guatemala", 
               "Honduras", "Kenya", "Mali", "Nepal",
               "Niger", "Nigeria", "Senegal", "Uganda")
iso3 <- c("BGD", "ETH", "GHA", "GTM", "HND", "KEN", "MLI", "NPL", "NER", "NGA", "SEN", "UGA")
ciso <- data.frame(country = countries, iso = iso3, stringsAsFactors = FALSE)

  
dir <- "C:/Users/anibi/Documents/work/covid_hotspot/data"

lapply(1:nrow(ciso), getHeathFacilityRiskScore, ciso, dir, overwrite = TRUE)
