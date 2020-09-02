library(terra)

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
  # which age group?
  ag <- risktable$cat[stringr::str_detect(key, risktable$cat)]
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
  
  # directory
  datadir <- file.path(dir, "processed", iso)
  outdir <- file.path(dir, "output" ,iso)
  dir.create(outdir, FALSE, TRUE)
  
  # list of input files
  rr <- list.files(datadir, pattern = glob2rx(paste0(iso,"*_pop_agegroup_*.tif")), full.names = TRUE)
  
  # output file name
  outname <- file.path(outdir, paste0(iso,"_pop_",gender,"_riskscore.csv"))
  
  if(!(file.exists(outname))|overwrite){
    # filter by gender
    rs <- grep(paste0("_",gender,"_"), rr, value = TRUE)
    
    # numerator of the weighted mean (sum(npop*w))
    rw <- lapply(rs, createHRScore, risktable)
    rw <- rast(rw)
    
    # compute risk by district
    vsp <- raster::getData("GADM", country = iso, level = 2, path = datadir)
    v <- vect(vsp)
    
    # not weighted risk
    frisknw <- app(rw, sum, nodes = 4, na.rm = TRUE)
    # summary stat for each district
    # non-weighted
    rskwtn <- extract(frisknw, v, fun = sum, na.rm = TRUE)
    
    # weighted risk
    # denominator for the weighted mean (sum(sex_pop)) => total population for the country population in that age group
    dr <- rast(rs)
    spop <- global(dr, fun = "sum", na.rm = TRUE)
    
    # divide each age group by their corresponding country total 
    frisk <- rw/as.numeric(spop$sum)
    
    # sum all risk
    frisk <- app(frisk, sum, nodes = 4, na.rm = TRUE)
    

    # summary stat for each district
    # weighted
    rskwt <- extract(frisk, v, fun = sum, na.rm = TRUE)
    
    # save result
    vs <- vsp[,c("NAME_1","NAME_2")]
    vs$poprisk_no_wt <- rskwtn[,2] 
    vs$poprisk_wt <- rskwt[,2]
    names(vs) <- c("NAME_1", "NAME_2", paste0(c("poprisk_no_wt_","poprisk_wt_"), gender))
    write.csv(vs@data, outname, row.names = FALSE)
  }
}

# input
# dir <- "/share/spatial02/users/anighosh/covid/worldpop/"
dir <- "C:/Users/anibi/Documents/work/covid_hotspot/data"

# country
countries <- c("Bangladesh", "Ethiopia", "Ghana", "Guatemala", 
               "Honduras", "Kenya", "Mali", "Nepal",
               "Niger", "Nigeria", "Senegal", "Uganda")
iso3 <- c("BGD", "ETH", "GHA", "GTM", "HND", "KEN", "MLI", "NPL", "NER", "NGA", "SEN", "UGA")
ciso <- data.frame(country = countries, iso = iso3, stringsAsFactors = FALSE)


# process pop risk by gender
lapply(1:nrow(ciso), getPopRiskScore, ciso, dir, "m", overwrite = TRUE)
lapply(1:nrow(ciso), getPopRiskScore, ciso, dir, "f", overwrite = TRUE)

