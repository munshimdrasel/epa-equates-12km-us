rm(list = ls())

library(fst)
library( data.table)
library( magrittr)
library( raster)
library( sf)
library( ggplot2)
require(tidyverse)
library(lubridate)
library(splitstackshape)
library(downloader)
library(fst)
library(stringi)
library(plyr)
library(tidycensus, quietly = TRUE)
library(tigris, quietly = TRUE)
library( fasterize)
library( USAboundaries)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

options(timeout=1200)
#on my laptop
# setwd ("/Users/munshirasel/Library/CloudStorage/GoogleDrive-munshimdrasel@gwmail.gwu.edu/My Drive/R/epa-equates-12km-us")

#on Hopper
# setwd ("/projects/HAQ_LAB/mrasel/R/epa-equates-12km-us")

data.2019 <- read.fst("./data/species.data.2019.fst")

glimpse(data.2019)

#separating dates in different columns
data.2019<- data.2019 %>% mutate(date = ymd(date)) %>% mutate_at(vars(date), funs(year, month, day))

unique(data.2019$month)

species_daily_to_monthly <- setDT(data.2019)[, .(O3_MDA8 = mean(O3_MDA8, na.rm=TRUE),
                                                 O3_AVG = mean(O3_AVG, na.rm=TRUE),
                                                 CO_AVG = mean(CO_AVG, na.rm=TRUE),
                                                 NO_AVG = mean(NO_AVG, na.rm=TRUE),
                                                 NO2_AVG = mean(NO2_AVG, na.rm=TRUE),
                                                 SO2_AVG = mean(SO2_AVG, na.rm=TRUE),
                                                 PM25_AVG= mean(PM25_AVG, na.rm=TRUE),
                                                 PM25_SO4_AVG=mean(PM25_SO4_AVG, na.rm=TRUE),
                                                 PM25_NO3_AVG=mean(PM25_NO3_AVG, na.rm=TRUE),
                                                 PM25_NH4_AVG=mean(PM25_NH4_AVG, na.rm=TRUE),
                                                 PM25_OC_AVG=mean(PM25_OC_AVG, na.rm=TRUE),
                                                 PM25_EC_AVG=mean(PM25_EC_AVG, na.rm=TRUE)),
                                             by = .(longitude, latitude, year, month)]




glimpse(species_daily_to_monthly)

species.melt <- melt(species_daily_to_monthly, 
                     measure.vars = c("O3_MDA8", "O3_AVG", "CO_AVG", "NO_AVG", "NO2_AVG", 
                                               "SO2_AVG", "PM25_AVG", "PM25_SO4_AVG", "PM25_NO3_AVG", "PM25_NH4_AVG",
                                      "PM25_OC_AVG", "PM25_EC_AVG"),
                     variable.name = "pollutants", value.name = "value")


# we want to use an equal area projection, here's one I often use:
p4s <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=40 +lon_0=-97 +a=6370000 +b=6370000"

ras_dom<-raster(xmn=-126, xmx=-66, ymn=23, ymx=51, resolution=c(0.1,0.1))

species.vec <- as.vector(unique(species.melt$pollutants))

years <- as.vector(unique(species_daily_to_monthly$year))

months <- as.vector(unique(species_daily_to_monthly$month))

us_states <- USAboundaries::us_states()  %>%  filter(!stusps %in% c("PR", "HI", "AK") )
states.vec <- as.vector(us_states$stusps)
states.name <- as.vector(us_states$name)

#loading demographic data 2013-2019 block group levels
#look into pm2.5-species-exposure-gridded-data project's so4.urban.R scipt for this data generation

#my laptop
# load ("/Users/munshirasel/Library/CloudStorage/GoogleDrive-munshimdrasel@gwmail.gwu.edu/My Drive/R/pm2.5-species-exposure-gridded-data/data/output/demography/combined.pop.income.block.groups.2013.2019.RData")

#on hopper demographic data 

load ("/projects/HAQ_LAB/mrasel/R/pm2.5-species-exposure-gridded-data/data/output/demography/combined.pop.income.block.groups.2013.2019.RData")


demo.combined <- combined.years %>%
  separate(NAME, c("block_group", "tract", "county", "state" ), ",")

spc.state.list <- list()
spc.species.state.list <- list()
spc.month.species.state.list <- list()
spc.year.month.species.state.list <- list()

for (i in 1:length(years)) {
  for (j in 1:length(months)) {
    for (k in 1:length(species)) {
    species <-  species.melt %>% filter(month %in% months[j] & year %in% years[i] & pollutants %in% species.vec[k])
    dt.raster <- rasterize(x=species[, 1:2], y=ras_dom,field=species[, 6], fun = mean)
    df.sf <- rasterToPolygons( dt.raster ) %>%st_as_sf()
    
    df.sf.transform <- df.sf %>% st_transform(crs = p4s)
    
    for (l in 1:length(states.vec)) {
      block.groups <- block_groups(states.vec[l], year=years[i])
      
      block.groups.transform <- block.groups %>% st_transform(crs=p4s)
      bg.selected <- block.groups.transform %>% dplyr::select( GEOID, geometry)
      
      conc.bg <- st_interpolate_aw( df.sf.transform["layer"], bg.selected,   extensive = F)
      vec <- as.vector(intersect(bg.selected$geometry, conc.bg$geometry))
      bg.new <- bg.selected %>%  filter(geometry %in% vec)
      conc.bg$species <- species.vec[k]
      conc.bg$month <- months[j]
      conc.bg$year <- years[i]
      conc.bg$state <- states.vec[l]
      conc<- cbind (bg.new,conc.bg, by="geometry")%>% dplyr::select(-geometry.1)
      # ggplot( conc) + geom_sf( aes( fill = layer,  geometry = geometry),
      #                        color = NA) +scale_fill_viridis_c( option="plasma")

      spc.state.list[[l]] <- conc
      
    }  
    
    spc.data = do.call(rbind, spc.state.list) #all states data
    

    # combined.demo <-  do.call (rbind, df.list) 
    
 
    
    # combined.demo.all <- combined.demo #%>% filter (state==" Texas")
    
    #intersect(combined.years$GEOID, combined.demo$GEOID)
    demo_year <- demo.combined %>% filter (year %in% years[i] )
    geoid <- as.vector(unique( spc.data$GEOID))
    pop.inc.geoid <- as.vector(unique(demo_year$GEOID))
    
    common.geoid <- intersect(geoid,pop.inc.geoid)
    
    demo.data <- demo_year%>% filter(GEOID %in% common.geoid)
    spc.data <-as.data.frame(spc.data %>%  filter(GEOID %in% common.geoid) %>% dplyr::select(-"geometry", -"by"))
    
    names(demo.data)[names(demo.data) == 'state'] <- 'state.name'
    
    #merging demographic and species data
    spc.pop.inc <- st_as_sf(merge(as.data.frame(demo.data), as.data.frame(spc.data), by =c ("GEOID", "year"))) %>% 
      dplyr::select(-geometry.y)
    
    spc.species.state.list[[k]] <-    spc.pop.inc 
    
    
  }
  spc.data = do.call(rbind, spc.species.state.list) #all states all species all demographic
  spc.month.species.state.list[[j]] <-     spc.data
  
}

spc.data = do.call(rbind, spc.species.state.list) #all states all species all demographic all months
spc.month.species.state.list[[i]] <-     spc.data
save(spc.month.species.state.list, file=paste0("./data/",years[i], "_equates_species_blk_grp_demo.RData"))

}



# 
# for (j in 1:length(states.vec)) {
#   block.groups <- block_groups(states.vec[j], year=years[i])
#   
#   block.groups.transform <- block.groups %>% st_transform(crs=p4s)
#   bg.selected <- block.groups.transform %>% dplyr::select( GEOID, geometry)
#   
#   conc.bg <- st_interpolate_aw( df.sf.transform["layer"], block.groups.transform,   extensive = F)
#   vec <- as.vector(intersect(bg.selected$geometry, conc.bg$geometry))
#   bg.new <- bg.selected %>%  filter(geometry %in% vec)
#   conc.bg$species <- sub("^([^-]+-){3}([^-]+).*", "\\2", rds_files_non_urban[i]) #FOR urban change 3 to 2
#   conc.bg$year <- as.numeric(sub("^([^-]+-){4}([^-]+).*", "\\2", rds_files_non_urban[i]))
#   conc.bg$state <- states.vec[j]
#   conc<- cbind (bg.new,conc.bg, by="geometry")%>% dplyr::select(-geometry.1)
#   # ggplot( conc) + geom_sf( aes( fill = layer,  geometry = geometry),
#   #                        color = NA) +scale_fill_viridis_c( option="plasma")
#   # 
#   # cbind(bg.selected, conc.bg)
#   df.list[[j]] <- conc
#   
# }  
# save(df.list, file=paste0("./data/",selected.species, "-", sub("^([^-]+-){4}([^-]+).*", "\\2", rds_files_non_urban[i]),".RData"))
# 
# 
# 
# 
# inv_distancer <-
#   function( n,
#             source.sf,
#             receptor.sf,
#             minimum_dist = 2000){
#     
#     # calculate distances in m
#     site_dist <-
#       as.vector(
#         st_distance( source.sf[n,],
#                      receptor.sf)) #/ 1e3
#     
#     # minimum distance should be half resolution of grid
#     site_dist[site_dist < minimum_dist] <- minimum_dist
#     
#     # output datset
#     out <- data.table( ID = source.sf$ID[n],
#                        ID_recept = receptor.sf$ID_recept,
#                        inv_dist = 1 / site_dist,
#                        Type=source.sf$Type[n])
#     return( out)
#   }
# 
# 
# exp_inverse_dist <-
#   pbmcapply::pbmclapply(
#     1:nrow( link_locations.sf.trans),
#     inv_distancer,
#     source.sf = link_locations.sf.trans,
#     receptor.sf = fishnet.sf) %>%
#   rbindlist
# 




